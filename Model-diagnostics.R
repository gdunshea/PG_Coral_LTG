## Model diagnostics
# ============================================================
# DEBUGGED Diagnostics for brms gamma(log) model: final_main
# - Robust logging, safe fallbacks, single PDF output
# - posterior >=1.6.x: uses ess_* (no neff_ratio)
# - Manual Pearson residuals for Gamma(mean=mu, shape=k)
# - Robust LOO-PIT (psis_object or weights fallback)
# ============================================================

suppressPackageStartupMessages({
  library(brms)
  library(bayesplot)
  library(loo)
  library(posterior)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

# -------------------- DEBUG HELPERS -------------------------
DEBUG <- TRUE
dbg <- function(..., .sep = "") if (DEBUG) cat("[DEBUG] ", paste0(..., collapse = .sep), "\n")

run_step <- function(step, code, .stop_on_error = TRUE) {
  cat(sprintf("\n=== %s ===\n", step))
  withCallingHandlers(
    {
      out <- tryCatch(
        {
          val <- force(code)
          cat(sprintf("[OK] %s\n", step))
          invisible(val)
        },
        error = function(e) {
          cat(sprintf("[FAIL] %s: %s\n", step, conditionMessage(e)))
          if (.stop_on_error) stop(e) else return(invisible(NULL))
        }
      )
      out
    },
    warning = function(w) {
      cat(sprintf("[WARN] %s: %s\n", step, conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
}

# -------------------- PRECHECKS -----------------------------
run_step("Precheck: final_main is brmsfit", {
  stopifnot(inherits(final_main, "brmsfit"))
})

fit <- final_main
dat <- fit$data

# Versions (one-by-one to avoid vectorized namespace issues)
run_step("Report package versions", {
  pkgs <- c("brms","bayesplot","loo","posterior","ggplot2","dplyr","tidyr")
  vers <- vapply(pkgs, function(p) {
    v <- try(utils::packageVersion(p), silent = TRUE)
    if (inherits(v, "try-error")) "unknown" else as.character(v)
  }, FUN.VALUE = character(1))
  dbg("Packages: ", paste(sprintf("%s-%s", pkgs, vers), collapse = ", "))
})

# ---- Response-name extraction with fallbacks ----
get_resp_name <- function(fit) {
  dbg("Detecting response via brms::formula()$forms$mu ...")
  fr <- try(brms::formula(fit), silent = TRUE)
  if (!inherits(fr, "try-error")) {
    mu_form <- try(fr$forms$mu, silent = TRUE)
    if (!inherits(mu_form, "try-error") && inherits(mu_form, "formula")) {
      v <- all.vars(mu_form)
      if (length(v) >= 1) {
        dbg("Response via mu-formula: ", v[1])
        return(v[1])
      }
    }
  }
  dbg("Fallback to model.frame() ...")
  mf <- try(model.frame(fit), silent = TRUE)
  if (!inherits(mf, "try-error")) {
    dbg("Response via model.frame(): ", names(mf)[1])
    return(names(mf)[1])
  }
  dbg("Last resort: first column of fit$data")
  names(fit$data)[1]
}

resp_var <- run_step("Detect response variable", { get_resp_name(fit) })
y <- run_step("Extract response vector", { dat[[resp_var]] })
if (is.null(y)) stop("Couldn't locate response variable in data (resp_var = ", resp_var, ").")
dbg("Response variable: ", resp_var, " | N=", length(y))

# -------------------- OPEN/CLOSE PDF SAFELY -----------------
pdf_file <- "diagnostics_final_main.pdf"
device_open <- FALSE
run_step(paste0("Open PDF: ", pdf_file), {
  pdf(pdf_file, width = 7.0, height = 6.0)
  device_open <<- TRUE
  color_scheme_set("brightblue")
})
.on_exit <- function() {
  if (isTRUE(device_open)) {
    try(dev.off(), silent = TRUE)
    device_open <<- FALSE
  }
}
reg.finalizer(environment(), function(e) .on_exit(), onexit = TRUE)

# ============================================================
# (1) MCMC / HMC diagnostics
# ============================================================
run_step("MCMC summary", {
  summ <- summary(fit)
  print(summ$fixed)
  if (!is.null(summ$random)) print(summ$random)
  if (!is.null(summ$spec_pars)) print(summ$spec_pars)
})

# Coerce draws safely for Rhat/ESS
safe_draws_for_diagnostics <- function(fit) {
  dbg("Trying posterior::as_draws_array(fit) ...")
  da <- try(posterior::as_draws_array(fit), silent = TRUE)
  if (!inherits(da, "try-error")) return(da)
  dbg("as_draws_array failed; trying as_draws_df(fit) ...")
  dd <- try(posterior::as_draws_df(fit), silent = TRUE)
  if (!inherits(dd, "try-error")) return(dd)
  dbg("as_draws_df failed; trying as_draws(fit) then coercion ...")
  dl <- try(posterior::as_draws(fit), silent = TRUE)
  if (!inherits(dl, "try-error")) {
    da2 <- try(posterior::as_draws_array(dl), silent = TRUE)
    if (!inherits(da2, "try-error")) return(da2)
    dd2 <- try(posterior::as_draws_df(dl), silent = TRUE)
    if (!inherits(dd2, "try-error")) return(dd2)
  }
  stop("Unable to coerce draws to a posterior::draws_* format usable by rhat/ess.")
}

draws_diag <- run_step("Coerce posterior draws for Rhat/ESS", {
  safe_draws_for_diagnostics(fit)
})

run_step("Compute Rhat and ESS ratios (bulk/tail)", {
  # Get all draws
  dr_all <- posterior::as_draws(fit)  # draws_list (works across brms backends)
  
  # Keep only model parameters (exclude sampler fields)
  vars_all  <- posterior::variables(dr_all)
  keep_vars <- grep("^(b_|r_|sd_|sigma|shape|cor_)", vars_all, value = TRUE)
  if (length(keep_vars) == 0) stop("No model parameters found for diagnostics.")
  
  # Subset then COERCE to draws_array to avoid draws_list bug in posterior 1.6.1
  dr_keep <- posterior::subset_draws(dr_all, variable = keep_vars)
  dr_arr  <- posterior::as_draws_array(dr_keep)
  
  # Compute diagnostics on draws_array
  rhat_all <- posterior::rhat(dr_arr)
  ess_b    <- posterior::ess_bulk(dr_arr)
  ess_t    <- posterior::ess_tail(dr_arr)
  nd       <- posterior::ndraws(dr_arr)
  
  neff_ratio_bulk <- as.numeric(ess_b) / nd
  neff_ratio_tail <- as.numeric(ess_t) / nd
  
  cat(sprintf("Rhat range (model params): [%.3f, %.3f]\n",
              min(rhat_all, na.rm = TRUE), max(rhat_all, na.rm = TRUE)))
  cat("ESS bulk ratio (summary):\n"); print(summary(neff_ratio_bulk))
  cat("ESS tail ratio (summary):\n"); print(summary(neff_ratio_tail))
  
  assign(".__RhatAll", rhat_all, envir = .GlobalEnv)
  assign(".__ESSBulkRatio", neff_ratio_bulk, envir = .GlobalEnv)
  assign(".__ESSTailRatio", neff_ratio_tail, envir = .GlobalEnv)
})

run_step("Trace & density overlays for ^b_ and ^shape", {
  da <- if (inherits(draws_diag, "draws_array")) draws_diag else posterior::as_draws_array(draws_diag)
  print(mcmc_trace(da, regex_pars = c("^b_", "^shape")))
  print(mcmc_dens_overlay(da, regex_pars = c("^b_", "^shape")))
})

# ============================================================
# (2) Posterior predictive checks (global fit)
# ============================================================
run_step("Posterior predictive checks", {
  print(pp_check(fit, type = "dens_overlay", ndraws = 100))
  print(pp_check(fit, type = "stat", stat = "mean"))
  print(pp_check(fit, type = "stat", stat = "sd"))
  print(pp_check(fit, type = "error_hist"))
})

# ============================================================
# (3) Residual structure (independence, nonlinearity, heteroscedasticity)
#     Manual Pearson residuals for Gamma: Var(y|mu,k) = mu^2 / k
# ============================================================
res_df <- run_step("Compute Pearson residuals (manual) & fitted", {
  mu_hat <- fitted(fit, summary = TRUE)[, "Estimate"]              # E[y]
  # shape (posterior mean)
  sh <- try(summary(fit)$spec_pars["shape", "Estimate"], silent = TRUE)
  if (inherits(sh, "try-error") || is.na(sh)) {
    sdra <- posterior::as_draws_df(fit)
    sh <- mean(sdra$shape)
  }
  var_hat <- (mu_hat^2) / sh
  var_hat[var_hat <= .Machine$double.eps] <- .Machine$double.eps
  r_pear <- (y - mu_hat) / sqrt(var_hat)
  dat %>% mutate(.fitted = mu_hat, .resid = r_pear)
})

run_step("Plot: residuals vs fitted", {
  print(
    ggplot(res_df, aes(.fitted, .resid)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_point(alpha = 0.45) +
      geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = 0.9) +
      labs(title = "Pearson residuals (manual) vs fitted", x = "Fitted (E[y])", y = "Pearson residual")
  )
})

run_step("Plots: residuals vs covariates (auto)", {
  covs <- intersect(c("Bheight_c", "WtavR_zg", "winSlope_c", "gulf"), names(res_df))
  for (v in covs) {
    gp <- if (is.numeric(res_df[[v]])) {
      ggplot(res_df, aes(.data[[v]], .resid)) +
        geom_hline(yintercept = 0, linetype = 2) +
        geom_point(alpha = 0.45) +
        geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = 0.9) +
        labs(title = paste("Pearson residuals vs", v), x = v, y = "Pearson residual")
    } else {
      ggplot(res_df, aes(.data[[v]], .resid)) +
        geom_hline(yintercept = 0, linetype = 2) +
        geom_boxplot(outlier.alpha = 0.3) +
        labs(title = paste("Pearson residuals by", v), x = v, y = "Pearson residual")
    }
    print(gp)
  }
})

# ============================================================
# (4) PSIS-LOO (moment_match = TRUE, reloo = FALSE)
# ============================================================
loo1 <- run_step("PSIS-LOO (moment_match=TRUE, reloo=FALSE)", {
  loo(fit, moment_match = TRUE, reloo = FALSE)
})
run_step("Print LOO summary & plot", {
  print(loo1)
  print(plot(loo1))
})

# Robust Pareto-k extraction (multiple fallbacks)
run_step("Pareto-k histogram (robust extraction)", {
  k_vals <- NA
  k_vals <- tryCatch({
    if (!is.null(loo1$diagnostics$pareto_k)) {
      as.numeric(loo1$diagnostics$pareto_k)
    } else if (!is.null(loo1$pareto_k)) {
      as.numeric(loo1$pareto_k)
    } else if ("pareto_k_values" %in% getNamespaceExports("loo")) {
      as.numeric(loo::pareto_k_values(loo1))
    } else {
      as.numeric(loo::pareto_k(loo1)) # may not exist in your loo
    }
  }, error = function(e) NA)
  if (all(is.na(k_vals))) {
    cat("Pareto-k values unavailable; skipping histogram.\n")
  } else {
    pk_df <- data.frame(k = k_vals)
    print(
      ggplot(pk_df, aes(k)) +
        geom_histogram(bins = 30, linewidth = 0.2) +
        geom_vline(xintercept = 0.7, linetype = 2) +
        labs(title = "Pareto-k histogram", x = "k", y = "Count")
    )
    assign(".__ParetoK", k_vals, envir = .GlobalEnv)
  }
})

# ============================================================
# (5) LOO-PIT overlay & predictive interval coverage
# ============================================================
run_step("LOO-PIT overlay & predictive intervals (robust)", {
  set.seed(1)
  yrep <- posterior_predict(fit, draws = 1000)
  
  # Try to extract a psis_object from the loo result (several known locations)
  psis_obj <- NULL
  psis_obj <- tryCatch({
    if (!is.null(loo1$psis_object)) {
      loo1$psis_object
    } else if (!is.null(loo1$diagnostics$psis_object)) {
      loo1$diagnostics$psis_object
    } else if (!is.null(loo1$diagnostics$psis)) {
      loo1$diagnostics$psis
    } else if (!is.null(loo1$psis)) {
      loo1$psis
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  if (!is.null(psis_obj)) {
    print(bayesplot::ppc_loo_pit_overlay(y = y, yrep = yrep, psis_object = psis_obj))
  } else {
    # Fallback: normalized weights (if available)
    wts <- try(weights(loo1, normalize = TRUE), silent = TRUE)
    if (!inherits(wts, "try-error") && !is.null(wts)) {
      print(bayesplot::ppc_loo_pit_overlay(y = y, yrep = yrep, lw = wts))
    } else {
      cat("Neither psis_object nor weights() available for LOO-PIT; skipping this plot.\n")
    }
  }
  
  # Predictive intervals figure (thin to avoid overplotting)
  yrep_thin <- yrep[sample(seq_len(nrow(yrep)), size = min(200, nrow(yrep))), , drop = FALSE]
  print(bayesplot::ppc_intervals(y, yrep_thin) + ggtitle("Predictive intervals vs observed"))
})

# ============================================================
# (6) Random-effects sanity checks
# ============================================================
run_step("Random effects plots (^r_)", {
  print(plot(fit, pars = "^r_", ask = FALSE))
})

# ============================================================
# (7) bayes_R2
# ============================================================
run_step("bayes_R2", {
  print(bayes_R2(fit))
})

# -------------------- CLOSE PDF -----------------------------
run_step("Close PDF", { .on_exit() })
cat(sprintf("\nSaved plots to: %s\n", pdf_file))

# ============================================================
# (8) Quick PASS criteria (console) with safeguards
# ============================================================
cat("\n=== Quick PASS criteria ===\n")
rhat_all <- if (exists(".__RhatAll", inherits = FALSE)) get(".__RhatAll", inherits = FALSE) else NA
pk       <- if (exists(".__ParetoK", inherits = FALSE)) get(".__ParetoK", inherits = FALSE) else NA

rho <- suppressWarnings(cor(abs(res_df$.resid), res_df$.fitted,
                            method = "spearman", use = "complete.obs"))

cat(sprintf("Rhat<=1.01 everywhere: %s\n",
            ifelse(all(is.na(rhat_all)), "UNKNOWN",
                   ifelse(max(rhat_all, na.rm = TRUE) <= 1.01, "OK", "CHECK"))))
if (!all(is.na(pk))) {
  cat(sprintf("Pareto-k mostly < 0.7: %s (%.1f%% < 0.7)\n",
              ifelse(mean(pk < 0.7, na.rm = TRUE) > 0.9, "OK", "CHECK"),
              mean(pk < 0.7, na.rm = TRUE)*100))
} else {
  cat("Pareto-k: UNKNOWN (unavailable in this loo version)\n")
}
cat(sprintf("Residual heteroscedasticity (|res| vs fitted Spearman |rho| < 0.2): %s (rho=%.3f)\n",
            ifelse(is.na(rho) || abs(rho) < 0.2, "OK", "CHECK"),
            ifelse(is.na(rho), NA, rho)))

cat("\nNotes:\n- If residuals show a funnel, consider a distributional model: sigma ~ gulf + Bheight_c.\n- With many Pareto-k > 0.7, prefer K-fold CV (keeping reloo = FALSE) or revise the model.\n- Gamma(log) Pearson residuals use Var = mu^2/shape; if tails misfit, check the lowest y values.\n")
