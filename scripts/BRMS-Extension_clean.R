# BRMS-Extension_clean.R
# ------------------------------------------------------------
# Purpose: Cleaned and organized script for BRMS-based analyses
#          of coral extension with inside/outside Gulf contrasts.
# Author: (your name here)
# Created: 2025-10-26
# Notes:
# - Expects a data.frame `testex` in the workspace with the variables
#   used below (see "Required columns" check).
# - This script does *not* auto-install packages by default (best practice
#   in shared/cluster environments). Set INSTALL_MISSING_PACKAGES = TRUE
#   if you want it to install what's missing.
# - A small utility `psurface()` is provided for 2D prediction grids
#   and a simple ggplot surface.
# ------------------------------------------------------------

# --- Config -------------------------------------------------
INSTALL_MISSING_PACKAGES <- FALSE
SEED <- 1234
CHAINS <- 3
CORES <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
options(mc.cores = CORES)

# --- Packages -----------------------------------------------
req_pkgs <- c(
  "brms",        # Bayesian models
  "performance", # r2_bayes()
  "loo",         # loo()
  "bayesplot",   # pp_check() backend used by brms
  "dplyr",       # data wrangling (across, where, any_of)
  "ggplot2",     # plotting
  "gridExtra",   # arranging ggplots
  "posterior",   # as_draws_df, draws handling
  "tidyr"        # data helpers
)

if (INSTALL_MISSING_PACKAGES) {
  to_install <- req_pkgs[!req_pkgs %in% rownames(installed.packages())]
  if (length(to_install)) {
    install.packages(to_install, dependencies = TRUE)
  }
}

invisible(lapply(req_pkgs, function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE))) {
    stop(sprintf("Package '%s' is not installed. Set INSTALL_MISSING_PACKAGES=TRUE to auto-install.", x))
  }
}))

set.seed(SEED)

# --- Data requirements --------------------------------------
required_cols <- c(
  # response
  "extension",
  # grouping
  "colony", "year", "gulf",
  # covariates mentioned
  "StavR", "WtavR", "WtsdR", "winSlope", "StsdR", "sumSlope", "WcsavR",
  "Bheight",
  # optional site column for some models
  "Site"
)

if (!exists("testex")) {
  stop("Object `testex` not found. Please load your dataset as `testex` before sourcing this script.")
}

missing_cols <- setdiff(required_cols, names(testex))
if (length(missing_cols)) {
  warning("The following expected columns are missing in `testex`: ", paste(missing_cols, collapse = ", "))
}

# Make sure some key columns are properly typed
testex <- testex %>%
  dplyr::mutate(
    gulf   = factor(gulf),
    colony = factor(colony),
    year   = factor(year),
    Site   = if ("Site" %in% names(.)) factor(Site) else factor(NA)
  )

# --- Within-gulf z-scoring and centering --------------------
testex <- testex %>%
  group_by(gulf) %>%
  mutate(
    StavR_zg = (StavR - mean(StavR, na.rm = TRUE)) / sd(StavR, na.rm = TRUE),
    WtavR_zg = (WtavR - mean(WtavR, na.rm = TRUE)) / sd(WtavR, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    WtsdR_c    = WtsdR - mean(WtsdR, na.rm = TRUE),
    winSlope_c = winSlope - mean(winSlope, na.rm = TRUE),
    StsdR_c    = StsdR - mean(StsdR, na.rm = TRUE),
    sumSlope_c = sumSlope - mean(sumSlope, na.rm = TRUE),
    WcsavR_c   = WcsavR - mean(WcsavR, na.rm = TRUE),
    Bheight_c  = Bheight - mean(Bheight, na.rm = TRUE)
  )

# ------------------------------------------------------------------
# Utility: 2D surface plot helper using model predictions
# ------------------------------------------------------------------
psurface <- function(x, y, data, model, grid_length = 50,
                     xlab = x, ylab = y, re_formula = NA) {
  stopifnot(is.data.frame(data))
  if (!all(c(x, y) %in% names(data))) {
    stop("`x` and/or `y` not found in data.")
  }

  xseq <- seq(min(data[[x]], na.rm = TRUE), max(data[[x]], na.rm = TRUE), length.out = grid_length)
  yseq <- seq(min(data[[y]], na.rm = TRUE), max(data[[y]], na.rm = TRUE), length.out = grid_length)

  grid <- tidyr::expand_grid(!!x := xseq, !!y := yseq)

  # Fill other required columns with typical values
  needed <- setdiff(all.vars(formula(model)), c("extension", x, y))
  for (nm in needed) {
    if (!nm %in% names(grid) && nm %in% names(data)) {
      if (is.numeric(data[[nm]])) {
        grid[[nm]] <- mean(data[[nm]], na.rm = TRUE)
      } else if (is.factor(data[[nm]])) {
        grid[[nm]] <- levels(data[[nm]])[1]
      } else {
        grid[[nm]] <- data[[nm]][[1]]
      }
    }
  }

  mu <- brms::posterior_epred(model, newdata = grid, re_formula = re_formula)
  grid$fit <- apply(mu, 2, mean, na.rm = TRUE)

  p <- ggplot(grid, aes_string(x = x, y = y, fill = "fit")) +
    geom_raster(interpolate = TRUE) +
    labs(x = xlab, y = ylab, fill = "Predicted") +
    theme_minimal()

  print(p)
  invisible(list(grid = grid, plot = p))
}

# ------------------------------------------------------------------
# Model set 1: Compare Gaussian vs Gamma on saturated structure
# ------------------------------------------------------------------
form_saturated <- bf(
  extension ~ StavR_zg + WtavR_zg +
    WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +
    gulf + Bheight_c +
    StavR_zg:Bheight_c + WtavR_zg:Bheight_c +
    winSlope_c:Bheight_c + StsdR_c:Bheight_c + sumSlope_c:Bheight_c +
    StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf +
    winSlope_c:gulf + StsdR_c:gulf + sumSlope_c:gulf + WcsavR_c:gulf +
    Bheight_c:gulf +
    (1 + Bheight_c | colony/year)
)

ex_gauss <- brm(
  formula = form_saturated,
  data    = testex,
  family  = gaussian(),
  chains  = CHAINS, iter = 3000, warmup = 1000,
  control = list(adapt_delta = 0.95),
  seed    = SEED
)

ex_gamma <- brm(
  formula = form_saturated,
  data    = testex,
  family  = Gamma(link = "log"),
  chains  = CHAINS, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.95),
  seed    = SEED
)

# Model comparison + checks
print(performance::r2_bayes(ex_gauss))
print(performance::r2_bayes(ex_gamma))
bayesplot::pp_check(ex_gauss)
bayesplot::pp_check(ex_gamma)
loo_compare_1 <- loo::loo_compare(loo::loo(ex_gauss), loo::loo(ex_gamma))
print(loo_compare_1)

# ------------------------------------------------------------------
# Model set 2: Reduced random-effects candidates (Gamma family)
# ------------------------------------------------------------------
form_base <- bf(
  extension ~ StavR_zg + WtavR_zg + WtsdR_c + winSlope_c + StsdR_c + sumSlope_c +
    WcsavR_c + gulf + Bheight_c +
    StavR_zg:Bheight_c + WtavR_zg:Bheight_c +
    winSlope_c:Bheight_c + StsdR_c:Bheight_c + sumSlope_c:Bheight_c +
    StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf +
    winSlope_c:gulf + StsdR_c:gulf + sumSlope_c:gulf + WcsavR_c:gulf +
    Bheight_c:gulf
)

m_re1 <- brm(form_base + (1 | colony/year),
             data = testex, family = Gamma(link = "log"),
             chains = CHAINS, iter = 5000, warmup = 3000,
             control = list(adapt_delta = 0.95), seed = SEED)

m_re2 <- brm(form_base + (1 | colony),
             data = testex, family = Gamma(link = "log"),
             chains = CHAINS, iter = 5000, warmup = 3000,
             control = list(adapt_delta = 0.95), seed = SEED)

m_re3 <- brm(form_base + (1 + Bheight_c | colony),
             data = testex, family = Gamma(link = "log"),
             chains = CHAINS, iter = 5000, warmup = 3000,
             control = list(adapt_delta = 0.95), seed = SEED)

loo_compare_2 <- loo::loo_compare(loo::loo(ex_gamma), loo::loo(m_re1), loo::loo(m_re2), loo::loo(m_re3))
print(loo_compare_2)

# Choose a reasonable working model (example: m_re1 if tied)
working_gamma <- m_re1

# Further pruning based on weak terms (example workflow)
m_prune1 <- brm(
  extension ~ StavR_zg + WtavR_zg + WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +
    gulf + Bheight_c +
    winSlope_c:Bheight_c + StsdR_c:Bheight_c +
    WtavR_zg:gulf + WcsavR_c:gulf + Bheight_c:gulf +
    (1 | colony/year),
  data = testex, family = Gamma(link = "log"),
  chains = CHAINS, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.95), seed = SEED
)

m_prune2 <- brm(
  extension ~ WtavR_zg + winSlope_c + WcsavR_c + gulf + Bheight_c +
    winSlope_c:Bheight_c + WtavR_zg:gulf + WcsavR_c:gulf + Bheight_c:gulf +
    (1 | colony/year),
  data = testex, family = Gamma(link = "log"),
  chains = CHAINS, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.95), seed = SEED
)

loo_compare_3 <- loo::loo_compare(loo::loo(ex_gamma), loo::loo(m_re1),
                                  loo::loo(m_re2), loo::loo(m_re3),
                                  loo::loo(m_prune1), loo::loo(m_prune2))
print(loo_compare_3)

# Diagnostics for the pruned candidate
print(performance::r2_bayes(m_prune2))
bayesplot::pp_check(m_prune2)
meff <- brms::marginal_effects(m_prune2)
plot(meff)

# ------------------------------------------------------------------
# Heterogeneity in random effects across gulf (two parameterizations)
# ------------------------------------------------------------------
m_gulfre <- brm(
  extension ~ WtavR_zg + winSlope_c + WcsavR_c + gulf + Bheight_c +
    winSlope_c:Bheight_c + WtavR_zg:gulf + WcsavR_c:gulf +
    (1 | colony:gulf/year),
  data = testex, family = Gamma(link = "log"),
  chains = CHAINS, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.99), seed = SEED
)

m_gulfinter <- brm(
  extension ~ WtavR_zg + winSlope_c + WcsavR_c + gulf + Bheight_c +
    winSlope_c:Bheight_c + WtavR_zg:gulf + WcsavR_c:gulf +
    (1 | gulf/colony/year),
  data = testex, family = Gamma(link = "log"),
  chains = CHAINS, iter = 7000, warmup = 2000,
  control = list(adapt_delta = 0.99), seed = SEED
)

loo_compare_4 <- loo::loo_compare(loo::loo(ex_gamma), loo::loo(m_re1),
                                  loo::loo(m_gulfre), loo::loo(m_gulfinter),
                                  loo::loo(m_prune1), loo::loo(m_prune2))
print(loo_compare_4)

# Surface plots for working model (using pruned model as example)
psurface("Bheight_c", "WcsavR_c", data = testex, model = m_prune2,
         xlab = "Back Calc. Height (centered cm)",
         ylab = "Current Speed (centered m/s)")
psurface("Bheight_c", "WtavR_zg", data = testex, model = m_prune2,
         xlab = "Back Calc. Height (centered cm)",
         ylab = "Winter Temp (within-gulf z)")
psurface("Bheight_c", "winSlope_c", data = testex, model = m_prune2,
         xlab = "Back Calc. Height (centered cm)",
         ylab = "Rate of Change (centered deg/day)")

# ------------------------------------------------------------------
# Residual diagnostics for a selected model
# ------------------------------------------------------------------
aligned_fitted <- fitted(m_prune2)[, "Estimate"]
aligned_residuals <- residuals(m_prune2, summary = TRUE)[, "Estimate"]

plot(aligned_fitted, aligned_residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)
lines(lowess(aligned_fitted, aligned_residuals), col = "darkgreen", lwd = 2)

# ------------------------------------------------------------------
# Optional: model with Gamma shape sub-models
# ------------------------------------------------------------------
m_shape_site <- brm(
  bf(extension ~ WtavR_zg + winSlope_c + WcsavR_c + gulf + Bheight_c +
       winSlope_c:Bheight_c + WtavR_zg:gulf + WcsavR_c:gulf +
       (1 | colony/year),
     shape ~ Site),
  data = testex, family = Gamma(link = "log"),
  chains = CHAINS, iter = 6000, warmup = 3000,
  control = list(adapt_delta = 0.95), seed = SEED
)

m_shape_covs <- brm(
  bf(extension ~ WtavR_zg + winSlope_c + WcsavR_c + gulf + Bheight_c +
       winSlope_c:Bheight_c + WtavR_zg:gulf + WcsavR_c:gulf +
       (1 | colony/year),
     shape ~ WtavR_zg + WcsavR_c),
  data = testex, family = Gamma(link = "log"),
  chains = CHAINS, iter = 6000, warmup = 3000,
  control = list(adapt_delta = 0.95), seed = SEED
)
print(summary(m_shape_covs))
bayesplot::pp_check(m_shape_covs)

# ------------------------------------------------------------------
# Gulf-only analyses (subset + scaled except Bheight/year/extension)
# ------------------------------------------------------------------
testexgulf <- subset(testex, gulf == "gulf")

scaled_testexgulf <- testexgulf %>%
  dplyr::mutate(across(
    .cols = dplyr::where(is.numeric) & !dplyr::any_of(c("Bheight", "Bheight_c", "year", "extension")),
    .fns = ~ as.numeric(scale(.)),
    .names = "{.col}"
  ))

# Baselines
gulf_re_full <- brm(
  extension ~ StavR + WtavR + WtsdR + winSlope + StsdR + sumSlope + WcsavR + Bheight +
    StavR:Bheight + WtavR:Bheight + winSlope:Bheight + StsdR:Bheight + sumSlope:Bheight +
    (1 + Bheight | colony/year),
  data = testexgulf, family = Gamma(link = "log"),
  chains = CHAINS, iter = 6000, warmup = 3000,
  control = list(adapt_delta = 0.99), seed = SEED
)

gulf_re_simpler <- brm(
  extension ~ StavR + WtavR + WtsdR + winSlope + StsdR + sumSlope + WcsavR + Bheight +
    (1 | colony/year),
  data = testexgulf, family = Gamma(link = "log"),
  chains = CHAINS, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99), seed = SEED
)

# Scaled candidates
gulf_sc2 <- brm(
  extension ~ StavR + WtavR + WtsdR + winSlope + StsdR + sumSlope + WcsavR + Bheight +
    (1 | colony/year),
  data = scaled_testexgulf, family = Gamma(link = "log"),
  chains = CHAINS, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99), seed = SEED
)

gulf_sc3 <- brm(
  extension ~ StavR + WtavR + WtsdR + winSlope + StsdR + sumSlope + WcsavR + Bheight +
    (1 | colony),
  data = scaled_testexgulf, family = Gamma(link = "log"),
  chains = CHAINS, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99), seed = SEED
)

# Additive vs with interactions (examples)
gulf_sc24 <- brm(
  extension ~ Site + StavR + WtavR + WtsdR + winSlope + StsdR + sumSlope + WcsavR + Bheight +
    StsdR:Bheight + WcsavR:Bheight + Site:Bheight +
    (1 | colony/year),
  data = scaled_testexgulf, family = Gamma(link = "log"),
  chains = CHAINS, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99), seed = SEED
)

# Parsimonious (as indicated by your notes)
gulf_best <- brm(
  extension ~ WtsdR + StsdR + sumSlope + Bheight + StsdR:Bheight +
    (1 | colony/year),
  data = scaled_testexgulf, family = Gamma(link = "log"),
  chains = CHAINS, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99), seed = SEED
)

# Checks
print(performance::r2_bayes(gulf_best))
bayesplot::pp_check(gulf_best)

# Residuals for gulf_best
gfit <- fitted(gulf_best)[, "Estimate"]
gres <- residuals(gulf_best, summary = TRUE)[, "Estimate"]
plot(gfit, gres,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values (Gulf model)", pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)
lines(lowess(gfit, gres), col = "darkgreen", lwd = 2)

# Residuals vs key predictors (quick-look)
scaled_testexgulf$residuals <- scaled_testexgulf$extension - gfit
fx_vars <- c("sumSlope", "WtsdR", "StsdR", "Bheight")

plots <- lapply(fx_vars, function(v) {
  ggplot(scaled_testexgulf, aes_string(x = v, y = "residuals")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal() +
    labs(title = paste("Residuals vs", v))
})
do.call(gridExtra::grid.arrange, c(plots, ncol = 2))

# ------------------------------------------------------------------
# Variance decomposition (illustrative; careful interpretation needed)
# ------------------------------------------------------------------
current_model <- gulf_best

compute_variance_contribution <- function(terms, full_model, original_data) {
  reduced_formula <- reformulate(terms, response = "extension")
  reduced_model <- update(full_model, formula = reduced_formula)
  preds <- posterior_epred(reduced_model, newdata = original_data, re_formula = NA)
  apply(preds, 1, stats::var)
}

fixed_effect_terms <- c("WtsdR", "StsdR", "sumSlope", "Bheight", "StsdR:Bheight")
posterior_preds <- posterior_epred(current_model, newdata = scaled_testexgulf, re_formula = NA)
posterior_total_var <- apply(posterior_preds, 1, stats::var)
total_var <- mean(posterior_total_var)

results <- data.frame(Term = fixed_effect_terms, Variance_Proportion = NA, SE = NA)

for (i in seq_along(fixed_effect_terms)) {
  excluded <- fixed_effect_terms[i]
  dependent <- fixed_effect_terms[sapply(fixed_effect_terms, function(term) {
    any(unlist(strsplit(term, ":")) %in% excluded)
  })]
  included <- setdiff(fixed_effect_terms, c(excluded, dependent))

  subset_var <- compute_variance_contribution(included, current_model, scaled_testexgulf)
  variance_proportion <- (posterior_total_var - subset_var) / total_var
  results$Variance_Proportion[i] <- mean(variance_proportion, na.rm = TRUE)
  results$SE[i] <- sd(variance_proportion, na.rm = TRUE) / sqrt(length(variance_proportion))
}

# Random effects (approximate contribution via sd params)
post_samples <- posterior::as_draws_df(current_model) %>% as.data.frame()
re_cols <- grep("^sd_", colnames(post_samples), value = TRUE)
if (length(re_cols)) {
  random_effect_vars <- colMeans(post_samples[, re_cols, drop = FALSE]^2)
  random_effect_results <- data.frame(
    Term = names(random_effect_vars),
    Variance_Proportion = random_effect_vars / total_var,
    SE = apply(post_samples[, re_cols, drop = FALSE]^2, 2,
               function(vs) sd(vs / total_var) / sqrt(nrow(post_samples)))
  )
  results <- rbind(results, random_effect_results)
}

# Normalize to sum to 1
results$Variance_Proportion <- pmax(0, results$Variance_Proportion)
results$Variance_Proportion <- results$Variance_Proportion / sum(results$Variance_Proportion, na.rm = TRUE)
results <- results[order(-results$Variance_Proportion), ]
print(results)
cat("Sum of Variance Proportions:", sum(results$Variance_Proportion, na.rm = TRUE), "\n")

# Group and plot
results$Group <- dplyr::case_when(
  results$Term %in% c("StsdR", "StsdR:Bheight") ~ "StsdR and interaction",
  results$Term %in% c("Bheight") ~ "Bheight",
  results$Term %in% c("WtsdR") ~ "WtsdR",
  results$Term %in% c("sumSlope") ~ "sumSlope",
  grepl("^sd_", results$Term) ~ "Random effects",
  TRUE ~ results$Term
)

grouped_results <- results %>%
  group_by(Group) %>%
  summarise(
    Variance_Proportion = sum(Variance_Proportion, na.rm = TRUE),
    SE = sqrt(sum(SE^2, na.rm = TRUE))
  ) %>%
  arrange(desc(Variance_Proportion))

print(grouped_results)

ggplot(grouped_results, aes(x = reorder(Group, -Variance_Proportion), y = Variance_Proportion)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Variance_Proportion - SE, ymax = Variance_Proportion + SE), width = 0.3) +
  labs(x = "Terms/Groups", y = "Variance Proportion", title = "Variance Decomposition by Terms and Groups") +
  theme_minimal() +
  coord_flip()

# ------------------------------------------------------------------
# Save workspace image if desired
# ------------------------------------------------------------------
# save.image(file = "BRMS-Ex-Gamma.RData")
# save.image(file = "Extension_complete.RData")
# load(file = "Extension_complete.RData")  # example
# ------------------------------------------------------------
