## Useful functions
# ---- Interaction drop-check helper (brms) -----------------------------------
# Requires: brms, loo
# ---- Interaction drop-check helper (brms) -----------------------------------
# Requires: brms, loo
interaction_dropcheck <- function(
    fit,                        # brmsfit: your current (full) model
    data,                       # data.frame used to fit
    terms = NULL,               # optional: which interaction terms to test
    rope = c(-0.05, 0.05),
    chains = 4,                 # parallel chains
    cores  = max(1L, parallel::detectCores()),  # cores for sampling
    iter = 3000, warmup = 1500,
    control = list(adapt_delta = 0.98),
    mm = TRUE, reloo = FALSE,
    seed = 1234,
    family = NULL,
    quiet = TRUE,
    loo_cores = max(1L, floor(parallel::detectCores()/2)) # cores for LOO
) {
  stopifnot(inherits(fit, "brmsfit"))
  if (is.null(family)) family <- fit$family
  
  # pull priors from the provided fit
  prior_from_fit <- fit$prior  # may be length 0 (that's fine)
  
  # ------------------ term set ------------------
  if (is.null(terms)) {
    fe_names <- rownames(brms::fixef(fit))
    fe_names <- setdiff(fe_names, "Intercept")
    terms <- fe_names[grepl(":", fe_names, fixed = TRUE)]
  }
  terms <- unique(terms)
  if (length(terms) == 0L) {
    out <- data.frame(
      term=character(), mean=numeric(), prob_gt0=numeric(), prob_lt0=numeric(),
      rope_mass=numeric(), prob_dir=numeric(), elpd_diff=numeric(), se_diff=numeric(),
      safe_to_drop_by_LOO=logical(), weak_by_posterior=logical(), recommend_drop=logical(),
      stringsAsFactors = FALSE
    )
    attr(out, "message") <- "No interaction terms found."
    return(out)
  }
  
  set.seed(seed)
  
  # ------------------ posterior summaries ------------------
  post <- brms::as_draws_df(fit)
  grab <- function(name, rope) {
    col <- paste0("b_", name)
    if (!col %in% names(post)) {
      return(c(mean = NA_real_, prob_gt0 = NA_real_, prob_lt0 = NA_real_, rope_mass = NA_real_))
    }
    x <- post[[col]]
    c(
      mean      = mean(x),
      prob_gt0  = mean(x > 0),
      prob_lt0  = mean(x < 0),
      rope_mass = mean(x > rope[1] & x < rope[2])
    )
  }
  int_post <- do.call(
    rbind,
    lapply(terms, function(nm) {
      vals <- grab(nm, rope = rope)
      data.frame(
        term      = nm,
        mean      = unname(vals["mean"]),
        prob_gt0  = unname(vals["prob_gt0"]),
        prob_lt0  = unname(vals["prob_lt0"]),
        rope_mass = unname(vals["rope_mass"]),
        stringsAsFactors = FALSE
      )
    })
  )
  int_post$prob_dir <- pmax(int_post$prob_gt0, int_post$prob_lt0, na.rm = TRUE)
  
  # ------------------ LOO helpers ------------------
  safe_add_loo <- function(fit, mm = TRUE, reloo = FALSE) {
    if (is.null(fit$criteria$loo)) {
      old_mc <- getOption("mc.cores", 1L)
      on.exit(options(mc.cores = old_mc), add = TRUE)
      options(mc.cores = as.integer(loo_cores))
      fit <- brms::add_criterion(fit, "loo", moment_match = mm, reloo = reloo)
    }
    fit
  }
  
  # ---- filter priors to match the reduced formula (fix for your error) ------
  filter_priors_for_formula <- function(orig_prior, formula, data, family) {
    # nothing to filter
    if (length(orig_prior) == 0L) return(NULL)
    
    tmpl <- brms::get_prior(formula = formula, data = data, family = family)
    orig_df <- as.data.frame(orig_prior)
    tmpl_df <- as.data.frame(tmpl)
    
    keys <- c("class","coef","group","resp","dpar","nlpar")
    # build match keys, treating NA as empty string
    keyify <- function(df) {
      miss <- setdiff(keys, names(df))
      if (length(miss)) df[miss] <- NA_character_
      apply(df[keys], 1, function(r) paste(ifelse(is.na(r), "", r), collapse = "|"))
    }
    orig_df$.key <- keyify(orig_df)
    tmpl_df$.key <- keyify(tmpl_df)
    
    keep <- orig_df$.key %in% unique(tmpl_df$.key)
    if (!any(keep)) return(NULL)
    
    # IMPORTANT: row subset, not column subset
    orig_prior[keep, , drop = FALSE]
  }
  
  drop_once <- function(fit_full, term) {
    f_new <- update(stats::formula(fit_full), paste(". ~ . -", term))
    
    # filter priors so none target removed parameters
    prior_filtered <- filter_priors_for_formula(prior_from_fit, f_new, data, family)
    
    fit_red <- brms::brm(
      formula   = f_new,
      data      = data,
      family    = family,
      prior     = prior_filtered,                # <- filtered (or NULL)
      chains    = chains,
      cores     = as.integer(cores),             # parallel sampling
      iter      = iter,
      warmup    = warmup,
      control   = control,
      save_pars = brms::save_pars(all = TRUE),
      seed      = seed,
      silent    = if (quiet) 2 else 0,
      refresh   = if (quiet) 0 else 200
    )
    
    fit_full <- safe_add_loo(fit_full, mm = mm, reloo = reloo)
    fit_red  <- safe_add_loo(fit_red,  mm = mm, reloo = reloo)
    
    cmp <- loo::loo_compare(list(reduced = fit_red$criteria$loo,
                                 full    = fit_full$criteria$loo))
    cmp_df <- as.data.frame(cmp)
    cmp_df$model <- rownames(cmp_df); rownames(cmp_df) <- NULL
    
    ediff <- cmp_df$elpd_diff[cmp_df$model == "reduced"]
    sdiff <- cmp_df$se_diff[cmp_df$model == "reduced"]
    if (length(ediff) != 1 || length(sdiff) != 1 || any(is.na(c(ediff, sdiff)))) {
      stop("Could not extract elpd_diff/se_diff for the reduced model.")
    }
    
    data.frame(term = term,
               elpd_diff = as.numeric(ediff),
               se_diff   = as.numeric(sdiff),
               stringsAsFactors = FALSE)
  }
  
  # ------------------ run one-step drops (in sequence) -----------------------
  # (Refits are parallel across chains inside brm(); if you want to parallelize
  # across terms too, we can add future.apply support later.)
  loo_tab <- do.call(rbind, lapply(terms, function(t) drop_once(fit, t)))
  
  # ------------------ combine + rules ------------------
  tab <- merge(int_post, loo_tab, by = "term", sort = FALSE)
  tab$safe_to_drop_by_LOO <- with(tab, elpd_diff >= -1 * se_diff)
  tab$weak_by_posterior   <- with(tab, prob_dir < 0.95 | rope_mass > 0.5)
  tab$recommend_drop      <- tab$safe_to_drop_by_LOO & tab$weak_by_posterior
  
  tab <- tab[order(tab$recommend_drop, tab$rope_mass, -tab$prob_dir, decreasing = TRUE), ]
  
  attr(tab, "rope")   <- rope
  attr(tab, "mm")     <- mm
  attr(tab, "reloo")  <- reloo
  attr(tab, "family") <- family
  attr(tab, "seed")   <- seed
  class(tab) <- c("interaction_dropcheck", class(tab))
  tab
}

