library(lattice)    # for dotplot()
library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)

library(brms)
library(cmdstanr)   # backend for brms
library(loo)
library(performance)

library(plotly)



testex <- read.csv("processeddata/testex.csv")
str(testex)
#### checking extension for outliers / measurement errors
dotplot(testex$extension)
hist(testex$extension)
boxplot(testex$extension)
## there is a clear measurement / data entry error of an extension of 3cm for a single point - over double most extension. Unlikely. Dropping.
testex <- subset(testex, extension < 2.8)
str(testex)
dotplot(testex$extension)

## now adding eexplantory variable scaling and z scores
## In relation to non-overlapping temperatures inside/outside the gulf, we will standardize data by within-gulf z-scores. By doing this and 
## and testing for interactions between z scores and gulf, we can examine within-gulf slopes (i.e. the effect of temperature on extension within 
## each gulf92s observed range (no extrapolation) and interaction terms: how those slopes differ between gulfs.

## We will also centre the other relevant variables for ease of model fitting and simple interpretation of the final model:
testex <- testex %>%
  group_by(gulf) %>%
  mutate(
    StavR_zg = (StavR - mean(StavR, na.rm=TRUE)) / sd(StavR, na.rm=TRUE),
    WtavR_zg = (WtavR - mean(WtavR, na.rm=TRUE)) / sd(WtavR, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    WtsdR_c = WtsdR - mean(WtsdR, na.rm=TRUE),
    winSlope_c = winSlope - mean(winSlope, na.rm=TRUE),
    StsdR_c = StsdR - mean(StsdR, na.rm=TRUE),
    sumSlope_c = sumSlope - mean(sumSlope, na.rm=TRUE),
    WcsavR_c = WcsavR - mean(WcsavR, na.rm=TRUE),
    Bheight_c = Bheight - mean(Bheight, na.rm=TRUE)
  )

## for cummulcative degree heating and degree cooling weeks 
# Positive variable with many zeros 
testex$Z_dhwm <- as.integer(testex$cummu_dhwm > 0) 
testex$M_dhwm <- ifelse(testex$cummu_dhwm > 0, log1p(testex$cummu_dhwm), 0)
# Negative variable with many zeros (values ≤ 0) 
testex$Z_dcwm <- as.integer(testex$cummu_dcwm < 0) 
testex$M_dcwm <- ifelse(testex$cummu_dcwm < 0, log1p(-testex$cummu_dcwm), 0) 
testex$Ms_dhwm <- as.numeric(scale(testex$M_dhwm)) 
testex$Ms_dcwm <- as.numeric(scale(testex$M_dcwm))

str(testex)
# ========================== SETUP ==========================
library(brms)
options(brms.backend = "cmdstanr",
        mc.cores = max(1, parallel::detectCores() - 1))
set.seed(1234)

# (Optional) weakly-informative priors that work well with centered/z inputs
priors <- c(
  prior(normal(0, 1), class = "b"),        # fixed effects
  prior(exponential(1), class = "sd")      # group-level SDs
)

# A small helper used everywhere
fast_add_loo <- function(fit, mm = FALSE, reloo = FALSE) {
  if (is.null(fit$criteria$loo)) {
    fit <- add_criterion(fit, "loo", moment_match = mm, reloo = reloo)
  }
  fit
}

# =================== SATURATED BASELINES ===================
# =========== RANDOM-STRUCTURE SEARCH: BASE (no slopes) ===========
base <- brm(
  extension ~ StavR_zg + WtavR_zg + WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +
    gulf + Bheight_c +
    StavR_zg:Bheight_c + WtavR_zg:Bheight_c + winSlope_c:Bheight_c + StsdR_c:Bheight_c + sumSlope_c:Bheight_c +
    StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf + winSlope_c:gulf + StsdR_c:gulf + sumSlope_c:gulf +
    WcsavR_c:gulf + Bheight_c:gulf +
    (1 | colony/year),
  data   = testex,
  family = Gamma(link = "log"),
  prior  = priors,
  chains = 3, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.98),
  save_pars = save_pars(all = TRUE)
)

# ============== UNCORRELATED COLONY-LEVEL SLOPES ==============
covs <- c("Bheight_c","StavR_zg","WtavR_zg","WtsdR_c",
          "winSlope_c","StsdR_c","sumSlope_c","WcsavR_c")

base_rhs_rs <- paste(
  "StavR_zg + WtavR_zg + WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +",
  "gulf + Bheight_c +",
  "StavR_zg:Bheight_c + WtavR_zg:Bheight_c + winSlope_c:Bheight_c + StsdR_c:Bheight_c + sumSlope_c:Bheight_c +",
  "StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf + winSlope_c:gulf + StsdR_c:gulf + sumSlope_c:gulf +",
  "WcsavR_c:gulf + Bheight_c:gulf +",
  "(1 | colony:year) + (1 | colony)"
)

make_formula_rs <- function(v) {
  rhs <- paste(base_rhs_rs, "+", sprintf("(0 + %s | colony)", v))  # uncorrelated slope
  bf(as.formula(paste("extension ~", rhs)))
}

fits_rs1 <- lapply(covs, function(v) {
  brm(
    formula = make_formula_rs(v),
    data    = testex,
    family  = Gamma(link = "log"),
    prior   = priors,
    chains  = 3, iter = 4000, warmup = 2000,
    control = list(adapt_delta = 0.98),
    save_pars = save_pars(all = TRUE)
  )
})
names(fits_rs1) <- covs
invisible(lapply(fits_rs1, \(m) print(formula(m))))

# ============== CORRELATED COLONY-LEVEL SLOPES ==============
base_rhs_cor <- paste(
  "StavR_zg + WtavR_zg + WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +",
  "gulf + Bheight_c +",
  "StavR_zg:Bheight_c + WtavR_zg:Bheight_c + winSlope_c:Bheight_c +",
  "StsdR_c:Bheight_c + sumSlope_c:Bheight_c +",
  "StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf + winSlope_c:gulf +",
  "StsdR_c:gulf + sumSlope_c:gulf + WcsavR_c:gulf + Bheight_c:gulf +",
  "(1 | colony:year)"
)

make_formula_cor <- function(v) {
  rhs <- paste(base_rhs_cor, "+", sprintf("(1 + %s | colony)", v))  # correlated slope+intercept
  bf(as.formula(paste("extension ~", rhs)))
}

fits_cor <- lapply(covs, function(v) {
  brm(
    formula = make_formula_cor(v),
    data    = testex,
    family  = Gamma(link = "log"),
    prior   = priors,
    chains  = 3, iter = 4000, warmup = 2000,
    control = list(adapt_delta = 0.98),
    save_pars = save_pars(all = TRUE)
  )
})
names(fits_cor) <- paste0(covs, "_cor")

# Add LOO to all candidates and compare
all_fits <- c(list(base = base), fits_rs1, fits_cor)
all_fits <- lapply(all_fits, fast_add_loo, mm = FALSE, reloo = FALSE)
loos <- lapply(all_fits, function(m) m$criteria$loo)
names(loos) <- names(all_fits)
cmp_all <- loo_compare(loos)
print(cmp_all)

## there is a minor suggestion that having an un correlated random slope using WtavR_zg is better than the base model 
## but the difference is negligible
base <- fast_add_loo(base, mm = TRUE)
WtavR_zg <- fast_add_loo(fits_rs1$WtavR_zg, mm = TRUE)
loo_compare(base$criteria$loo, WtavR_zg$criteria$loo)

## ok. Since ∣ELPD difference∣=1.9 < 2 ×SE (1.0)=2.0, then this is really not worth the both and no random slope is more parsimonious

# ===================== FAST BACKWARD: INTERACTIONS =====================
# ===================== BACKWARD: INTERACTIONS =====================
# --- helpers (same as your working main-effects version) ---
fast_add_loo <- function(fit, mm = TRUE, reloo = FALSE) {
  if (!is.null(fit$criteria$loo)) fit$criteria$loo <- NULL
  add_criterion(fit, "loo", moment_match = mm, reloo = reloo)
}

loo_diff_new_minus_base <- function(loo_new, loo_base) {
  cmp <- loo::loo_compare(list(new = loo_new, base = loo_base))
  if (rownames(cmp)[1] == "new") {
    c(elpd_diff = -as.numeric(cmp["base","elpd_diff"]),
      se_diff   =  as.numeric(cmp["base","se_diff"]))
  } else {
    c(elpd_diff =  as.numeric(cmp["new","elpd_diff"]),
      se_diff   =  as.numeric(cmp["new","se_diff"]))
  }
}

get_fe_labels <- function(fit) {
  labs <- rownames(brms::fixef(fit))
  labs[labs != "Intercept"]
}

drop_once <- function(fit_full, term,
                      data = testex, prior = priors,
                      iter = 3000, warmup = 1500, chains = 3,
                      control = list(adapt_delta = 0.98)) {
  f_old <- formula(fit_full)
  f_new <- update(f_old, paste(". ~ . -", term))
  
  # bail if nothing changed
  if (identical(deparse(f_old), deparse(f_new))) {
    return(list(elpd_diff = NA_real_, se_diff = NA_real_, fit_new = fit_full))
  }
  
  fit_red <- brms::brm(
    formula = f_new, data = data,
    family = Gamma(link = "log"), prior = prior,
    chains = chains, iter = iter, warmup = warmup,
    control = control, save_pars = save_pars(all = TRUE), silent = 2
  )
  
  fit_full <- fast_add_loo(fit_full, mm = TRUE, reloo = FALSE)
  fit_red  <- fast_add_loo(fit_red,  mm = TRUE, reloo = FALSE)
  
  diffs <- loo_diff_new_minus_base(fit_red$criteria$loo, fit_full$criteria$loo)
  list(elpd_diff = as.numeric(diffs["elpd_diff"]),
       se_diff   = as.numeric(diffs["se_diff"]),
       fit_new   = fit_red)
}

# ===================== BACKWARD: INTERACTIONS =====================
# ===================== BACKWARD: INTERACTIONS (fixed) =====================

# helper to check if term (or any variant) is in model
term_present_in_model <- function(term, fe_labels) {
  any(startsWith(fe_labels, paste0(term)))
}

base_fit <- all_fits[["base"]]

cand_terms <- c(
  "StavR_zg:Bheight_c","WtavR_zg:Bheight_c","winSlope_c:Bheight_c",
  "StsdR_c:Bheight_c","sumSlope_c:Bheight_c",
  "StavR_zg:gulf","WtavR_zg:gulf","WtsdR_c:gulf","winSlope_c:gulf",
  "StsdR_c:gulf","sumSlope_c:gulf","WcsavR_c:gulf","Bheight_c:gulf"
)

threshold_SE <- 1.0  # drop if ΔELPD ≥ -1 * SE
path <- data.frame(step = integer(), dropped = character(),
                   elpd_diff = double(), se_diff = double())

current_fit <- base_fit
step <- 0

repeat {
  fe_now <- get_fe_labels(current_fit)
  
# --- FIXED FILTER ---
  remaining <- cand_terms[vapply(cand_terms, term_present_in_model, logical(1), fe_now)]
  if (!length(remaining)) {
    message("No interaction candidates left."); break
  }
  
  cat("Trying drops for:\n  ", paste(remaining, collapse = "\n  "), "\n")
  
  trials <- lapply(remaining, function(t) drop_once(current_fit, t))
  tab <- do.call(rbind, Map(function(t, res)
    data.frame(term = t,
               elpd_diff = as.numeric(res$elpd_diff),
               se_diff   = as.numeric(res$se_diff)),
    remaining, trials))
  
  tab <- subset(tab, is.finite(elpd_diff) & is.finite(se_diff))
  if (!nrow(tab)) { message("No valid interaction trials; stopping."); break }
  
  tab <- tab[order(-tab$elpd_diff), ]
  print(tab, row.names = FALSE)
  
  best <- tab[1, ]
  if (best$elpd_diff >= -threshold_SE * best$se_diff) {
    step <- step + 1
    path <- rbind(path, transform(best, step = step))
    message(sprintf("Dropping '%s' (ΔELPD = %.3f, SE = %.3f)",
                    best$term, best$elpd_diff, best$se_diff))
    current_fit <- trials[[which(remaining == best$term)]]$fit_new
  } else {
    message("No further harmless drops; stopping.")
    break
  }
}

final_fit  <- current_fit
final_path <- transform(path, step = seq_len(nrow(path)))
message("\n=== DROP PATH (interactions) ==="); print(final_path, row.names = FALSE)

post_auto_interaction_fit <- brm(
  extension ~ StavR_zg + WtavR_zg + WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c + gulf + Bheight_c + (1 | colony/year) 
  + winSlope_c:gulf + WcsavR_c:gulf + StsdR_c:Bheight_c + winSlope_c:Bheight_c + WtavR_zg:gulf,
  data   = testex,
  family = Gamma(link = "log"),
  prior  = priors,
  chains = 3, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.98),
  save_pars = save_pars(all = TRUE)
)

## looking at furtehr diagnotics for term dropping - using ROPE to drop the weakest interactions in term of adding to prediction (values >0.1)
# Current model you are pruning (whichever object you're using right now):
fit_now <- post_auto_interaction_fit  # or final_fit if the loop ended

#run useful functions script  - for interaction_dropcheck() function
interaction_dropcheck(
  fit   = post_auto_interaction_fit,
  data  = testex,
  chains = 8,                 # or 6/10 depending on cores you want per model
  cores  = 8,                 # parallel chains for each reduced refit
  loo_cores = 8,              # parallel PSIS-LOO
  mm = TRUE, reloo = FALSE
)

### trying to reduce interactions further: dropping interaction suggested by interaction_dropcheck() function 
### (most obvious first from prob_gt0, prob_lt0, rope_mass & ) - StsdR_c:Bheight_c first

funal_fit1 <- brm(
  extension ~ StavR_zg + WtavR_zg + WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c + gulf + Bheight_c + (1 | colony/year) 
  + winSlope_c:gulf + WcsavR_c:gulf + winSlope_c:Bheight_c + WtavR_zg:gulf,
  data   = testex,
  family = Gamma(link = "log"),
  prior  = priors,
  chains = 3, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.98),
  save_pars = save_pars(all = TRUE)
)

interaction_dropcheck(
  fit   = funal_fit1,
  data  = testex,
  chains = 8,                 # or 6/10 depending on cores you want per model
  cores  = 8,                 # parallel chains for each reduced refit
  loo_cores = 8,              # parallel PSIS-LOO
  mm = TRUE, reloo = FALSE
)
library(performance)
r2_bayes(post_auto_interaction_fit) # from loo model reduction
r2_bayes(funal_fit1) # from above dropped term on rope_mass / prob_gt0 / prob_lt0

### trying to reduce interactions further: dropping interaction suggested by interaction_dropcheck() function 
### (most obvious first from prob_gt0, prob_lt0, rope_mass & ) - winSlope_c:gulfoman

funal_fit2 <- brm(
  extension ~ StavR_zg + WtavR_zg + WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c + gulf + Bheight_c + (1 | colony/year) 
  +  WcsavR_c:gulf + winSlope_c:Bheight_c + WtavR_zg:gulf,
  data   = testex,
  family = Gamma(link = "log"),
  prior  = priors,
  chains = 3, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.98),
  save_pars = save_pars(all = TRUE)
)

interaction_dropcheck(
  fit   = funal_fit2,
  data  = testex,
  chains = 8,                 # or 6/10 depending on cores you want per model
  cores  = 8,                 # parallel chains for each reduced refit
  loo_cores = 8,              # parallel PSIS-LOO
  mm = TRUE, reloo = FALSE
)
library(performance)
r2_bayes(post_auto_interaction_fit) # from loo model reduction
r2_bayes(funal_fit1)
r2_bayes(funal_fit2)


### trying to reduce interactions further: dropping interaction suggested by interaction_dropcheck() function 
### (most obvious first from prob_gt0, prob_lt0, rope_mass & ) - WcsavR_c:gulfoman

funal_fit3 <- brm(
  extension ~ StavR_zg + WtavR_zg + WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c + gulf + Bheight_c + (1 | colony/year) 
  +  winSlope_c:Bheight_c + WtavR_zg:gulf,
  data   = testex,
  family = Gamma(link = "log"),
  prior  = priors,
  chains = 3, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.98),
  save_pars = save_pars(all = TRUE)
)

interaction_dropcheck(
  fit   = funal_fit3,
  data  = testex,
  chains = 8,                 # or 6/10 depending on cores you want per model
  cores  = 8,                 # parallel chains for each reduced refit
  loo_cores = 8,              # parallel PSIS-LOO
  mm = TRUE, reloo = FALSE
)
library(performance)
r2_bayes(post_auto_interaction_fit) # from loo model reduction
r2_bayes(funal_fit1)
r2_bayes(funal_fit2)
r2_bayes(funal_fit3)

#################################################
## Ok looking at final fit using these results before moving onto dropping unnecessary main effects
final_interaction  <- funal_fit3

# ===================== FAST BACKWARD: MAIN EFFECTS =====================

# ===================== FAST BACKWARD: MAIN EFFECTS =====================
## ===================== BACKWARD: MAIN EFFECTS (marginality enforced) =====================

library(brms)
library(loo)

# --- helpers ---
fast_add_loo <- function(fit, mm = TRUE, reloo = TRUE) {
  if (is.null(fit$criteria$loo)) {
    fit <- add_criterion(fit, "loo", moment_match = mm, reloo = reloo)
  } else {
    # refresh in case earlier run used different settings
    fit$criteria$loo <- NULL
    fit <- add_criterion(fit, "loo", moment_match = mm, reloo = reloo)
  }
  fit
}

# correct ΔELPD(new − base) extraction irrespective of which row is "best"
loo_diff_new_minus_base <- function(loo_new, loo_base) {
  cmp <- loo_compare(list(new = loo_new, base = loo_base))
  if (rownames(cmp)[1] == "new") {
    # new is reference; take base row and flip sign
    elpd_diff <- -as.numeric(cmp["base", "elpd_diff"])
    se_diff   <-  as.numeric(cmp["base", "se_diff"])
  } else {
    # base is reference; take new row directly
    elpd_diff <-  as.numeric(cmp["new", "elpd_diff"])
    se_diff   <-  as.numeric(cmp["new", "se_diff"])
  }
  c(elpd_diff = elpd_diff, se_diff = se_diff)
}

drop_once <- function(fit_full, term,
                      data = testex, prior = priors,
                      iter = 5000, warmup = 3000, chains = 3,
                      control = list(adapt_delta = 0.98)) {
  
  f_old <- formula(fit_full)
  f_new <- update(f_old, paste(". ~ . -", term))
  
  # if the update didn’t change the formula, bail out
  if (identical(deparse(f_old), deparse(f_new))) {
    return(list(elpd_diff = NA_real_, se_diff = NA_real_, fit_new = fit_full))
  }
  
  fit_red <- brm(
    formula = f_new, data = data,
    family = Gamma(link = "log"), prior = prior,
    chains = chains, iter = iter, warmup = warmup,
    control = control, save_pars = save_pars(all = TRUE), silent = 2
  )
  
  fit_full <- fast_add_loo(fit_full, mm = TRUE, reloo = FALSE)
  fit_red  <- fast_add_loo(fit_red,  mm = TRUE, reloo = FALSE)
  
  diffs <- loo_diff_new_minus_base(fit_red$criteria$loo, fit_full$criteria$loo)
  list(elpd_diff = diffs["elpd_diff"], se_diff = diffs["se_diff"], fit_new = fit_red)
}

# labels from fixef (no Intercept)
get_fe_labels <- function(fit) {
  labs <- rownames(fixef(fit))
  labs[labs != "Intercept"]
}

# does main appear in any interaction currently in the model?
main_in_any_interaction <- function(main, fe_labels) {
  ints <- fe_labels[grepl(":", fe_labels, fixed = TRUE)]
  if (!length(ints)) return(FALSE)
  any(grepl(paste0("(^|:)", main, "(:|$)"), ints))
}

# --- config ---
threshold_SE <- 1.0  # drop if ΔELPD ≥ -1 * SE

# >>> starting model <<<
base_fit <- final_interaction

# your main-effect candidates (do NOT include 'gulf' or any term that appears in interactions)
cand_terms <- c("StavR_zg","WtsdR_c","StsdR_c","sumSlope_c","WcsavR_c")

# --- backward elimination (MAIN EFFECTS ONLY) ---
path_main <- data.frame(step = integer(), dropped = character(),
                        elpd_diff = double(), se_diff = double())

current_fit <- base_fit
step <- 0

repeat {
  fe_labels <- get_fe_labels(current_fit)
  mains_now <- fe_labels[!grepl(":", fe_labels, fixed = TRUE)]
  remaining <- intersect(cand_terms, mains_now)
  if (!length(remaining)) { message("No main-effect candidates left."); break }
  
  # enforce marginality
  allowed <- remaining[ !vapply(remaining, main_in_any_interaction, logical(1), fe_labels) ]
  if (!length(allowed)) {
    message("No main effects can be dropped without violating marginality. Stopping.")
    break
  }
  
  trials <- lapply(allowed, function(t) drop_once(current_fit, t))
  tab <- do.call(rbind, Map(function(t, res)
    data.frame(term = t, elpd_diff = as.numeric(res$elpd_diff), se_diff = as.numeric(res$se_diff)),
    allowed, trials))
  
  tab <- subset(tab, is.finite(elpd_diff) & is.finite(se_diff))
  if (!nrow(tab)) { message("No valid main-effect trials; stopping."); break }
  
  tab <- tab[order(-tab$elpd_diff), ]
  print(tab, row.names = FALSE)
  
  best <- tab[1, ]
  if (best$elpd_diff >= -threshold_SE * best$se_diff) {
    step <- step + 1
    path_main <- rbind(path_main, transform(best, step = step))
    message(sprintf("Dropping main '%s' (ΔELPD = %.3f, SE = %.3f)",
                    best$term, best$elpd_diff, best$se_diff))
    current_fit <- trials[[which(allowed == best$term)]]$fit_new
  } else {
    message("No further harmless main-effect drops under marginality; stopping.")
    break
  }
}

final_main <- current_fit
final_main <- add_criterion(final_main, "loo", moment_match = TRUE, reloo = FALSE)
message("\n=== DROP PATH (main effects) ==="); print(path_main, row.names = FALSE)
print(final_main$criteria$loo)

r2_bayes(post_auto_interaction_fit) # from loo model reduction
r2_bayes(funal_fit1)
r2_bayes(funal_fit2)
r2_bayes(funal_fit3)
r2_bayes(final_main)
final_main

#### Some plots - Marginal Effects

## 1) Slope × Body height interaction
ce1 <- conditional_effects(
  final_main,
  effects = "winSlope_c:Bheight_c",
  re_formula = NA,        # population-level (no random effects)
  method = "fitted"
)
plot(ce1, points = TRUE)

## 2) WtavR_zg × Gulf interaction
ce2 <- conditional_effects(
  final_main,
  effects = "WtavR_zg:gulf",
  re_formula = NA,
  method = "fitted"
)
plot(ce2, points = FALSE)

### Further plots on the original scale - ALL data population level

library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)

# 1) Per-gulf summary: mean, sd, min, max (force dplyr versions)
stats_WtavR <- testex %>%
  dplyr::mutate(gulf = as.character(gulf)) %>%
  dplyr::group_by(gulf) %>%
  dplyr::summarise(
    mean_W = mean(WtavR, na.rm = TRUE),
    sd_W   = sd(WtavR,   na.rm = TRUE),
    min_W  = min(WtavR,  na.rm = TRUE),
    max_W  = max(WtavR,  na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

# 2) Build a common fine grid, then clip per-gulf to observed ranges
gulf_levels <- sort(unique(testex$gulf))
new_WtavR <- seq(min(testex$WtavR, na.rm = TRUE),
                 max(testex$WtavR, na.rm = TRUE),
                 length.out = 200)

nd <- tidyr::crossing(
  WtavR = new_WtavR,
  gulf  = gulf_levels
) %>%
  dplyr::left_join(stats_WtavR, by = "gulf") %>%
  dplyr::filter(WtavR >= min_W, WtavR <= max_W) %>%
  dplyr::mutate(
    WtavR_zg  = (WtavR - mean_W) / sd_W,
    winSlope_c = 0,
    Bheight_c  = 0
  )

# 3) Predict (population-level)
nd_pred <- nd %>%
  tidybayes::add_epred_draws(final_main, re_formula = NA)

## The plot
library(ggplot2)
library(tidybayes)
library(dplyr)

ggplot2::ggplot() +
  # 1️⃣ Raw data (semi-transparent points, coloured by group)
  geom_point(
    data = testex,
    aes(x = WtavR, y = extension, color = gulf),
    alpha = 0.10, size = 1.5
  ) +
  
  # 2️⃣ Model predictions with credible intervals
  tidybayes::stat_lineribbon(
    data = nd_pred,
    aes(x = WtavR, y = .epred, color = gulf, fill = gulf),
    .width    = c(0.95, 0.66),
    .point    = "mean",      # single best-fit line
    alpha     = 0.20,        # transparency of ribbons
    linewidth = 1.1
  ) +
  
  # 3️⃣ Labels and theme
  labs(
    x = "Mean temperature (°C) in coldest 61-day period per year",
    y = expression(paste("Predicted extension (cm ", yr^{-1}, ")")),
    color = "Gulf group",
    fill  = "Gulf group",
    title = "Interaction: WtavR × gulf (original WtavR scale)"
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

##### And for winter slope and colony size:

library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)

## --- Settings / summaries ---
gulf_ref <- "oman"  # change to "gulf" if you prefer that group

mu_winSlope <- mean(testex$winSlope, na.rm = TRUE)
mu_Bheight  <- mean(testex$Bheight,  na.rm = TRUE)

# Quantiles for Bheight (representative line levels)
bh_q <- quantile(testex$Bheight, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
q10 <- as.numeric(bh_q[1]); q50 <- as.numeric(bh_q[2]); q90 <- as.numeric(bh_q[3])

# Midpoints for non-overlapping bands
b1 <- (q10 + q50) / 2
b2 <- (q50 + q90) / 2

# Assign observations to bands
test_band <- testex %>%
  dplyr::filter(gulf == gulf_ref) %>%
  dplyr::mutate(
    Bheight_band = cut(
      Bheight,
      breaks = c(-Inf, b1, b2, Inf),
      labels = c("Bheight: 10th pct", "Bheight: 50th pct", "Bheight: 90th pct"),
      right  = TRUE
    )
  ) %>%
  dplyr::filter(!is.na(Bheight_band))

# Observed winSlope ranges per band
band_ranges <- test_band %>%
  dplyr::group_by(Bheight_band) %>%
  dplyr::summarise(
    min_win = min(winSlope, na.rm = TRUE),
    max_win = max(winSlope, na.rm = TRUE),
    .groups = "drop"
  )

# Representative Bheight values for the lines
bh_rep <- dplyr::tibble(
  Bheight_band = factor(
    c("Bheight: 10th pct","Bheight: 50th pct","Bheight: 90th pct"),
    levels = c("Bheight: 10th pct","Bheight: 50th pct","Bheight: 90th pct")
  ),
  Bheight = c(q10, q50, q90)
)

# Band-specific prediction grids (only within observed winSlope ranges)
nd2 <- band_ranges %>%
  dplyr::left_join(bh_rep, by = "Bheight_band") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(winSlope = list(seq(min_win, max_win, length.out = 200))) %>%
  tidyr::unnest(winSlope) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    winSlope_c = winSlope - mu_winSlope,
    Bheight_c  = Bheight  - mu_Bheight,
    gulf       = gulf_ref,
    WtavR_zg   = 0
  )

# Posterior expected means (population-level)
nd2_pred <- nd2 %>%
  tidybayes::add_epred_draws(final_main, re_formula = NA)

## --- Plot (same style as WtavR × gulf) ---
ggplot() +
  # Raw data points (transparent)
  ggplot2::geom_point(
    data = test_band,
    ggplot2::aes(x = winSlope, y = extension, color = Bheight_band),
    alpha = 0.10, size = 1.5
  ) +
  # Model predictions (no extrapolation)
  tidybayes::stat_lineribbon(
    data = nd2_pred,
    ggplot2::aes(x = winSlope, y = .epred, color = Bheight_band, fill = Bheight_band),
    .width    = c(0.95, 0.66),
    .point    = "mean",
    alpha     = 0.20,
    linewidth = 1.1
  ) +
  # Labels and theme
  ggplot2::labs(
    x = expression(paste("Winter slope (°C ", Day^{-1}, ")")),
    y = expression(paste("Predicted extension (cm ", yr^{-1}, ")")),
    color = "Branch height",
    fill  = "Branch height",
    title = "Interaction: winter slope × branch height (original scales; no extrapolation)"
  ) +
  ggplot2::theme_classic(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.box.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
  )
## Some model diagnostics to check that the model assumptions are validated etc.
## Use Model-diagnostics.R script and make sure it is the correct model on line 53!

##### Lets do some variance decomposition to see which model terms explain the most variation
# ============================================================
# Variance decomposition of conditional R² for brms model: final_main
# - Uses fixef() draws + your data (no update/compile)
# - DEBUG mode prints shapes/names at each step
# - Fixed mains & interactions apportioned -> Marginal R²
# - Random effects split -> (Conditional R² - Marginal R²)
# - Saves a PDF barplot
# ============================================================

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(posterior)
  library(ggplot2)
})

# -----------------------------
# CONFIG
# -----------------------------
DEBUG <- TRUE
R2_marg <- 0.090   # your marginal R² (fixed)
R2_cond <- 0.465   # your conditional R² (fixed + random)

if (!inherits(final_main, "brmsfit")) stop("Object 'final_main' is not a brmsfit.")
dat <- final_main$data
if (!is.data.frame(dat)) stop("final_main$data is not a data.frame.")

R2_rand <- max(0, R2_cond - R2_marg)

# Helper for debug printing
dlog <- function(...) if (isTRUE(DEBUG)) cat("[DEBUG]", ..., "\n")

dlog("nrow(dat) =", nrow(dat), "; ncol(dat) =", ncol(dat))

# -----------------------------
# 1) Fixed-effect draws and matching design matrix X
# -----------------------------
B <- brms::fixef(final_main, summary = FALSE)
if (is.null(B)) stop("fixef(final_main, summary = FALSE) returned NULL.")
B <- as.matrix(B)
coef_names <- colnames(B)
if (is.null(coef_names)) stop("No column names on fixef() draws.")

dlog("fixef draws dims:", paste(dim(B), collapse = " x "))
dlog("First 10 coef_names:", paste(utils::head(coef_names, 10), collapse = ", "))

# Treat character columns as categorical for dummy matching
is_cat <- vapply(dat, function(x) is.factor(x) || is.character(x), logical(1))
cat_vars <- names(dat)[is_cat]
dlog("Categorical vars detected:", if (length(cat_vars)) paste(cat_vars, collapse = ", ") else "<none>")

# Build one column per coefficient name using *actual* data
build_col_for_term <- function(term) {
  if (term == "Intercept") return(rep(1, nrow(dat)))
  
  make_base <- function(atom) {
    # 1) exact numeric covariate present
    if (atom %in% names(dat) && !(is_cat[atom])) return(as.numeric(dat[[atom]]))
    
    # 2) factor/character dummy like 'gulfoman' => var = 'gulf', level = 'oman'
    for (fv in cat_vars) {
      if (startsWith(atom, fv)) {
        lev <- sub(paste0("^", fv), "", atom)
        if (lev == "") break
        x <- dat[[fv]]
        if (is.character(x)) {
          return(as.numeric(x == lev))
        } else if (is.factor(x)) {
          if (!lev %in% levels(x)) {
            stop("Factor level '", lev, "' not found in factor ", fv,
                 ". Levels available: {", paste(levels(x), collapse = ", "), "}")
          }
          return(as.numeric(x == lev))
        }
      }
    }
    
    # 3) fallback: still in data (numeric or coerced)
    if (atom %in% names(dat)) return(as.numeric(dat[[atom]]))
    
    stop("Could not construct column for coefficient atom: '", atom,
         "'. Check that your data has this variable/level.")
  }
  
  if (grepl(":", term, fixed = TRUE)) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    cols <- lapply(parts, make_base)
    out <- Reduce(`*`, cols)
  } else {
    out <- make_base(term)
  }
  
  if (length(out) != nrow(dat)) {
    stop("Constructed column for term '", term, "' has length ", length(out),
         " but nrow(dat) is ", nrow(dat))
  }
  out
}

dlog("Building design matrix X from coefficient names...")
X <- tryCatch(
  vapply(coef_names, build_col_for_term, numeric(nrow(dat))),
  error = function(e) {
    cat("\n[DEBUG] Failed while building X. Offending message:\n", conditionMessage(e), "\n", sep = "")
    cat("[DEBUG] Example coef_names:", paste(utils::head(coef_names, 15), collapse = ", "), "\n")
    stop(e)
  }
)
X <- as.matrix(X)               # [n_obs x n_coefs]
colnames(X) <- coef_names
dlog("X dims:", paste(dim(X), collapse = " x "))

# -----------------------------
# 2) Eta draws helper (subset-multiply, NO in-place assignment)
# -----------------------------
eta_from_cols <- function(cols_to_keep) {
  idx <- which(coef_names %in% c("Intercept", cols_to_keep))
  if (length(idx) == 0) stop("eta_from_cols: empty idx — nothing to keep.")
  Bk <- B[, idx, drop = FALSE]      # [draws x k]
  Xk <- X[, idx, drop = FALSE]      # [obs x k]
  if (ncol(Bk) != ncol(Xk)) {
    stop("eta_from_cols: column mismatch Bk(", ncol(Bk), ") vs Xk(", ncol(Xk), ").")
  }
  # Multiply: [draws x k] %*% [k x obs] -> [draws x obs]
  Bk %*% t(Xk)
}

# Full fixed-effects eta and between-observation variance per draw
dlog("Computing full fixed-effects eta...")
eta_full <- eta_from_cols(coef_names)
if (!all(is.finite(eta_full))) {
  dlog("Non-finite values in eta_full; filtering rows with non-finite variances.")
}
var_full_draw <- apply(eta_full, 1, function(v) stats::var(v[is.finite(v)]))
var_full_draw <- var_full_draw[is.finite(var_full_draw)]
if (length(var_full_draw) == 0) stop("No finite variances in full fixed-effects predictions.")
dlog("var_full_draw length:", length(var_full_draw), "; mean:", mean(var_full_draw))

# -----------------------------
# 3) Conceptual terms to report
# -----------------------------
main_terms <- c("WtavR_zg", "winSlope_c", "gulf", "Bheight_c")
int_terms  <- c("winSlope_c:Bheight_c", "WtavR_zg:gulf")
analysis_terms <- c(main_terms, int_terms)
dlog("Analysis terms:", paste(analysis_terms, collapse = ", "))

# Map conceptual term -> coefficient columns to drop
map_term_to_coefs <- function(term) {
  if (term == "gulf") {
    gulf_cols <- grep("^gulf", coef_names, value = TRUE)
    if (!length(gulf_cols)) {
      dlog("No gulf dummy columns found in coef_names.")
      return(character(0))
    }
    int_with_gulf <- grep(paste0("(^|:)", paste(gulf_cols, collapse = "|"), "(:|$)"),
                          coef_names, value = TRUE)
    unique(c(gulf_cols, int_with_gulf))
  } else if (grepl(":", term, fixed = TRUE)) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    parts_rx <- sapply(parts, function(p) if (p == "gulf") "gulf[^:]+" else p)
    rx <- paste0("(^|:)", parts_rx[1], ":", parts_rx[2], "(:|$)|(^|:)",
                 parts_rx[2], ":", parts_rx[1], "(:|$)")
    grep(rx, coef_names, value = TRUE, perl = TRUE)
  } else {
    rx <- paste0("(^|:)", term, "(:|$)")
    grep(rx, coef_names, value = TRUE, perl = TRUE)
  }
}

drop_sets <- lapply(analysis_terms, map_term_to_coefs)
names(drop_sets) <- analysis_terms
if (DEBUG) {
  cat("[DEBUG] Drop sets:\n")
  for (nm in names(drop_sets)) cat(" -", nm, "->", if (length(drop_sets[[nm]])) paste(drop_sets[[nm]], collapse = ", ") else "<none>", "\n")
}

# -----------------------------
# 4) Fixed-effect contributions (LOO on η)
# -----------------------------
fixed_contrib <- lapply(seq_along(analysis_terms), function(i) {
  term <- analysis_terms[i]
  drop_cols <- unique(drop_sets[[i]])
  keep_cols <- setdiff(coef_names, drop_cols)
  
  if (DEBUG) {
    cat("\n[DEBUG] Term:", term,
        "\n  drop_cols:", if (length(drop_cols)) paste(drop_cols, collapse = ", ") else "<none>",
        "\n  keep_cols length:", length(keep_cols), "\n", sep = "")
  }
  
  eta_keep <- eta_from_cols(keep_cols)
  var_keep_draw <- apply(eta_keep, 1, function(v) stats::var(v[is.finite(v)]))
  var_keep_draw <- var_keep_draw[is.finite(var_keep_draw)]
  
  # Align lengths if any rows were filtered
  L <- min(length(var_full_draw), length(var_keep_draw))
  if (L == 0) stop("No finite variances for term '", term, "'.")
  contrib_draw <- pmax(0, var_full_draw[seq_len(L)] - var_keep_draw[seq_len(L)])
  
  data.frame(
    Term = term,
    Var_contrib_mean = mean(contrib_draw),
    Var_contrib_sd   = sd(contrib_draw),
    n_draws          = length(contrib_draw)
  )
}) %>% bind_rows()

sum_fixed <- sum(fixed_contrib$Var_contrib_mean)
if (!is.finite(sum_fixed) || sum_fixed <= 0) {
  dlog("fixed_contrib table:\n"); print(fixed_contrib)
  stop("Fixed-effect contributions summed to zero; check mapping of terms to coefficients and coef_names.")
}

fixed_contrib <- fixed_contrib %>%
  mutate(
    within_fixed_share = Var_contrib_mean / sum_fixed,
    within_fixed_se    = (Var_contrib_sd / sqrt(pmax(1, n_draws))) / sum_fixed,
    R2_share           = within_fixed_share * R2_marg,
    Type               = "Fixed"
  ) %>%
  select(Term, Type, R2_share, within_fixed_share, within_fixed_se)

# =======================
# 4b) Credible intervals
# =======================

# Re-run step 4 but store per-draw contributions for each term (not just means)
# We'll reuse var_full_draw computed earlier.
fixed_contrib_draws <- list()
term_order <- analysis_terms

for (i in seq_along(analysis_terms)) {
  term <- analysis_terms[i]
  drop_cols <- unique(drop_sets[[i]])
  keep_cols <- setdiff(coef_names, drop_cols)
  eta_keep <- eta_from_cols(keep_cols)
  var_keep_draw <- apply(eta_keep, 1, function(v) stats::var(v[is.finite(v)]))
  var_keep_draw <- var_keep_draw[is.finite(var_keep_draw)]
  L <- min(length(var_full_draw), length(var_keep_draw))
  contrib_draw <- pmax(0, var_full_draw[seq_len(L)] - var_keep_draw[seq_len(L)])
  fixed_contrib_draws[[term]] <- contrib_draw
}

# Align all fixed-term vectors to same length
L_fixed <- min(vapply(fixed_contrib_draws, length, 1L))
fixed_mat <- sapply(fixed_contrib_draws, function(v) v[seq_len(L_fixed)])  # [draw x term]

# Per-draw normalization within fixed effects (row sums)
row_sums <- rowSums(fixed_mat)
# guard against zeros
row_sums[row_sums == 0] <- NA_real_
fixed_share_draws <- fixed_mat / row_sums  # each row sums to 1 (possibly NA if degenerate)

# Scale by marginal R^2
fixed_R2_draws <- fixed_share_draws * R2_marg  # [draw x term]

# Summarise: median and 95% CI
quant <- function(x) stats::quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
fixed_ci <- apply(fixed_R2_draws, 2, quant)
fixed_ci_df <- data.frame(
  Term   = colnames(fixed_R2_draws),
  Type   = "Fixed",
  R2_low = fixed_ci[1, ],
  R2_med = fixed_ci[2, ],
  R2_hi  = fixed_ci[3, ],
  row.names = NULL
)

# ============================
# 5b) Random-effects intervals
# ============================
draws_df <- as_draws_df(final_main)
sd_names <- grep("^sd_", colnames(draws_df), value = TRUE)

nm_colony      <- grep("^sd_colony__Intercept$", sd_names, value = TRUE)
nm_colony_year <- grep("^sd_colony[\\.:]year__Intercept$", sd_names, value = TRUE, perl = TRUE)

if (length(nm_colony) == 1L && length(nm_colony_year) == 1L) {
  v_colony_draw      <- as.numeric(draws_df[[nm_colony]])^2
  v_colony_year_draw <- as.numeric(draws_df[[nm_colony_year]])^2
  vsum <- v_colony_draw + v_colony_year_draw
  # Avoid division by zero
  vsum[vsum == 0] <- NA_real_
  
  rand_share_colony      <- v_colony_draw      / vsum
  rand_share_colony_year <- v_colony_year_draw / vsum
  
  rand_R2_colony      <- rand_share_colony      * (R2_cond - R2_marg)
  rand_R2_colony_year <- rand_share_colony_year * (R2_cond - R2_marg)
  
  rc <- quant(rand_R2_colony)
  rcy <- quant(rand_R2_colony_year)
  
  rand_ci_df <- rbind(
    data.frame(Term = "RE: colony (Intercept)",      Type = "Random",
               R2_low = rc[1], R2_med = rc[2], R2_hi = rc[3]),
    data.frame(Term = "RE: colony:year (Intercept)", Type = "Random",
               R2_low = rcy[1], R2_med = rcy[2], R2_hi = rcy[3])
  )
} else {
  # Combined random-effects slice
  R2_rand <- max(0, R2_cond - R2_marg)
  rand_ci_df <- data.frame(
    Term = "Random effects (combined)", Type = "Random",
    R2_low = R2_rand, R2_med = R2_rand, R2_hi = R2_rand
  )
}

# =========================
# 6b) Combine + plot with CI
# =========================
final_ci_tbl <- bind_rows(fixed_ci_df, rand_ci_df) %>%
  arrange(desc(R2_med))

print(final_ci_tbl, row.names = FALSE)
cat("Check (medians) sum to ~conditional R²:",
    sum(final_ci_tbl$R2_med), "target =", R2_cond, "\n")

# Plot with CI (median bar + 95% CI errorbars)
pdf("variance_decomposition_R2_with_CI.pdf", width = 7, height = 4.8, useDingbats = FALSE)
on.exit(dev.off(), add = TRUE)
ggplot(final_ci_tbl,
       aes(x = reorder(Term, R2_med), y = R2_med, fill = Type)) +
  geom_col() +
  geom_errorbar(aes(ymin = R2_low, ymax = R2_hi), width = 0.25) +
  coord_flip() +
  labs(x = NULL, y = "Share of total explained variance (R²)",
       title = "Variance decomposition of conditional R² (medians with 95% CI)") +
  theme_minimal(base_size = 11) +
  guides(fill = "none")
dev.off()
cat("[DEBUG] Saved: variance_decomposition_R2_with_CI.pdf\n")




### Some contour surface plots

library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(plotly)

# Choose one gulf to visualize (since your model includes random intercepts by colony/year)
gulf_ref <- "oman"

# Means for centering
mu_winSlope <- mean(testex$winSlope, na.rm = TRUE)
mu_Bheight  <- mean(testex$Bheight,  na.rm = TRUE)

# Regular grid (original data scale)
new_winSlope <- seq(min(testex$winSlope), max(testex$winSlope), length.out = 60)
new_Bheight  <- seq(min(testex$Bheight),  max(testex$Bheight),  length.out = 60)

grid3D <- tidyr::crossing(
  winSlope = new_winSlope,
  Bheight  = new_Bheight
) %>%
  dplyr::mutate(
    winSlope_c = winSlope - mu_winSlope,
    Bheight_c  = Bheight  - mu_Bheight,
    WtavR_zg   = 0,
    gulf       = gulf_ref
  )

# Predict (population-level)
grid_pred <- grid3D %>%
  tidybayes::add_epred_draws(final_main, re_formula = NA) %>%
  dplyr::group_by(winSlope, Bheight) %>%
  dplyr::summarise(pred = mean(.epred), .groups = "drop")

# Convert to matrix for 3D surface plotting
zmat <- matrix(grid_pred$pred, nrow = length(new_winSlope), ncol = length(new_Bheight))

## --- 1️⃣ 3D surface (interactive) ---
plotly::plot_ly(
  x = ~new_winSlope, y = ~new_Bheight, z = ~zmat,
  type = "surface",
  colorscale = "Viridis"
) %>%
  plotly::layout(
    title = "Predicted extension surface (winter slope × branch height)",
    scene = list(
      xaxis = list(title = "Winter slope (°C yr⁻¹)"),
      yaxis = list(title = "Colony height (cm)"),
      zaxis = list(title = "Predicted extension (cm yr⁻¹)")
    )
  )

r2_bayes(final_main)
final_main


