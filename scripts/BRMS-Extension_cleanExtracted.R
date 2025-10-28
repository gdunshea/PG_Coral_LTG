# Packages required
req_pkgs <- c(
 "brms", # Bayesian models
 "performance", # r2_bayes()
 "loo", # loo()
 "bayesplot", # pp_check() backend used by brms
 "dplyr", # data wrangling (across, where, any_of)
 "ggplot2", # plotting
 "gridExtra", # arranging ggplots
 "posterior" # as_draws_df, draws handling
)

# Install any that are missing
to_install <- req_pkgs[!req_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) 
 install.packages(to_install, dependencies = TRUE)


# Load them all
invisible(lapply(req_pkgs, library, character.only = TRUE))


## There are some structural issues with this data that may cause problems with BRMS modelling. For example, one is the 
## non-overlapping winter and summer temperature between oman and gulf. What we are ultimately interested in is using the entire
## dataset to determine how the corals inside/outside the persion gulf respond to environmental drivers and whether this differs.
## That is, whether colonies inside vs. outside the Gulf respond differently to environmental variation, and what that difference looks like.

## In relation to non-overlapping temperatures inside/outside the gulf, we will standardize data by within-gulf z-scores. By doing this and 
## and testing for interactions between z scores and gulf, we can examine within-gulf slopes (i.e. the effect of temperature on extension within 
## each gulf92s observed range (no extrapolation) and interaction terms: how those slopes differ between gulfs.

## We will also centre the other relevant variables for ease of model fitting and simple interpretation of the final model:

##load extension data (if required)
testex <- read.csv("processeddata/testex.csv")
str(testex)
library(dplyr)

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


### Getting optimal random effects structure
## First examining the differences between gaussian and gamma error distributions on saturated models 

# GAUSSIAN
ex_mbrms3dwHW_1full_gauss <- brm(
 extension ~ StavR_zg + WtavR_zg +
 WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +
 gulf + Bheight_c +
 StavR_zg:Bheight_c + WtavR_zg:Bheight_c +
 winSlope_c:Bheight_c + StsdR_c:Bheight_c + sumSlope_c:Bheight_c +
 StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf +
 winSlope_c:gulf + StsdR_c:gulf + sumSlope_c:gulf + WcsavR_c:gulf +
 Bheight_c:gulf +
 (1 + Bheight_c | colony/year),
 data = testex,
 family = gaussian(),
 chains = 3, iter = 3000, warmup = 1000,
 control = list(adapt_delta = 0.95),
 save_pars = save_pars(all = TRUE)
)

# GAMMA (log link)
ex_mbrms3dwHW_1full_gamma <- brm(
 extension ~ StavR_zg + WtavR_zg +
 WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +
 gulf + Bheight_c +
 StavR_zg:Bheight_c + WtavR_zg:Bheight_c +
 winSlope_c:Bheight_c + StsdR_c:Bheight_c + sumSlope_c:Bheight_c +
 StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf +
 winSlope_c:gulf + StsdR_c:gulf + sumSlope_c:gulf + WcsavR_c:gulf +
 Bheight_c:gulf +
 (1 + Bheight_c | colony/year),
 data = testex,
 family = Gamma(link = "log"),
 chains = 3, iter = 5000, warmup = 3000,
 control = list(adapt_delta = 0.95),
 save_pars = save_pars(all = TRUE)
)

## Backward selection of full random effects
r2_bayes(ex_mbrms3dwHW_1full_gauss)
r2_bayes(ex_mbrms3dwHW_1full_gamma)
pp_check(ex_mbrms3dwHW_1full_gauss)
pp_check(ex_mbrms3dwHW_1full_gamma)


loo(ex_mbrms3dwHW_1full_gauss, ex_mbrms3dwHW_1full_gamma, cores = getOption("mc.cores", 1))


#### Going from here looking at the random structure

# (1 | colony/year)
ex_mbrms3dwHW_1full_1 <- brms::brm(
  extension ~ StavR_zg + WtavR_zg +
    WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +
    gulf + Bheight_c +
    StavR_zg:Bheight_c + WtavR_zg:Bheight_c +
    winSlope_c:Bheight_c + StsdR_c:Bheight_c + sumSlope_c:Bheight_c +
    StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf +
    winSlope_c:gulf + StsdR_c:gulf + sumSlope_c:gulf + WcsavR_c:gulf +
    Bheight_c:gulf +
    (1 | colony/year),
  data = testex, family = Gamma(link = "log"),
  chains = 3, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

# (1 | colony)
ex_mbrms3dwHW_1full_2 <- brms::brm(
  extension ~ StavR_zg + WtavR_zg +
    WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +
    gulf + Bheight_c +
    StavR_zg:Bheight_c + WtavR_zg:Bheight_c +
    winSlope_c:Bheight_c + StsdR_c:Bheight_c + sumSlope_c:Bheight_c +
    StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf +
    winSlope_c:gulf + StsdR_c:gulf + sumSlope_c:gulf + WcsavR_c:gulf +
    Bheight_c:gulf +
    (1 | colony),
  data = testex, family = Gamma(link = "log"),
  chains = 3, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

# (1 + Bheight_c | colony)
ex_mbrms3dwHW_1full_3 <- brms::brm(
  extension ~ StavR_zg + WtavR_zg +
    WtsdR_c + winSlope_c + StsdR_c + sumSlope_c + WcsavR_c +
    gulf + Bheight_c +
    StavR_zg:Bheight_c + WtavR_zg:Bheight_c +
    winSlope_c:Bheight_c + StsdR_c:Bheight_c + sumSlope_c:Bheight_c +
    StavR_zg:gulf + WtavR_zg:gulf + WtsdR_c:gulf +
    winSlope_c:gulf + StsdR_c:gulf + sumSlope_c:gulf + WcsavR_c:gulf +
    Bheight_c:gulf +
    (1 + Bheight_c | colony),
  data = testex, family = Gamma(link = "log"),
  chains = 3, iter = 5000, warmup = 3000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

loo(
  ex_mbrms3dwHW_1full_gauss,
  ex_mbrms3dwHW_1full_gamma,
  ex_mbrms3dwHW_1full_1,
  ex_mbrms3dwHW_1full_2,
  ex_mbrms3dwHW_1full_3,
  moment_match = TRUE, #reloo = TRUE,
  cores = getOption("mc.cores", 1)
)


## loo indicates ex_mbrms3dwHW_1full_1 performs as well as the random slope model
r2_bayes(final_fit)
r2_bayes(ex_mbrms3dwHW_1full_gamma)
r2_bayes(ex_mbrms3dwHW_1full_1)
r2_bayes(final_fit_main)
summary(ex_mbrms3dwHW_1full_1)

##now removing obviously irrelevant model terms (parameter close to zero, CIs overlapping zero well and truely)
ex_mbrms3dwHW_1full_11 <- brms::brm(extension~ StavR_centered+WtavR_centered+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ gulf+ Bheight+ 
 winSlope:Bheight+ StsdR:Bheight+ WtavR_centered:gulf+ 
 WcsavR:gulf+ Bheight:gulf+(1 |colony/year),
 data = testex, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.95),
 iter = 5000, warmup = 3000)
r2_bayes(ex_mbrms3dwHW_1full_1)
r2_bayes(ex_mbrms3dwHW_1full_11)
summary(ex_mbrms3dwHW_1full_11)

loo(ex_mbrms3dwHW_1full, ex_mbrms3dwHW_1fullgamma, ex_mbrms3dwHW_1full_1, 
 ex_mbrms3dwHW_1full_2, ex_mbrms3dwHW_1full_3, ex_mbrms3dwHW_1full_11,
 cores = getOption("mc.cores", 1))

ex_mbrms3dwHW_1full_111 <- brms::brm(extension~ WtavR_centered+ winSlope+ WcsavR+ gulf+ Bheight+ 
 winSlope:Bheight+ WtavR_centered:gulf+ 
 WcsavR:gulf+ Bheight:gulf+(1 |colony/year),
 data = testex, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.95),
 iter = 5000, warmup = 3000)
r2_bayes(ex_mbrms3dwHW_1full_11)
r2_bayes(ex_mbrms3dwHW_1full_111)
summary(ex_mbrms3dwHW_1full_111)

loo(ex_mbrms3dwHW_1full, ex_mbrms3dwHW_1fullgamma, ex_mbrms3dwHW_1full_1, 
 ex_mbrms3dwHW_1full_2, ex_mbrms3dwHW_1full_3, ex_mbrms3dwHW_1full_11, ex_mbrms3dwHW_1full_111,
 cores = getOption("mc.cores", 1))

ex_mbrms3dwHW_1full_1111 <- brms::brm(extension~ WtavR_centered+ winSlope+ WcsavR+ gulf+ Bheight+ 
 winSlope:Bheight+ WtavR_centered:gulf+ 
 WcsavR:gulf+ (1 |colony/year),
 data = testex, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.95),
 iter = 5000, warmup = 3000)
r2_bayes(ex_mbrms3dwHW_1full_111)
r2_bayes(ex_mbrms3dwHW_1full_1111)
summary(ex_mbrms3dwHW_1full_1111)

## to examine if there is within or between colony variation inside and outside the gulf specifiying the random effects a 
## little differently
ex_mbrms3dwHW_1full_1111_gulfre <- brm(
 extension ~ WtavR_centered + winSlope + WcsavR + gulf + Bheight + 
 winSlope:Bheight + WtavR_centered:gulf + WcsavR:gulf + 
 (1 | colony:gulf/year),
 data = testex,
 family = Gamma(link = "log"), # Correct specification
 chains = 3,
 iter = 5000,
 warmup = 3000,
 control = list(adapt_delta = 0.99)
)

ex_mbrms3dwHW_1full_1111_gulfinter <- brms::brm(
 extension ~ WtavR_centered + winSlope + WcsavR + gulf + Bheight + 
 winSlope:Bheight + WtavR_centered:gulf + WcsavR:gulf + 
 (1 | gulf/colony/year),
 data = testex, 
 family = Gamma(link = "log"), 
 chains = 3, 
 control = list(adapt_delta = 0.99),
 iter = 7000, 
 warmup = 2000
)

loo(ex_mbrms3dwHW_1full, ex_mbrms3dwHW_1fullgamma, ex_mbrms3dwHW_1full_1, ex_mbrms3dwHW_1full_1111_gulfre, ex_mbrms3dwHW_1full_1111_gulfinter,
 ex_mbrms3dwHW_1full_2, ex_mbrms3dwHW_1full_3, ex_mbrms3dwHW_1full_11, ex_mbrms3dwHW_1full_111,ex_mbrms3dwHW_1full_1111,
 cores = getOption("mc.cores", 1))

## Looks good with all parameters not overlapping zero - some diagnostics
pp_check(ex_mbrms3dwHW_1full_1111)
marginal_effects_plot <- marginal_effects(ex_mbrms3dwHW_1full_1111)
plot(marginal_effects_plot)

#### some surface plots - see functions
summary(ex_mbrms3dwHW_1full_1111)
psurface("Bheight", "WcsavR", data = testex, model = ex_mbrms3dwHW_1full_1111, 
 xlab = "Back Calc. Height (cm)", ylab = "Current Speed (m/s)")
psurface("Bheight", "WtavR_centered", data = testex, model = ex_mbrms3dwHW_1full_1111, 
 xlab = "Back Calc. Height (cm)", ylab = "Deviation from Mean Temp.")
psurface("Bheight", "winSlope", data = testex, model = ex_mbrms3dwHW_1full_1111, 
 xlab = "Back Calc. Height (cm)", ylab = "R.O.C degrees per day")



























## not strictly a bayesian thing but check residual plots anyway
# Extract aligned fitted values and residuals
aligned_fitted <- fitted(ex_mbrms3dwHW_1full_1111s1)[, "Estimate"]
aligned_residuals <- residuals(ex_mbrms3dwHW_1full_1111s1, summary = TRUE)[, "Estimate"]

# Create the residuals vs. fitted values plot
plot(aligned_fitted, aligned_residuals,
 xlab = "Fitted Values", ylab = "Residuals",
 main = "Residuals vs Fitted Values", pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)
lines(lowess(aligned_fitted, aligned_residuals), col = "darkgreen", lwd = 2)

### Finally, the lowess on the total residual plot is not completely ideal. Therefore seeing if changing the shape of gamma will help.

ex_mbrms3dwHW_1full_1111s <- brm(
 bf(extension ~ WtavR_centered + winSlope + WcsavR + gulf + Bheight +
 winSlope:Bheight + WtavR_centered:gulf + WcsavR:gulf + (1 | colony/year),
 shape ~ Site), # Model shape parameter for variance
 data = testex, family = Gamma(link = "log"),
 chains = 3, control = list(adapt_delta = 0.95), iter = 6000)

ex_mbrms3dwHW_1full_1111s1 <- brm(
 bf(extension ~ WtavR_centered + winSlope + WcsavR + gulf + Bheight +
 winSlope:Bheight + WtavR_centered:gulf + WcsavR:gulf + (1 | colony/year)), # Model shape parameter for variance
 data = testex, family = Gamma(link = "log"),
 chains = 3, control = list(adapt_delta = 0.95), iter = 6000)

loo(ex_mbrms3dwHW_1full, ex_mbrms3dwHW_1fullgamma, ex_mbrms3dwHW_1full_1, ex_mbrms3dwHW_1full_1111s,
 ex_mbrms3dwHW_1full_2, ex_mbrms3dwHW_1full_3, ex_mbrms3dwHW_1full_11, ex_mbrms3dwHW_1full_111,
 ex_mbrms3dwHW_1full_1111,ex_mbrms3dwHW_1full_1111s1,
 cores = getOption("mc.cores", 1))


ex_mbrms3dwHW_1full_ex113 <- brm(
 bf(extension ~ WtavR_centered + winSlope + WcsavR + gulf + Bheight +
 winSlope:Bheight + WtavR_centered:gulf + WcsavR:gulf + (1 | colony/year),
 shape ~ WtavR_centered + WcsavR), # Model shape parameter for variance
 data = testex, family = Gamma(link = "log"),
 chains = 3, control = list(adapt_delta = 0.95), iter = 6000)
summary(ex_mbrms3dwHW_1full_ex113)
pp_check(ex_mbrms3dwHW_1full_ex113)
# Extract aligned fitted values and residuals
aligned_fitted <- fitted(ex_mbrms3dwHW_1full_ex113)[, "Estimate"]
aligned_residuals <- residuals(ex_mbrms3dwHW_1full_ex113, summary = TRUE)[, "Estimate"]

# Create the residuals vs. fitted values plot
plot(aligned_fitted, aligned_residuals,
 xlab = "Fitted Values", ylab = "Residuals",
 main = "Residuals vs Fitted Values", pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)

ex_mbrms3dwHW_1full_ex113a <- brm(
 bf(extension ~ WtavR_centered + winSlope + WcsavR + gulf + Bheight +
 winSlope:Bheight + WtavR_centered:gulf + WcsavR:gulf + (1 | colony/year),
 shape ~ WtavR_centered + WcsavR), # Model shape parameter for variance
 data = testex, family = Gamma(link = "log"),
 chains = 3, control = list(adapt_delta = 0.95), iter = 6000)

### Not much fruit here. But considering the pp_check() - a bayesian assessment of goodness of fit
library(brms)
pp_check(ex_mbrms3dwHW_1full_1111)
## the overall fit is excellent

### Moving onto growth inside the gulf exclusively

## checking the contributions to variance again for model ex_mbrms3dwHW_1full_ex11
## Bayesian Posterior Predictive Comparisons

##### Finished extension saved in:
save.image(file='BRMS-Ex-Gamma.RData')
## for the whole dataset

### Now moving onto model for just gulf colonies
testexgulf <- subset (testex, gulf =="gulf")


gulf_1full <- brms::brm(extension~ StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+StavR:Bheight+WtavR:Bheight+ 
 winSlope:Bheight+ StsdR:Bheight+ sumSlope:Bheight +(1 + Bheight|colony/year),
 data = testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 6000)

gulf_1full_1 <- brms::brm(extension~ StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+(1 |colony/year),
 data = testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)



gulf_1full_2 <- brms::brm(extension~ StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+(1 |colony),
 data = testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 3000)

gulf_1full_3 <- brms::brm(extension~ StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+StavR:Bheight+WtavR:Bheight+ 
 winSlope:Bheight+ StsdR:Bheight+ sumSlope:Bheight+(1 + Bheight|colony),
 data = testexgulf, family = gaussian(), chains = 3, control = list(adapt_delta = 0.99),
 iter = 3000)


### trouble fitting models with gamma distribution.... trying to scale explanatory variables

# Scale and center all numeric predictors except Bheight, year, and extension
scaled_testexgulf <- testexgulf %>%
 dplyr::mutate(across(
 .cols = dplyr::where(is.numeric) & !dplyr::any_of(c("Bheight", "year", "extension")),
 .fns = ~ scale(.)[, 1], # Center and scale, extract scaled column
 .names = ".col"
 ))

# View the result
head(scaled_testexgulf)

#### Tring to fit again with scaled variables
gulf_1full_sc1 <- brms::brm(extension~ StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+(1 + Bheight|colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 3000)
gulf_1full_sc2 <- brms::brm(extension~ StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
gulf_1full_sc3 <- brms::brm(extension~ StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+(1 |colony),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
gulf_1full_sc4 <- brms::brm(extension~ StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+(1 + Bheight|colony),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)

loo(gulf_1full_sc2, gulf_1full_sc3, gulf_1full_sc4,
 cores = getOption("mc.cores", 1))

library(performance)
r2_bayes(gulf_1full_sc1)
r2_bayes(gulf_1full_sc2)
r2_bayes(gulf_1full_sc3)
r2_bayes(gulf_1full_sc4)
loo(gulf_1full, gulf_1full_1, 
 gulf_1full_2,
 gulf_1full_3,
 cores = getOption("mc.cores", 1))
summary(gulf_1full_sc2)
### model 1 is useless, model 2 looks the best. Tring to add interactions to see where they destabilize forward-wise
## also checking addition of site
gulf_1full_sc21 <- brms::brm(extension~ Site+StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+
 StavR:Bheight+WtavR:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
gulf_1full_sc22 <- brms::brm(extension~ Site+StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+
 WtsdR:Bheight+winSlope:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
gulf_1full_sc23 <- brms::brm(extension~ Site+StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+
 StsdR:Bheight+sumSlope:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
gulf_1full_sc24 <- brms::brm(extension~ Site+StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+
 StsdR:Bheight+WcsavR:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
loo(gulf_1full_sc2, gulf_1full_sc3, gulf_1full_sc4,gulf_1full_sc21, gulf_1full_sc22, gulf_1full_sc23, gulf_1full_sc24,
 cores = getOption("mc.cores", 1))
r2_bayes(gulf_1full_sc2)
r2_bayes(gulf_1full_sc24)
gulf_1full_sc241 <- brms::brm(extension~ Site+WtsdR+ StsdR+ sumSlope+ Bheight+
 StsdR:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
gulf_1full_sc242 <- brms::brm(extension~ WtsdR+ StsdR+ sumSlope+ Bheight+
 StsdR:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
gulf_1full_sc243 <- brms::brm(extension~ Site+StavR+WtavR+ WtsdR+ winSlope+ StsdR+ sumSlope+ WcsavR+ Bheight+
 StsdR:Bheight+WcsavR:Bheight+Site:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
loo(gulf_1full_sc2, gulf_1full_sc3, gulf_1full_sc4,gulf_1full_sc21, 
 gulf_1full_sc22, gulf_1full_sc23, gulf_1full_sc24, gulf_1full_sc241, gulf_1full_sc242,gulf_1full_sc243,
 cores = getOption("mc.cores", 1))
r2_bayes(gulf_1full_sc24)
r2_bayes(gulf_1full_sc242)
r2_bayes(gulf_1full_sc241)

### Looks like model gulf_1full_sc242 is the simplest that explains the most variation - can it be reduced?
gulf_1full_sc242 <- brms::brm(extension~ WtsdR+ StsdR+ sumSlope+ Bheight+
 StsdR:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
gulf_1full_sc2421 <- brms::brm(extension~ WtsdR+ StsdR+ Bheight+
 StsdR:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
gulf_1full_sc2422 <- brms::brm(extension~StsdR+ sumSlope+ Bheight+
 StsdR:Bheight+(1 |colony/year),
 data = scaled_testexgulf, family = Gamma(link = "log"), chains = 3, control = list(adapt_delta = 0.99),
 iter = 4000)
loo(gulf_1full_sc24, gulf_1full_sc241, gulf_1full_sc242,gulf_1full_sc243,
 gulf_1full_sc2421, gulf_1full_sc2422,
 cores = getOption("mc.cores", 1))

## No it looks like gulf_1full_sc242 is the best model
### Some checks of goodness of fit
pp_check(gulf_1full_sc242) ## looks excellent
## model residuals
aligned_fitted <- fitted(gulf_1full_sc242)[, "Estimate"]
aligned_residuals <- residuals(gulf_1full_sc242, summary = TRUE)[, "Estimate"]

# Create the residuals vs. fitted values plot
plot(aligned_fitted, aligned_residuals,
 xlab = "Fitted Values", ylab = "Residuals",
 main = "Residuals vs Fitted Values", pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)
lines(lowess(aligned_fitted, aligned_residuals), col = "darkgreen", lwd = 2) ## Looks ok

##residuals per predictor
fitted_values <- fitted(gulf_1full_sc242)[, "Estimate"]
scaled_testexgulf$residuals <- scaled_testexgulf$extension - fitted_values # Residuals = Observed - Fitted

# Calculate mean residuals for each colony
library(dplyr)
mean_residuals_colony <- scaled_testexgulf %>%
 group_by(colony) %>%
 summarise(mean_residual = mean(residuals, na.rm = TRUE))

# List of single fixed terms and random effect variables
fixed_random_effects <- c("sumSlope", "WtsdR", "StsdR", "Bheight","colony", "year")

# Create individual plots
library(ggplot2)
residplots <- lapply(fixed_random_effects, function(effect) 
 if (effect == "colony") 
 # Special case for colony: Add mean points
 ggplot(testex, aes_string(x = effect, y = "residuals")) +
 geom_point(alpha = 0.5) +
 geom_point(data = mean_residuals_colony, aes(x = colony, y = mean_residual), 
 color = "blue", size = 3) +
 geom_smooth(method = "loess", color = "red", se = FALSE) +
 theme_minimal() +
 labs(x = effect, y = "Residuals",
 title = paste("Residuals vs", effect))
  else if (effect == "gulf") 
 # Special case for gulf: Use boxplot
 ggplot(testex, aes_string(x = effect, y = "residuals")) +
 geom_boxplot(fill = "lightblue", color = "black") +
 theme_minimal() +
 labs(x = effect, y = "Residuals",
 title = paste("Residuals vs", effect))
  else 
 # Standard plots for other variables
 ggplot(testex, aes_string(x = effect, y = "residuals")) +
 geom_point(alpha = 0.5) +
 geom_smooth(method = "loess", color = "blue", se = FALSE) +
 theme_minimal() +
 labs(x = effect, y = "Residuals",
 title = paste("Residuals vs", effect))
 
)

# Arrange plots in a grid
library(grid)
library(gridExtra)
grid.arrange(grobs = residplots, ncol = 2)

#### Looks like a good model - nice fit and no trends in residuals
############## Variance decomposition::
## checking the contributions to variance again for model gulf_1full_sc242
## Bayesian Posterior Predictive Comparisons
library(brms)
library(dplyr)
library(ggplot2)

# Define the full list of fixed effect terms
fixed_effect_terms <- c("Site", "WtsdR", "StsdR", "sumSlope", "Bheight", "StsdR:Bheight")

# Function to compute variance contribution for a subset of terms
compute_variance_contribution <- function(terms, full_model, original_data) 
 reduced_formula <- reformulate(terms, response = "extension")
 reduced_model <- update(full_model, formula = reduced_formula)
 preds <- posterior_epred(reduced_model, newdata = original_data, re_formula = NA)
 apply(preds, 1, stats::var) # Variance across posterior samples


# Fit the full model
current_model <- gulf_1full_sc241

# Total variance for the full model
posterior_preds <- posterior_epred(current_model, newdata = scaled_testexgulf, re_formula = NA)
posterior_total_var <- apply(posterior_preds, 1, stats::var) # Variance for each posterior sample
total_var <- mean(posterior_total_var)

# Initialize results
results <- data.frame(Term = fixed_effect_terms, Variance_Proportion = NA, SE = NA)

for (i in seq_along(fixed_effect_terms)) 
 # Exclude the current term and all dependent interaction terms
 excluded_terms <- fixed_effect_terms[i]
 dependent_terms <- fixed_effect_terms[sapply(fixed_effect_terms, function(term) 
 any(unlist(strsplit(term, ":")) %in% excluded_terms)
 )]
 
 included_terms <- setdiff(fixed_effect_terms, c(excluded_terms, dependent_terms))
 
 # Compute variance contribution
 subset_var <- compute_variance_contribution(included_terms, current_model, scaled_testexgulf)
 
 # Proportion of variance explained by the excluded term
 variance_proportion <- (posterior_total_var - subset_var) / total_var
 results$Variance_Proportion[i] <- mean(variance_proportion, na.rm = TRUE)
 results$SE[i] <- sd(variance_proportion, na.rm = TRUE) / sqrt(length(variance_proportion))


# Handle random effects
post_samples <- as_draws_df(current_model) %>% as.data.frame()
random_effect_vars <- colMeans(post_samples[, grepl("^sd_", colnames(post_samples))]^2)

# Normalize random effect contributions
random_effect_proportions <- random_effect_vars / total_var
random_effect_ses <- apply(
 post_samples[, grepl("^sd_", colnames(post_samples))]^2,
 2,
 function(var_samples) sd(var_samples / total_var) / sqrt(nrow(post_samples))
)

# Add random effects
random_effect_results <- data.frame(
 Term = names(random_effect_vars),
 Variance_Proportion = random_effect_proportions,
 SE = random_effect_ses
)

# Combine fixed effects and random effects
results <- rbind(
 results,
 random_effect_results
)

# Normalize contributions to sum to 1
results$Variance_Proportion <- pmax(0, results$Variance_Proportion) / sum(pmax(0, results$Variance_Proportion), na.rm = TRUE)

# Sort results by Variance_Proportion
results <- results[order(-results$Variance_Proportion), ]

# Print results
print(results)

# Verify total proportion
cat("Sum of Variance Proportions:", sum(results$Variance_Proportion, na.rm = TRUE), "")

## Plot variance explained
# Group terms for visualization
results$Group <- case_when(
 results$Term %in% c("StsdR", "StsdR:Bheight") ~ "StsdR and interactions",
 results$Term %in% c("Bheight") ~ "Bheight",
 results$Term %in% c("Site") ~ "Site",
 results$Term %in% c("WtsdR") ~ "WtsdR",
 results$Term %in% c("sumSlope") ~ "sumSlope",
 TRUE ~ results$Term
)

# Summarize grouped contributions
grouped_results <- results %>%
 group_by(Group) %>%
 summarise(
 Variance_Proportion = sum(Variance_Proportion, na.rm = TRUE),
 SE = sqrt(sum(SE^2, na.rm = TRUE)) # Pooled SE for grouped terms
 ) %>%
 arrange(desc(Variance_Proportion))

# Plot grouped contributions
ggplot(grouped_results, aes(x = reorder(Group, -Variance_Proportion), y = Variance_Proportion)) +
 geom_bar(stat = "identity", fill = "skyblue") +
 geom_errorbar(aes(ymin = Variance_Proportion - SE, ymax = Variance_Proportion + SE), width = 0.3) +
 labs(x = "Terms/Groups", y = "Variance Proportion", title = "Variance Decomposition by Terms and Groups") +
 theme_minimal() +
 coord_flip()

#########################
########## Some plots of results
current_model <- gulf_1full_sc241

plot_bheight_stsdr <- psurface(
 x = "Bheight", # Variable for the x-axis
 y = "StsdR", # Variable for the y-axis
 data = scaled_testexgulf, # Dataset used in your model
 model = current_model, # The fitted Bayesian model
 grid_length = 50, # Number of points in the prediction grid
 xlab = "Bheight (cm)", # Label for the x-axis
 ylab = "StsdR (Summer Temp variation)" # Label for the y-axis
 )
plot_bheight_wtsdr <- psurface(
 x = "Bheight", # Variable for the x-axis
 y = "WtsdR", # Variable for the y-axis
 data = scaled_testexgulf, # Dataset used in your model
 model = current_model, # The fitted Bayesian model
 grid_length = 50, # Number of points in the prediction grid
 xlab = "Bheight (cm)", # Label for the x-axis
 ylab = "WtsdR (Summer Temp variaitin)" # Label for the y-axis
 )
plot_bheight_sumslope <- psurface(
 x = "Bheight", # Variable for the x-axis
 y = "sumSlope", # Variable for the y-axis
 data = scaled_testexgulf, # Dataset used in your model
 model = current_model, # The fitted Bayesian model
 grid_length = 50, # Number of points in the prediction grid
 xlab = "Bheight (cm)", # Label for the x-axis
 ylab = "R-O-C to summer" # Label for the y-axis
 )
save.image(file='Extension_complete.RData')

load(file='/Users/glenndunshea/Documents/AAlongtermgrowth/Extension_complete.RData')
