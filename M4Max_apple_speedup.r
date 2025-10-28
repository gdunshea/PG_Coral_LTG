##system speed up.

## Use all cores everywhere
options(mc.cores = parallel::detectCores())
Sys.setenv(MAKEFLAGS = paste0("-j", parallel::detectCores()))  # parallel C++ builds

if (!"RhpcBLASctl" %in% rownames(installed.packages())) install.packages("RhpcBLASctl")
library(RhpcBLASctl)
blas_set_num_threads(parallel::detectCores())
omp_set_num_threads(parallel::detectCores())

## One-time if needed:
install.packages("remotes")
remotes::install_github("stan-dev/cmdstanr")
library(cmdstanr)
# cmdstanr::install_cmdstan()  # run once; comment out afterwards

library(brms)
options(brms.backend = "cmdstanr")

## Helper: auto-choose chains and threads-per-chain to saturate CPU
choose_chains_threads <- function(target_chains = 4L) {
  cores <- parallel::detectCores()
  chains <- max(2L, min(target_chains, cores))
  tpc <- max(1L, floor(cores / chains))
  list(chains = chains, threads_per_chain = tpc)
}
ct <- choose_chains_threads(4L)  # try 4 chains; will distribute threads

## Example fit call (Gamma model shown)
fit <- brm(
  extension ~ WtavR_centered + winSlope + WcsavR + gulf + Bheight +
    winSlope:Bheight + WtavR_centered:gulf + WcsavR:gulf + (1 | colony/year),
  data   = testex,
  family = Gamma(link = "log"),
  chains = ct$chains,
  threads = threading(ct$threads_per_chain),  # <-- within-chain parallelism
  iter   = 5000, warmup = 3000,
  control = list(adapt_delta = 0.99),         # safer with heavy threading
  refresh = 0                                  # quieter console
)

## Posterior predictive etc. will also parallelize across cores automatically


