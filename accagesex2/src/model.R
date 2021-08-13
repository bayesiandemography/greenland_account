
library(pomp)
library(dplyr)
library(tidyr)
library(purrr)

N0 <- readRDS("out/N0.rds")

data <- readRDS("out/data.rds")

covar_df <- readRDS("out/covar_df.rds")

## Assemble components --------------------------------------------------------

data_covar <- covar_df %>%
    left_join(data, by = c("birth_cohort", "sex", "k")) %>%
    group_by(birth_cohort, sex) %>%
    nest() %>%
    left_join(N0, by = c("birth_cohort", "sex"))


## Make estimation functions --------------------------------------------------

## Function to generate initial values for state parameters
## 'N' is population, 'im' is counts of immigration,
## 'em' is counts of emigration

rinit <- function(N0, ...) {
    c(N = N0, im = 0, em = 0, exposure = 0)
}

## The following is a bit of a hack -
## we can improve in future
step.fun <- Csnippet("
  int n_attempt = 100;
  double eps = 0.1;
  int Nstart = N;
  int found_ans = 0;
  for (int i = 0; i < n_attempt; i++) {
    im = rpois(rate_im);
    double exposure_approx = (2 * Nstart + im - count_dth) / (4 + rate_em);
    exposure_approx = (exposure_approx > eps) ? exposure_approx : eps;
    em = rpois(rate_em * exposure_approx);
    N = Nstart - count_dth + im - em;
    if (N >= 0) {
      found_ans = 1;
      break;
    }
  }
  if (!found_ans) { // use 'im' to get non-negative 'N'
    N = 0;
    im = em + count_dth - Nstart;
  }
  exposure = 0.25 * Nstart + 0.25 * N;
")


## Create an 'rprocess' object from the function

rprocess <- onestep(step.fun)


## Function to calculate probability of data
## given value of state variables.

dmeasure <- Csnippet("
  double eps = 1;
  double na_ans = (give_log) ? 0 : 1;
  double ans_N = (ISNA(YN)) ? na_ans : dnorm(YN, N, s * (N + eps), give_log);
  double ans_im = dnorm(Yim, im, s * (im + eps), give_log);
  double ans_em = dnorm(Yem, em, s * (em + eps), give_log);
  double ans_bth = (ISNA(Ybth)) ? na_ans : dpois(Ybth, Yratebth * exposure, give_log);
  double ans_dth = dpois(Ydth, Yratedth * exposure, give_log);
  lik = ans_N + ans_im + ans_em + ans_bth + ans_dth;
")



## Function to estimate one model ---------------------------------------------


run_pfilter <- function(data_covar, N0, rinit, rprocess, dmeasure, Np, s) {
    col_data <- c("k", "YN", "Yim", "Yem", "Ybth", "Yratebth", "Ydth", "Yratedth")
    col_covar <- c("k", "count_bth", "count_dth", "rate_im", "rate_em")
    data <- data_covar[col_data]
    covar <- data_covar[col_covar]
    covar$k  <- covar$k - 1L
    covar <- covariate_table(covar,
                             times = "k")
    times <- data$k
    t0 <- data$k[1L] - 1L
    params <- c(N0 = N0, s = s)
    suppressWarnings(
        pfilter(data = data,
                Np = Np,
                times = times,
                t0 = t0,
                rinit = rinit,             
                params = params,
                rprocess = rprocess,
                dmeasure = dmeasure,
                covar = covar,
                statenames = c("N", "im", "em", "exposure"),
                paramnames = c("N0", "s"),
                save.states = TRUE,
                filter.mean = TRUE)
    )
}


## Apply to all cohorts -------------------------------------------------------

cohort_with_single_obs <- c(1910, 2020)

model <- data_covar %>%
    filter(!(birth_cohort %in% cohort_with_single_obs)) %>%
    mutate(model = map2(data, N0, run_pfilter,
                        rinit = rinit,
                        rprocess = rprocess,
                        dmeasure = dmeasure,
                        Np = 10000,
                        s = 0.025))

saveRDS(model,
        file = "out/model.rds")

