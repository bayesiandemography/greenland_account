
library(pomp)
library(dplyr)
library(tidyr)
library(purrr)

N0 <- readRDS("out/N0.rds")

data <- readRDS("out/data.rds")

covar_df <- readRDS("out/covar_df.rds")

## Assemble components --------------------------------------------------------

data_covar <- covar_df %>%
    left_join(data, by = c("birth_cohort", "sex", "region", "k")) %>%
    group_by(birth_cohort, sex, region) %>%
    nest() %>%
    left_join(N0, by = c("birth_cohort", "sex", "region"))


## Make estimation functions --------------------------------------------------

## C functions to simulate from, and calculate
## density for, a Poisson-binomial mixture.

globals <- "

int rpoisbin(int size, double prob) {
   int x = rbinom(size, prob);
   int y = rpois(size * (1 - prob));
   return x + y;
}

double dpoisbin(int x, int size, double prob, int give_log) {
    double lambda = (1 - prob) * size;
    double ans = 0.0;
    if (x > 50) {
        double mean_binom = prob * size;
        double var_binom = prob * lambda;
        double mean_pois = lambda;
        double var_pois = lambda;
        double mean = mean_binom + mean_pois;
        double sd = sqrt(var_binom + var_pois);
        ans = dnorm(x, mean, sd, give_log);
    }
    else {
        double limit = x < size ? x : size;
        for (int i = 0; i <= limit; ++i) {
            ans += dbinom(i, size, prob, 0)
                * dpois(x - i, lambda, 0);
        }
        if (give_log)
            ans = log(ans);
    }
    return ans;
}

"


## Function to generate initial values for state parameters
## 'N' is population, 'cin' is counts of combined inward migration,
## 'count' is counts of combined outmigration, 'exposure' is
## (approximate) person years lived during period
## Allow N0 to be a noisy measure of the true initial state,
## though with only a small amount of noise.

rinit <- Csnippet("
    N = rpoisbin(N0, 0.98);
    cin = 0;
    cout = 0;
    exposure = 0;
")



## Function to calculate state variables at end of
## time period. The state variables 'bth' and 'dth'
## are counts births and deaths during that period.
## We are treating them as known.
## The 'exposure_approx' term in the step function
## is derived from the equations
##       N1 = N0 - dth + cin - rate_cout * exposure
## exposure = 0.25 * (N0 + N1).
step.fun <- Csnippet("
  int n_attempt = 100;
  double eps = 0.1;
  int Nstart = N;
  int found_ans = 0;
  for (int i = 0; i < n_attempt; i++) {
    cin = rpois(rate_cin);
    double exposure_approx = (2 * Nstart + cin - count_dth) / (4 + rate_cout);
    exposure_approx = (exposure_approx > eps) ? exposure_approx : eps;
    cout = rpois(rate_cout * exposure_approx);
    N = Nstart - count_dth + cin - cout;
    if (N >= 0) {
      found_ans = 1;
      break;
    }
  }
  if (!found_ans) { // use 'cin' to get non-negative 'N'
    N = 0;
    cin = cout + count_dth - Nstart;
  }
  exposure = 0.25 * Nstart + 0.25 * N;
")
rprocess <- onestep(step.fun)


## Function to calculate probability of data
## (including births and deaths)
## given value of state variables.

dmeasure <- Csnippet("
    double na_ans = (give_log) ? 0 : 1;
    double ans_N = (ISNA(YN)) ? na_ans : dpoisbin(YN, N, 0.98, give_log);
    double ans_cin = dpoisbin(Ycin, cin, 0.95, give_log);
    double ans_cout = dpoisbin(Ycout, cout, 0.95, give_log);
    double ans_bth = (ISNA(Ybth)) ? na_ans : dpois(Ybth, Yratebth * exposure, give_log);
    double ans_dth = dpois(Ydth, Yratedth * exposure, give_log);
    lik = ans_N + ans_cin + ans_cout + ans_bth + ans_dth;
    lik = give_log ? lik : exp(lik);
")



## Function to estimate one model ---------------------------------------------


run_pfilter <- function(data_covar, N0, rinit, rprocess, dmeasure, Np) {
    col_data <- c("k", "YN", "Ycin", "Ycout", "Ybth", "Yratebth", "Ydth", "Yratedth")
    col_covar <- c("k", "count_bth", "count_dth", "rate_cin", "rate_cout")
    data <- data_covar[col_data]
    covar <- data_covar[col_covar]
    covar$k  <- covar$k - 1L
    covar <- covariate_table(covar,
                             times = "k")
    times <- data$k
    t0 <- data$k[1L] - 1L
    params <- c(N0 = N0)
    suppressWarnings(
        pfilter(data = data,
                Np = Np,
                times = times,
                t0 = t0,
                globals = globals,
                rinit = rinit,             
                params = params,
                rprocess = rprocess,
                dmeasure = dmeasure,
                covar = covar,
                statenames = c("N", "cin", "cout", "exposure"),
                paramnames = "N0",
                save.states = TRUE,
                filter.mean = TRUE)
    )
}


## Apply to all cohorts -------------------------------------------------------

cohort_with_single_obs <- c(1910, 2020)

Sys.time()
model <- data_covar %>%
    filter(!(birth_cohort %in% cohort_with_single_obs)) %>%
    mutate(model = map2(data, N0, run_pfilter,
                        rinit = rinit,
                        rprocess = rprocess,
                        dmeasure = dmeasure,
                        Np = 10000))
Sys.time()

saveRDS(model,
        file = "out/model.rds")

