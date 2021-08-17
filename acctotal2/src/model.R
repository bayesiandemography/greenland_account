
library(pomp)
library(dplyr)
library(tidyr)

data <- readRDS("out/data.rds")

covar_df <- readRDS("out/covar_df.rds")

## Assemble components --------------------------------------------------------

## Covariates

covar <- covar_df %>%
    covariate_table(times = "time")


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
## 'N' is population, 'im' is counts of immigration,
## 'em' is counts of emigration, 'exposure' is
## (approximate) person years lived during period
## Allow N0 to be a noisy measure of the true initial state,
## though with only a small amount of noise.

rinit <- Csnippet("
    N = rpoisbin(N0, 0.98);
    im = 0;
    em = 0;
    exposure = 0;
")


## Function to calculate state variables at end of
## time period. The state variables 'bth' and 'dth'
## are counts births and deaths during that period.
## We are treating them as known.
## The 'exposure_approx' term in the step function
## is derived from the equations
##       N1 = N0 + bth - dth + im - rate_em * exposure
## exposure = 0.5 * (N0 + N1).

step.fun <- Csnippet("
    int Nstart = N;
    im = rpois(rate_im);
    double exposure_approx = (Nstart + 0.5 * (count_bth - count_dth + im)) / (1 + 0.5 * rate_em);
    em = rpois(rate_em * exposure_approx);
    N = Nstart + count_bth - count_dth + im - em;
    exposure = 0.5 * (Nstart + N);
")
rprocess <- onestep(step.fun)


## Function to calculate probability of data
## given value of state variables.
## The data model for population, immigration,
## and emigration is log(y + 1) ~ N(log(x + 1), s^2).

dmeasure <- Csnippet("
    double ans_N = dpoisbin(YN, N, 0.98, give_log);
    double ans_im = dpoisbin(Yim, im, 0.95, give_log);
    double ans_em = dpoisbin(Yem, em, 0.95, give_log);
    double ans_bth = dpois(Ybth, Yratebth * exposure, give_log);
    double ans_dth = dpois(Ydth, Yratedth * exposure, give_log);
    lik = ans_N + ans_im + ans_em + ans_bth + ans_dth;
    lik = give_log ? lik : exp(lik);
")

    
## Parameters 

params <- c(N0 = 56615) # initial population


## Times (note - time indexing in 'pomp' is confusing!)

times <- covar_df %>%
    pull(time) %>%
    as.integer() + 1L
t0 <- times[1] - 1L


## Run model ------------------------------------------------------------------

message("*** Ignore the warning about extrapolating ***")

model <- pfilter(data = data,
                 Np = 100000,
                 times = times,
                 t0 = t0,
                 globals = globals,
                 rinit = rinit,             
                 params = params,
                 rprocess = rprocess,
                 dmeasure = dmeasure,
                 covar = covar,
                 save.states = TRUE,
                 filter.mean = TRUE,
                 statenames = c("N", "im", "em", "exposure"),
                 paramnames = "N0")

model %>%
    as.data.frame() %>%
    print(digits = 3)

saveRDS(model,
        file = "out/model.rds")

