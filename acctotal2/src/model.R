
library(pomp)
library(dplyr)
library(tidyr)

data <- readRDS("out/data.rds")

covar_df <- readRDS("out/covar_df.rds")

## Assemble components --------------------------------------------------------

## Covariates

covar <- covar_df %>%
    covariate_table(times = "time")


## Function to generate initial values for state parameters
## 'N' is population, 'im' is counts of immigration,
## 'em' is counts of emigration, 'exposure' is
## (approximate) person years lived during period

rinit <- function(N0, ...) {
    c(N = N0, im = 0, em = 0, exposure = 0)
}


## Function to calculate state variables at end of
## time period. The state variables 'bth' and 'dth'
## are counts births and deaths during that period.
## We are treating them as known.
## The 'exposure_approx' term in the step function
## is derived from the equations
##       N1 = N0 + bth - dth + im - rate_em * exposure
## exposure = 0.5 * (N0 + N1).
step.fun <- function(N, count_bth, count_dth, rate_im, rate_em, ...) {
    im <- rpois(n = 1, lambda = rate_im)
    exposure_approx <- (N + 0.5 * (count_bth - count_dth + im)) / (1 + 0.5 * rate_em)
    em <- rpois(n = 1, lambda = rate_em * exposure_approx)
    N1 <- N + count_bth - count_dth + im - em
    exposure <- 0.5 * (N + N1)
    c(N = N1, im = im, em = em, exposure = exposure)
}

## Create an 'rprocess' object from the function

rprocess <- onestep(step.fun)


## Function to calculate probability of data
## given value of state variables.
## The data model for population, immigration,
## and emigration is y ~ N(x, (sx)^2).

dmeasure <- function(log,
                     YN, N, Yim, im, Yem, em, 
                     Ybth, Yratebth, Ydth, Yratedth,
                     exposure, s, ...) {
    dnorm(YN, mean = N, sd = s * N, log = log) +
        dnorm(Yim, mean = im, sd = s * im, log = log) + 
        dnorm(Yem, mean = em, sd = s * em, log = log) +
        dpois(Ybth, lambda = Yratebth * exposure, log = log) +
        dpois(Ydth, lambda = Yratedth * exposure, log = log)
}

## Parameters 

params <- c(N0 = 56615, # initial population
            s = 0.01)   # parameter from data model


## Times (note - time indexing in 'pomp' is confusing!)

times <- covar_df %>%
    pull(time) %>%
    as.integer() + 1L
t0 <- times[1] - 1L


## Run model ------------------------------------------------------------------

message("*** Ignore the warning about extrapolating ***")

model <- pfilter(data = data,
                 Np = 1000,
                 times = times,
                 t0 = t0,
                 rinit = rinit,             
                 params = params,
                 rprocess = rprocess,
                 dmeasure = dmeasure,
                 covar = covar,
                 save.states = TRUE,
                 filter.mean = TRUE)

model %>%
    as.data.frame() %>%
    print(digits = 3)

saveRDS(model,
        file = "out/model.rds")

