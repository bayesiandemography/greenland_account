
library(pomp)
library(dplyr)
library(tidyr)
library(purrr)

N0 <- readRDS("out/N0.rds")

data <- readRDS("out/data.rds")

covar_df <- readRDS("out/covar_df.rds")

## Assemble components --------------------------------------------------------

models <- covar_df %>%
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

## Function to calculate state variables at end of
## time period. The state variables 'bth' and 'dth'
## are counts births and deaths during that period.
## We are treating them as known.
## The 'exposure' term in the step function
## is derived from the equations
##       N1 = N0 - dth + im - rate_em * exposure
## exposure = 0.25 * (N0 + N1).
step.fun <- function(N, count_dth, rate_im, rate_em, ...) {
    im <- rpois(n = 1, lambda = rate_im)
    exposure_approx <- (2 * N + im - count_dth) / (4 + rate_em)
    em <- rpois(n = 1, lambda = rate_em * exposure_approx)
    N1 <- N - count_dth + im - em
    exposure <- 0.25 * N + 0.25 * N1
    c(N = N1, im = im, em = em, exposure = exposure)
}

## Create an 'rprocess' object from the function

rprocess <- onestep(step.fun)


## Function to calculate probability of data
## given value of state variables.

dmeasure <- function(log,
                     YN, N, Yim, im, Yem, em,
                     count_bth, rate_bth, count_dth, rate_dth,
                     exposure, s, ...) {
    eps <- 1
    na_ans <- if (log) 0 else 1
    ans_N <- if (is.na(YN)) na_ans else dnorm(YN, mean = N, sd = s * (N + eps), log = log)
    ans_im <- dnorm(Yim, mean = im, sd = s * (im + eps), log = log)
    ans_em <- dnorm(Yem, mean = em, sd = s * (em + eps), log = log)
    ans_bth <- if (is.na(count_bth)) na_ans else dpois(count_bth, lambda = rate_bth * exposure, log = log)
    ans_dth <- dpois(count_dth, lambda = rate_dth * exposure, log = log)
    ans_N + ans_im + ans_em + ans_bth + ans_dth
}


## Function to estimate one model ---------------------------------------------


run_pfilter <- function(data_covar, N0, rinit, rprocess, dmeasure, Np, s) {
    col_data <- c("k", "YN", "Yim", "Yem")
    col_covar <- c("k", "count_bth", "count_dth", "rate_bth", "rate_dth", "rate_im", "rate_em")
    data <- data_covar[col_data]
    covar <- data_covar[col_covar]
    covar$k  <- covar$k - 1L
    covar <- covariate_table(covar,
                             times = "k")
    times <- data$k
    t0 <- data$k[1L] - 1L
    params <- c(N0 = N0, s = s)
    pfilter(data = data,
            Np = Np,
            times = times,
            t0 = t0,
            rinit = rinit,             
            params = params,
            rprocess = rprocess,
            dmeasure = dmeasure,
            covar = covar,
            save.states = TRUE,
            filter.mean = TRUE)
}


models1 <- models %>%
    filter(sex == "Male" & birth_cohort == 1980)

x <- run_pfilter(data_covar = models1$data[[1]],
                 N0 = models1$N0,
                 rinit = rinit,
                 rprocess = rprocess,
                 dmeasure = dmeasure,
                 Np = 10000,
                 s = 0.1)

as.data.frame(x)

x %>%
    as.data.frame() %>%
    select(time,
           pop_obs = YN,
           pop_est = filter.mean.N,
           im_obs = Yim,
           im_est = filter.mean.im,
           em_obs = Yem,
           em_est = filter.mean.em) %>%
    pivot_longer(cols = -time) %>%
    separate(name, into = c("variable", "variant"), convert = TRUE) %>%
    ggplot(aes(x = time, y = value, color = variant)) +
    facet_wrap(vars(variable), scales = "free_y") +
    geom_line() +
    geom_point()


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

