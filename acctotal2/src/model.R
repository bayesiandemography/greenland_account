
library(dembase)
library(demest)
library(pomp)
library(dplyr)
library(tidyr)

datasets <- readRDS("out/datasets.rds")


## Calculate migration rates

exposure <- datasets$reg_popn %>%
    exposure()

estimateModel(Model(y ~ Poisson(mean ~ 1, useExpose = FALSE)),
              y = datasets$reg_immigration,
              filename = "out/immigration.est",
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 5)


estimateModel(Model(y ~ Poisson(mean ~ 1)),
              y = datasets$reg_emigration,
              exposure = exposure,
              filename = "out/emigration.est",
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 5)


                    
rate_immigration <- fetch("out/immigration.est", where = c("model", "likelihood", "count")) %>%
    collapseIterations(FUN = median)

rate_emigration <- fetch("out/emigration.est", where = c("model", "likelihood", "rate")) %>%
    collapseIterations(FUN = median)


rinit <- function(N0, ...) {
    c(N = N0)
}

step.fun <- function(N, mu_im, mu_em, ...) {
    im <- rpois(n = 1, lambda = mu_im)
    exposure_em <- N + 0.5 * im
    em <- rpois(n = 1, lambda = mu_em * exposure_em)
    N <- N + im - em
    c(N = N)
}

rprocess <- discrete_time(step.fun)

params <- c(N0 = 60000,
            mu_im = 2300,
            mu_em = 0.04)

sim1 <- simulate(t0 = 0,
                 times = 1:10,
                 params = params,
                 rinit = rinit,
                 rprocess = rprocess)

rmeasure <- function(N, s, ...) {
    Y <- rnorm(n = 1, mean = N, sd = s * N)
    c(Y = Y)
}

params <- c(N0 = 60000,
            mu_im = 2300,
            mu_em = 0.04,
            s = 0.05)

sim2 <- simulate(nsim = 20,
                 t0 = 0,
                 times = 1:10,
                 params = params,
                 rinit = rinit,
                 rprocess = rprocess,
                 rmeasure = rmeasure)


sim2 %>%
    as.data.frame() %>%
    pivot_longer(c(Y, N)) %>%
    ggplot(aes(x = time, y = value, group = .id)) +
    facet_wrap(vars(name)) +
    geom_line()

count_bth <- datasets$reg_births %>%
    as.data.frame() %>%
        rename(bth = count)

count_dth <- datasets$reg_deaths %>%
    as.data.frame() %>%
    rename(dth = count)

rate_im <- rate_immigration %>%
    as.data.frame() %>%
    rename(mu_im = count)

rate_em <- rate_emigration %>%
    as.data.frame() %>%
    rename(mu_em = value)

covar <- count_bth %>%
    inner_join(count_dth, by = "time") %>%
    inner_join(rate_im, by = "time") %>%
    inner_join(rate_em, by = "time") %>%
    covariate_table(times = "time")

rinit <- function(N0, im, em, ...) {
    c(N = N0, im = 0, em = 0)
}

step.fun <- function(N, bth, dth, mu_im, mu_em, ...) {
    im <- rpois(n = 1, lambda = mu_im)
    exposure_em <- N + 0.5 * im - 0.5 * em + bth - dth
    em <- rpois(n = 1, lambda = mu_em * exposure_em)
    N <- N + im - em + bth - dth
    c(N = N, im = im, em = em)
}

rprocess <- onestep(step.fun)

params <- c(N0 = 56615)


sim3 <- simulate(times = 2011:2020,
                 t0 = 2010,
                 rinit = rinit,             
                 params = params,
                 rprocess = rprocess,
                 covar = covar)



## Completely deterministic

count_bth <- datasets$reg_births %>%
    as.data.frame() %>%
        rename(bth = count)

count_dth <- datasets$reg_deaths %>%
    as.data.frame() %>%
    rename(dth = count)

count_im <- datasets$reg_immigration %>%
    as.data.frame() %>%
    rename(im = count)

count_em <- datasets$reg_emigration %>%
    as.data.frame() %>%
    rename(em = count)

covar <- count_bth %>%
    inner_join(count_dth, by = "time") %>%
    inner_join(count_im, by = "time") %>%
    inner_join(count_em, by = "time") %>%
    covariate_table(times = "time")


rinit <- function(N0, ...) {
    c(N = N0)
}


step.fun <- function(N, bth, dth, im, em, ...) {
    N <- N + im - em + bth - dth
    c(N = N)
}

rprocess <- onestep(step.fun)

params <- c(N0 = 56615)


sim3 <- simulate(times = 2011:2020,
                 t0 = 2011,
                 rinit = rinit,             
                 params = params,
                 rprocess = rprocess,
                 covar = covar)

res3 <- as.data.frame(sim3)

res3 %>%
    mutate(N1 = N + bth - dth + im - em)




## Stochastic birth

count_bth <- datasets$reg_births %>%
    as.data.frame() %>%
        rename(ebth = count)

count_dth <- datasets$reg_deaths %>%
    as.data.frame() %>%
    rename(dth = count)

count_im <- datasets$reg_immigration %>%
    as.data.frame() %>%
    rename(im = count)

count_em <- datasets$reg_emigration %>%
    as.data.frame() %>%
    rename(em = count)

covar <- count_bth %>%
    inner_join(count_dth, by = "time") %>%
    inner_join(count_im, by = "time") %>%
    inner_join(count_em, by = "time") %>%
    covariate_table(times = "time")


rinit <- function(N0, bth0, ...) {
    c(N = N0, bth = bth0)
}


step.fun <- function(N, ebth, dth, im, em, ...) {
    bth <- rpois(n = 1, lambda = ebth)
    N <- N + im - em + bth - dth
    c(N = N, bth = bth)
}

rprocess <- onestep(step.fun)

params <- c(N0 = 56615, bth0 = 0)


sim4 <- simulate(times = 2011:2020,
                 t0 = 2011,
                 rinit = rinit,             
                 params = params,
                 rprocess = rprocess,
                 covar = covar)

res4 <- as.data.frame(sim4)

res4 %>%
    mutate(diff = N - (lag(N) + bth - lag(dth) + lag(im) - lag(em)))





## Stochastic immigration, emigration

count_bth <- datasets$reg_births %>%
    as.data.frame() %>%
        rename(bth = count)

count_dth <- datasets$reg_deaths %>%
    as.data.frame() %>%
    rename(dth = count)

rate_im <- rate_immigration %>%
    as.data.frame() %>%
    rename(rate_im = count) %>%
    mutate(time = time - 1)

rate_em <- rate_emigration %>%
    as.data.frame() %>%
    rename(rate_em = value) %>%
    mutate(time = time - 1)

covar <- count_bth %>%
    inner_join(count_dth, by = "time") %>%
    inner_join(rate_im, by = "time") %>%
    inner_join(rate_em, by = "time") %>%
    covariate_table(times = "time")


rinit <- function(N0, im0, em0, ...) {
    c(N = N0, im = im0, em = em0)
}

step.fun <- function(N, bth, dth, rate_im, rate_em, ...) {
    im <- rnorm(n = 1, mean = rate_im, sd = 0.01)
    exposure <- N + 0.5 * (bth - dth + im)
    em <- rpois(n = 1, lambda = rate_em * exposure)
    N <- N + bth - dth + im - em
    c(N = N, im = im, em = em)
}

rprocess <- onestep(step.fun)

params <- c(N0 = 56615, im0 = 0, em0 = 0)


sim5 <- simulate(times = 2011:2020,
                 t0 = 2011,
                 rinit = rinit,             
                 params = params,
                 rprocess = rprocess,
                 covar = covar)

res5 <- as.data.frame(sim5)

res5 %>%
    mutate(diff = N - (lag(N) + lag(bth) - lag(dth) + im - em))



## Include estimation

data_popn <- datasets$reg_popn %>%
    as.data.frame() %>%
    slice(-1) %>%
    rename(YN = count)

data_im <- datasets$reg_immigration %>%
    as.data.frame() %>%
    rename(Yim = count)

data_em <- datasets$reg_emigration %>%
    as.data.frame() %>%
    rename(Yem = count)

## For intervals [t, t+1) time refers to t+1,
data <- data_popn %>%
    inner_join(data_im, by = "time") %>%
    inner_join(data_em, by = "time")

## For intervals [t, t+1) time refers to t+1,
count_bth <- datasets$reg_births %>%
    as.data.frame() %>%
        rename(bth = count)

## For intervals [t, t+1) time refers to t+1,
count_dth <- datasets$reg_deaths %>%
    as.data.frame() %>%
    rename(dth = count)

## For interval [t, t+1), time refers to t+1
rate_im <- rate_immigration %>%
    as.data.frame() %>%
    rename(rate_im = count)

## For interval [t, t+1), time refers to t+1
rate_em <- rate_emigration %>%
    as.data.frame() %>%
    rename(rate_em = value)

covar_df <- count_bth %>%
    inner_join(count_dth, by = "time") %>%
    inner_join(rate_im, by = "time") %>%
    inner_join(rate_em, by = "time") %>%
    mutate(time = time - 1)

## supply values for 2020, to silence warning
## covar_df  <- bind_rows(covar_df, tail(covar_df, n = 1)) %>%
##     mutate(time = seq(from = min(time), length.out = n()))

covar <- covar_df %>%
    covariate_table(times = "time")


rinit <- function(N0, ...) {
    c(N = N0, im = 0, em = 0, btho = 0, n0 = 0, n1 = 0, rateim = 0)
}

## Step function takes inputs where "time" variable equals t
## and returns outputs where "time" variable equals t+1
step.fun <- function(N, bth, dth, rate_im, rate_em, ...) {
    n0 <- N
    im <- rpois(n = 1, lambda = rate_im)
    exposure <- (N + 0.5 * (bth - dth + im)) / (1 + 0.5 * rate_em)
    em <- rpois(n = 1, lambda = rate_em * exposure)
    N <- N + bth - dth + im - em
    n1 <- N
    c(N = N, im = im, em = em, btho = bth, n0 = n0, n1 = n1, rateim = rate_im)
}

rprocess <- onestep(step.fun)

dmeasure <- function(log, YN, N, Yim, im, Yem, em, ...) {
    dnorm(YN, mean = N, sd = 0.02 * N, log = log) +
        dnorm(Yim, mean = im, sd = 0.02 * im, log = log) + 
        dnorm(Yem, mean = em, sd = 0.02 * em, log = log)
}


params <- c(N0 = 56615)


sim5 <- pfilter(data = data,
                Np = 1000,
                times = 2011:2020,
                 t0 = 2010,
                 rinit = rinit,             
                 params = params,
                rprocess = rprocess,
                dmeasure = dmeasure,
                covar = covar,
                filter.traj = TRUE,
                filter.mean = TRUE)
res5 <- as.data.frame(sim5)




res5 %>%
    select(-1) %>%
    mutate(errN = 100 * (YN / filter.mean.N - 1),
           errim = 100 * (Yim / filter.mean.im - 1),
           errem = 100 * (Yem / filter.mean.em - 1)) %>%
    round(1)


bd <- rbind(bth = c(0, as.numeric(datasets$reg_births)),
            dth = c(0, as.numeric(datasets$reg_deaths)))


acc <- filter.traj(sim5) %>%
    as.data.frame() %>%
    as.matrix() %>%
    rbind(bd)
acc[1,-1] - (acc[1,-11] + acc[2,-1] - acc[3,-1] + acc[4,-1] - acc[5,-1])



res5 %>%
    mutate(diff = N - (lag(N) + lag(bth) - lag(dth) + im - em))

