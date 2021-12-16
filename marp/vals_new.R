
## avoid loading 'pomp', which causes name conflicts
library(dplyr)
library(tidyr)
library(purrr)

model <- readRDS("../accreg2/out/model.rds")

initial_year <- 2010

summarise_states <- function(states) {
    prob <- c(0.025, 0.5, 0.975)
    ans <- lapply(states, function(x) apply(x, 1, quantile, prob = prob))
    ans <- lapply(ans, as.data.frame.table)
    ans <- bind_rows(ans, .id = "k")
    ans <- ans[ans$variable != "exposure",]
    ans
}

states <- model %>%
    mutate(states = map(model, pomp::saved.states),
           states = map(states, summarise_states)) %>%
    select(birth_cohort, sex, region, states) %>%
    unnest(states) %>%
    ungroup() %>%
    pivot_wider(names_from = Var1, values_from = Freq) %>%
    rename(lower = "2.5%", median = "50%", upper = "97.5%", name = variable) %>%
    mutate(k = as.integer(k)) %>%
    arrange(birth_cohort, sex, region, k)
    

vals_new.R <- states %>%
    filter(name == "N") %>%
    mutate(born_before_est_period = birth_cohort <= initial_year) %>%
    mutate(is_accession = if_else(born_before_est_period,
                                  k %% 2L == 1L,
                                  k %% 2L == 0L)) %>%
    filter(!is_accession) %>%
    mutate(time = if_else(born_before_est_period,
                          initial_year + (k %/% 2),
                          birth_cohort + (k %/% 2)),
           age = if_else(born_before_est_period,
                         initial_year - birth_cohort + (k %/% 2),
                         (k %/% 2))) %>%
    mutate(is_accession = NULL,
           born_before_est_period = NULL) %>%
    select(-c(name, birth_cohort, k)) %>%
    arrange(age, sex, region, time)


saveRDS(vals_new.R,
        file = "vals_new.rds")




