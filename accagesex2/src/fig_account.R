
## avoid loading 'pomp', which causes name conflicts
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2) 


prob <- c(0.025, 0.5, 0.975)

model <- readRDS("out/model.rds")

datasets <- readRDS("out/datasets.rds")

initial_year <- 2010

states <- model %>%
    mutate(states = map(model, pomp::saved.states), ## using 'pomp' function here
           states = map(states, function(x) map(x, t)),
           states = map(states, function(x) map(x, data.frame)),
           states = map(states, bind_rows, .id = "k")) %>%
    select(birth_cohort, sex, states) %>%
    unnest(states) %>%
    select(-exposure) %>%
    pivot_longer(cols = c(N, im, em)) %>%
    group_by(birth_cohort, sex, k, name) %>%
    summarise(lower = quantile(value, prob[1]),
              median = quantile(value, prob[2]),
              upper = quantile(value, prob[3])) %>%
    ungroup() %>%
    mutate(k = as.integer(k)) %>%
    arrange(birth_cohort, sex, k)
    

data_popn_model <- states %>%
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
    select(-c(name, birth_cohort, k))


data_popn_raw <- datasets$reg_popn %>%
    as.data.frame() %>%
    rename(raw = count)

data_popn <- data_popn_model %>%
    inner_join(data_popn_raw, by = c("age", "sex", "time"))


make_popn <- function(time_inner) {
    data_inner <- filter(data_popn, time == time_inner)
    ggplot(data_inner, aes(x = age, group = sex)) +
        facet_wrap(vars(sex),
                   ncol = 2) +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      col = "darkorange") +
        geom_point(aes(y = median),
                   col = "darkorange") +
        geom_line(aes(y = median),
                  col = "darkorange",
                  size = 0.1) +
        geom_point(aes(y = raw),
                   col = "darkblue",
                   shape = 4) +
        ylab("") +
        ggtitle(sprintf("Population %d", time_inner))
}                                         


data_comp_model <- states %>%
    filter(name != "N") %>%
    mutate(born_before_est_period = birth_cohort <= initial_year) %>%
    mutate(time = if_else(born_before_est_period,
                          initial_year + (k - 1) %/% 2 + 1,
                          birth_cohort + k %/% 2),
           age = if_else(born_before_est_period,
                         initial_year - birth_cohort + (k %/% 2L),
                         ((k - 1L) %/% 2))) %>%
    select(age, sex, time, component = name, lower, median, upper) %>%
    pivot_longer(cols = c(lower, median, upper)) %>%
    count(age, sex, time, component, name, wt = value) %>%
    pivot_wider(names_from = name, values_from = n)

data_comp_raw <- datasets[c("reg_immigration", "reg_emigration")] %>%
    map(as.data.frame) %>%
    set_names(nm = c("im", "em")) %>%
    bind_rows(.id = "component") %>%
    count(component, age, sex, time, wt = count, name = "raw")

data_comp <- data_comp_model %>%
    inner_join(data_comp_raw, by = c("component", "age", "sex", "time")) %>%
    mutate(component = factor(component,
                              levels = c("im", "em"),
                              labels = c("Immigration", "Emigration")))
    

make_comp <- function(comp, time_inner) {
    data_inner <- filter(data_comp,
                         component == comp,
                         time == time_inner)
    ggplot(data_inner, aes(x = age, group = sex)) +
        facet_wrap(vars(sex),
                   ncol = 2) +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      col = "darkorange") +
        geom_point(aes(y = median),
                   col = "darkorange") +
        geom_line(aes(y = median),
                  col = "darkorange",
                  size = 0.1) +
        geom_point(aes(y = raw),
                   col = "darkblue",
                   shape = 4) +
        ylab("") +
        ggtitle(sprintf("%s %d", comp, time_inner))
}    


## Plot

graphics.off()
pdf(file = "out/fig_account.pdf",
    onefile = TRUE,
    width = 12,
    height = 12)
for (time_inner in unique(data_popn$time)) {
    p <- make_popn(time_inner = time_inner)
    plot(p)
}
for (comp in levels(data_comp$component)) {
    for (time_inner in unique(data_comp$time)) {
        p <- make_comp(comp = comp,
                       time_inner = time_inner)
        plot(p)
    }
}
dev.off()

