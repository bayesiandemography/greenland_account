
## avoid loading 'pomp', which causes name conflicts
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2) 




model <- readRDS("out/model.rds")

datasets <- readRDS("out/datasets.rds")

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
    select(-c(name, birth_cohort, k)) %>%
    arrange(age, sex, region, time)


data_popn_raw <- datasets$reg_popn %>%
    as.data.frame() %>%
    rename(raw = count)

data_popn <- data_popn_model %>%
    inner_join(data_popn_raw, by = c("age", "sex", "region", "time"))


make_popn <- function(time_inner) {
    data_inner <- filter(data_popn, time == time_inner)
    ggplot(data_inner, aes(x = age, group = sex)) +
        facet_grid(rows = vars(region), cols = vars(sex), scale = "free_y") +
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
    select(age, sex, region, time, component = name, lower, median, upper) %>%
    pivot_longer(cols = c(lower, median, upper)) %>%
    count(age, sex, region, time, component, name, wt = value) %>%
    pivot_wider(names_from = name, values_from = n)

data_comp_raw_cin <- (datasets$reg_immigration + subarray(datasets$reg_internal, direction == "In")) %>%
    as.data.frame() %>%
    mutate(component = "cin")

data_comp_raw_cout <- (datasets$reg_emigration + subarray(datasets$reg_internal, direction == "Out")) %>%
    as.data.frame() %>%
    mutate(component = "cout")

data_comp_raw <- bind_rows(data_comp_raw_cin, data_comp_raw_cout) %>%
    count(component, age, sex, region, time, wt = count, name = "raw")

data_comp <- data_comp_model %>%
    inner_join(data_comp_raw, by = c("component", "age", "sex", "region", "time")) %>%
    mutate(component = factor(component,
                              levels = c("cin", "cout"),
                              labels = c("Combined inward", "Combined outward")))
    

make_comp <- function(comp, time_inner) {
    data_inner <- filter(data_comp,
                         component == comp,
                         time == time_inner)
    ggplot(data_inner, aes(x = age, group = sex)) +
        facet_grid(rows = vars(region), cols = vars(sex), scale = "free_y") +
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

