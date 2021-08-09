
library(demest)
library(dplyr)
library(tidyr)
library(ggplot2) 


prob <- c(0.025, 0.5, 0.975)

## Population

data_popn_model <- fetch("out/model.est", where = c("account", "population")) %>%
    collapseIterations(prob = prob) %>%
    as.data.frame() %>%
    pivot_wider(names_from = quantile, values_from = count) %>%
    rename(lower = "2.5%", median = "50%", upper = "97.5%")

data_popn_raw <- fetch("out/model.est", where = c("datasets", "reg_popn")) %>%
    as.data.frame() %>%
    rename(raw = count)

data_popn <- inner_join(data_popn_model, data_popn_raw, by = "time") %>%
    mutate(name = "Population")


## Immigration

data_im_model <- fetch("out/model.est", where = c("account", "immigration")) %>%
    collapseIterations(prob = prob) %>%
    as.data.frame() %>%
    pivot_wider(names_from = quantile, values_from = count) %>%
    rename(lower = "2.5%", median = "50%", upper = "97.5%")

data_im_raw <- fetch("out/model.est", where = c("datasets", "reg_immigration")) %>%
    as.data.frame() %>%
    rename(raw = count)

data_im <- inner_join(data_im_model, data_im_raw, by = "time") %>%
    mutate(name = "Immigration")


## Emigration

data_em_model <- fetch("out/model.est", where = c("account", "emigration")) %>%
    collapseIterations(prob = prob) %>%
    as.data.frame() %>%
    pivot_wider(names_from = quantile, values_from = count) %>%
    rename(lower = "2.5%", median = "50%", upper = "97.5%")

data_em_raw <- fetch("out/model.est", where = c("datasets", "reg_emigration")) %>%
    as.data.frame() %>%
    rename(raw = count)

data_em <- inner_join(data_em_model, data_em_raw, by = "time") %>%
    mutate(name = "Emigration")


## Combine

data <- bind_rows(data_popn, data_im, data_em) %>%
    mutate(name = factor(name,
                         levels = c("Population", "Immigration", "Emigration")))


p <- ggplot(data, aes(x = time)) +
    facet_wrap(vars(name),
               ncol = 1,
               scales = "free_y") +
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
    ylab("")


## Plot

graphics.off()
pdf(file = "out/fig_account.pdf",
    paper = "a4")
plot(p)
dev.off()

