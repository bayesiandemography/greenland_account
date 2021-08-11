
library(pomp) ## have to load first, because of namespace conflicts
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2) 


prob <- c(0.025, 0.5, 0.975)

model <- readRDS("out/model.rds")

datasets <- readRDS("out/datasets.rds")


## Extract estimated states from model and create quantiles.
## Unfortunately, it does not seem to be possible to
## extract weights from a 'pomp' object, which is likely
## to reduce the accuracy of the quantiles slightly.

states <- model %>%
    saved.states() %>%
    map(t) %>%
    map(as.data.frame) %>%
    bind_rows(.id = "time") %>%
    pivot_longer(c(N, im, em)) %>%
    group_by(time, name) %>%
    summarise(lower = quantile(value, prob = prob[1]),
              median = quantile(value, prob = prob[2]),
              upper = quantile(value, prob = prob[3])) %>%
    ungroup() %>%
    mutate(time = as.integer(time))

              
## Population

data_popn_model <- states %>%
    filter(name == "N")

data_popn_raw <- datasets$reg_popn %>%
    as.data.frame() %>%
    rename(raw = count)

data_popn <- inner_join(data_popn_model, data_popn_raw, by = "time")


## Immigration

data_im_model <- states %>%
    filter(name == "im")

data_im_raw <- datasets$reg_immigration %>%
    as.data.frame() %>%
    rename(raw = count)

data_im <- inner_join(data_im_model, data_im_raw, by = "time")


## Emigration

data_em_model <- states %>%
    filter(name == "em")

data_em_raw <- datasets$reg_emigration %>%
    as.data.frame() %>%
    rename(raw = count)

data_em <- inner_join(data_em_model, data_em_raw, by = "time")


## Combine

data <- bind_rows(data_popn, data_im, data_em) %>%
    mutate(name = factor(name,
                         levels = c("N", "im", "em"),
                         labels = c("Population", "Immigration", "Emigration")))


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

