
library(dplyr)
library(tidyr)
library(ggplot2)


vals_old <- readRDS("vals_old.rds")
vals_new <- readRDS("vals_new.rds")

vals_old <- vals_old %>%
    mutate(variant = "Old method")

vals_new <- vals_new %>%
    mutate(variant = "New method")

vals_model <- bind_rows(vals_old, vals_new) %>%
    filter(time == 2020,
           sex == "Female",
           region == "Kommune Kujalleq")

p <- ggplot(vals_model, aes(x = age, color = variant)) +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_point(aes(y = median)) +
    scale_color_manual(values = c("Old method" = "darkblue",
                                  "New method" = "darkorange")) +
    ylab("") +
    xlab("Age") +
    theme(legend.title = element_blank()) +
    ggtitle("Female population, by single year of age, in Kommune Kujalleq, 2015")
p


graphics.off()
png(file = "fig_greenland.png",
    width = 1800,
    height = 1000,
    res = 200)
plot(p)
dev.off()

