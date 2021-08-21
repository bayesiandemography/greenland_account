
## Create 'reg_births' data frame from 'pxweb'

library(dplyr)
library(assertr)
library(readr)
library(tidyr)

pxweb_births <- readRDS("out/pxweb_births.rds")

reg_births <- pxweb_births %>%
    mutate(triangle = case_when(age == time - cohort ~ "Lower",
                                age == time - cohort - 1 ~ "Upper",
                                TRUE ~ "Undefined")) %>%
    verify(!((triangle == "Undefined") & (count > 0))) %>%
    filter(triangle != "Undefined") %>%
    mutate(age = factor(age, levels = seq(from = min(age), to = max(age)))) %>%
    select(age, triangle, sex, region, time, count) %>%
    complete(age, triangle, sex, region, time, fill = list(count = 0L)) %>%
    arrange(time, region, sex, age, triangle)

write_csv(reg_births,
          file = "../data/reg_births_region.csv")
