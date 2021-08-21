
## Create 'reg_immigration' data frame from 'pxweb_nonbirths'

library(dplyr)
library(assertr)
library(readr)
library(tidyr)

pxweb_nonbirths <- readRDS("out/pxweb_nonbirths.rds")

reg_immigration_region <- pxweb_nonbirths %>%
    filter(event == "In migration") %>%
    mutate(age = time - cohort - (triangle == "Upper")) %>%
    mutate(is_valid_age = !is.na(age) & (age >= 0)) %>%
    verify(is_valid_age | (count == 0L)) %>%
    filter(is_valid_age) %>%
    mutate(age = ifelse(age >= 100, "100+", age),
           age = factor(age, levels = c(0:99, "100+"))) %>%
    count(age, triangle, sex, region, time, wt = count, name = "count") %>%
    complete(age, triangle, sex, region, time, fill = list(count = 0L)) %>%
    arrange(time, region, sex, age, triangle)

write_csv(reg_immigration_region,
          file = "../data/reg_immigration_region.csv")
