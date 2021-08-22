
## Create 'reg_immigration' data frame from 'pxweb_nonbirths'

library(dplyr)
library(assertr)
library(readr)
library(tidyr)

pxweb_nonbirths <- readRDS("out/pxweb_nonbirths.rds")

reg_internal_region <- pxweb_nonbirths %>%
    filter(event %in% c("Internal in migration", "Internal out migration")) %>%
    mutate(direction = factor(event,
                              levels = c("Internal out migration", "Internal in migration"),
                              labels = c("Out", "In"))) %>%
    mutate(age = time - cohort - (triangle == "Upper")) %>%
    mutate(is_valid_age = !is.na(age) & (age >= 0)) %>%
    verify(is_valid_age | (count == 0L)) %>%
    filter(is_valid_age) %>%
    mutate(age = ifelse(age >= 100, "100+", age),
           age = factor(age, levels = c(0:99, "100+"))) %>%
    count(age, triangle, sex, region, time, direction, wt = count, name = "count") %>%
    complete(age, triangle, sex, region, time, direction, fill = list(count = 0L)) %>%
    arrange(direction, time, region, sex, age, triangle)

write_csv(reg_internal_region,
          file = "../data/reg_internal_region.csv")
