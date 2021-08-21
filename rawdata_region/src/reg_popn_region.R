
## Create 'reg_popn' data frame from 'nonbirths'

## Holds population at the *end* of each year

library(dplyr)
library(assertr)
library(readr)
library(tidyr)

pxweb_nonbirths <- readRDS("out/pxweb_nonbirths.rds")

head <- pxweb_nonbirths %>%
    filter(event == "Population (start of year)") %>%
    filter(time == min(time)) %>%
    mutate(time = time - 1L) ## convert to end of year

tail <- pxweb_nonbirths %>%
    filter(event == "Population (end of year)")

reg_popn_region <- bind_rows(head, tail) %>%
    verify(triangle == "Lower" | count == 0) %>% ## count is 0 if triangle is Upper
    filter(triangle == "Lower") %>%
    filter(time >= cohort) %>%
    mutate(age = time - cohort) %>%
    verify(!is.na(age) & age >= 0) %>%
    mutate(age = ifelse(age >= 100, "100+", age),
           age = factor(age, levels = c(0:99, "100+"))) %>%
    count(age, sex, region, time, wt = count, name = "count") %>%
    complete(age, sex, region, time, fill = list(count = 0L)) %>%
    arrange(time, region, sex, age)
           
write_csv(reg_popn_region,
          file = "../data/reg_popn_region.csv")
