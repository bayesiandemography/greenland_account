
library(tidyverse)

#install.packages("devtools")

# devtools::install_github("StatisticsNZ/dembase")
library(dembase)

#devtools::install_github("StatisticsNZ/demdata")
library(demdata)

#devtools::install_github("StatisticsNZ/demest", build_vignettes = FALSE)
library(demest)


# devtools::install_github("StatisticsGreenland/statgl")
# library(statgl)
# raw_data_df <-
#   statgl_fetch(statgl_url("BEXCALC",lang = "en"),
#                cohort = px_all("*"), "place of birth" = c("N", "S"),
#                event = px_all("*"), gender = c("M", "K"), "triangles(Lexis)" = px_all("*"),
#                time = px_all("*"), .eliminate_rest = FALSE) %>% #, .col_code = "event", .val_code = "event") 
#   rename("Population Account"=value)

# install.packages("pxweb")
library(pxweb)
raw_data_df <- pxweb_get_data(url = "https://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BEXCALC.PX",
                              query =   list(cohort = "*",
                                             "place of birth" = "T",
                                             gender = c("M", "K"),
                                             "triangles(Lexis)" = "*",
                                             event = "*",
                                             time = "*"))

data_df <- raw_data_df %>%
  select(cohort,
         sex = gender,
         triangle = "triangles(Lexis)",
         event,
         time,
         count = "Population Account") %>%
  filter(count > 0L) %>% # restrict to observed combinations
  mutate(sex = factor(sex, 
                      levels = c("Woman", "Man"),
                      labels = c("Female", "Male"))) %>%
  mutate(cohort = as.integer(cohort),
         time = as.integer(time)) %>%
  mutate(age = if_else(event == "Status ultimo", # uses fact that population counts are at end of year
                       time - cohort,
                       time - cohort - (triangle == "Upper")))

population <- data_df %>%
  filter(event == "Status ultimo") %>%
  dtabs(count ~ age + sex + time) %>%
  Counts(dimscales = c(age = "Intervals", time = "Points"))
plot(population)

births <- data_df %>%
  filter(event == "Birth") %>%
  mutate(age = factor(age, 
                      levels = seq(from = min(age), to = max(age)))) %>% # missing some ages
  dtabs(count ~ age + sex + triangle + time) %>%
  Counts(dimscales = c(age = "Intervals", time = "Intervals"))
plot(births)
## all births have an "Upper" Lexis triangle

deaths <- data_df %>%
  filter(event == "Death") %>%
  mutate(age = factor(age, 
                      levels = seq(from = min(age), to = max(age)))) %>% # missing some ages
  dtabs(count ~ age + sex + triangle + time) %>%
  Counts(dimscales = c(age = "Intervals", time = "Intervals"))
plot(deaths)
## some negative ages

immigration <- data_df %>%
  filter(event == "Immigration") %>%
  mutate(age = factor(age, 
                      levels = seq(from = min(age), to = max(age)))) %>% # missing some ages
  dtabs(count ~ age + sex + triangle + time) %>%
  Counts(dimscales = c(age = "Intervals", time = "Intervals"))
plot(immigration)
## some negative ages

emigration <- data_df %>%
  filter(event == "Emigration") %>%
  mutate(age = factor(age, 
                      levels = seq(from = min(age), to = max(age)))) %>% # missing some ages
  dtabs(count ~ age + sex + triangle + time) %>%
  Counts(dimscales = c(age = "Intervals", time = "Intervals"))
plot(emigration)
## some negative ages

Correction <- data_df %>%
  filter(event == "Correction") %>%
  mutate(age = factor(age, 
                      levels = seq(from = min(age), to = max(age)))) %>% # missing some ages
  dtabs(count ~ age + sex + triangle + time) %>%
  Counts(dimscales = c(age = "Intervals", time = "Intervals"))
plot(Correction)

chk <- as.data.frame(emigration) %>% 
  filter(age==-1) %>% 
  filter(count>0)

## some negative ages
