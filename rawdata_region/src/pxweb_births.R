
## Read data directly from Statistics Greenland website using 'pxweb'

library(pxweb)
library(dplyr)
library(tidyr)
library(assertr)

url <- "http://betabank20.stat.gl/api/v1/en/Greenland/BE/BE80//BEXFERTR.PX"

## database not contain entries for 47 and 49,
## and asking for these ages causes an error
mothers_age <- setdiff(13:50, c(47, 49)) 

query <- list(area = c("955","956","957","959","960","961"),
              "mother's year of birth" = as.character(1950:2006), 
              "mother's age" = as.character(mothers_age),
              gender = c("K", "M"),
              time = as.character(2011:2020))

pxweb_births <- pxweb_get(url = url,
                          query = query) %>%
    as.data.frame(column.name.type = "text",
                  variable.value.type = "text") %>%
    filter(area != "d. Outside municipalities") %>%
    mutate(area = if_else(area == "d. Kommuneq Qeqertalik",
                          "d. Kommune Qeqertalik",
                          area)) %>%
    select(cohort = "mother's year of birth",
           age = "mother's age",
           sex = gender,
           region = area,
           time,
           count = "Live births by Greenland's administrative division") %>%
    mutate(cohort = as.integer(cohort),
           age = as.integer(age),
           time = as.integer(time),
           count = as.integer(count)) %>%
    tibble()


saveRDS(pxweb_births,
        file = "out/pxweb_births.rds")

