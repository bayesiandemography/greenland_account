
## Read data directly from Statistics Greenland website using 'pxweb'

library(pxweb)
library(dplyr)

url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BEXCALCR.PX"

query <- list("year of birth" = as.character(1900:2020),
              "place of birth" = "T",
              event = c("P","B","I","O","D","T","F","C","U"),
              area = c("955","956","957","959","960","961"),
              gender = c("K", "M"),
              "triangles(lexis)" = c("0", "1"),
              time = as.character(2011:2021))

pxweb <- pxweb_get(url = url,
                   query = query) %>%
    as.data.frame(column.name.type = "text",
                  variable.value.type = "text") %>%
    tibble() %>%
    select(cohort = "year of birth",
           event,
           region = area,
           sex = gender,
           triangle = "triangles(lexis)",
           time,
           count = "Population Account") %>%
    filter(!is.na(count)) %>%
    filter(cohort <= time) %>%
    mutate(cohort = as.integer(cohort),
           time = as.integer(time),
           count = as.integer(count))


saveRDS(pxweb,
        file = "out/pxweb.rds")
