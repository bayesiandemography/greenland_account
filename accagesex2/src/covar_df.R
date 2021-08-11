
## Assemble birth and death counts,
## and immigration and emigration rates,
## into a data frame that will be used
## as covariates in the model.

library(dembase)
library(demest)
library(dplyr)

datasets <- readRDS("out/datasets.rds")

rates <- readRDS("out/rates.rds")


## Assemble birth and death counts --------------------------------------------

## Change refernce of 'sex' from sex of child to sex of parent
counts_bth <- datasets$reg_births %>%
    collapseDimension(dimension = "sex") %>%
    as.data.frame() %>%
    rename(count_bth = count) %>%
    mutate(sex = "Female")

counts_dth <- datasets$reg_deaths %>%
    as.data.frame() %>%
    rename(count_dth = count)

counts_bthdth <- counts_dth %>% # 'counts_dth' has ages and sex that 'counts_bth' does not
    left_join(counts_bth, by = c("age", "triangle", "sex", "time")) %>%
    tibble() %>%
    mutate(birth_cohort = time - age - (triangle == "Upper"),
           born_before_est_period = birth_cohort <= min(time),
           k = if_else(born_before_est_period,
                       2L * (time - min(time)) + (triangle == "Lower") + 1L,
                       2L * age + (triangle == "Upper") + 1L)) %>%
    select(birth_cohort, sex, k, count_bth, count_dth)



## Put together into data frame -----------------------------------------------

covar_df <- inner_join(counts_bthdth, rates,
                       by = c("birth_cohort", "sex", "k")) %>%
    arrange(birth_cohort, sex, k)


## Save -----------------------------------------------------------------------

saveRDS(covar_df,
        file = "out/covar_df.rds")
