
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

counts_dth <- datasets$reg_deaths %>%
    as.data.frame() %>%
    rename(count_dth = count)

counts_bth <- datasets$reg_births %>%
    as.data.frame() %>%
    rename(count_bth = count)

counts_bthdth <- inner_join(counts_bth, counts_dth, by = "time")


## Put together into data frame -----------------------------------------------

rates_imem <- rates %>%
    select(time, rate_im, rate_em)

covar_df <- counts_bthdth %>%
    inner_join(rates_imem, by = "time")

## Subtract 1 from time, because function 'pomp' uses covariates
## with time index 't-1' to predict outcomes with time index 't'

covar_df <- covar_df %>%
    mutate(time = time - 1)


## Save -----------------------------------------------------------------------

saveRDS(covar_df,
        file = "out/covar_df.rds")
