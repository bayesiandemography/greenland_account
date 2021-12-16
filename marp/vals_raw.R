
## avoid loading 'pomp', which causes name conflicts
library(dplyr)
library(dembase)

datasets <- readRDS("../accreg2/out/datasets.rds")

vals_raw <- datasets$reg_popn %>%
    as.data.frame() %>%
    rename(raw = count)

saveRDS(vals_raw,
        file = "vals_raw.rds")
