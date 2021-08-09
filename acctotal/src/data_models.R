
library(demest)

datasets <- readRDS("out/datasets.rds")

use_norm <- TRUE

if (use_norm) {

    mean_popn <- as(datasets$reg_popn, "Values")
    sd_popn <- 0.01 * mean_popn
    mean_popn[] <- 1

    mean_im <- as(datasets$reg_immigration, "Values")
    sd_im <- 0.01 * mean_im
    mean_im[] <- 1

    mean_em <- as(datasets$reg_emigration, "Values")
    sd_em <- 0.01 * mean_em
    mean_em[] <- 1


    mod_popn <- Model(reg_popn ~ NormalFixed(mean = mean_popn, sd = sd_popn),
                      series = "population")

    mod_immigration <- Model(reg_immigration ~ NormalFixed(mean = mean_im, sd = sd_im),
                             series = "immigration")

    mod_emigration <- Model(reg_emigration ~ NormalFixed(mean = mean_em, sd = sd_em),
                            series = "emigration")

} else {

    mod_popn <- Model(reg_popn ~ PoissonBinomial(prob = 0.98),
                      series = "population")
    
    mod_immigration <- Model(reg_immigration ~ PoissonBinomial(prob = 0.98),
                             series = "immigration")

    mod_emigration <- Model(reg_emigration ~ PoissonBinomial(prob = 0.98),
                            series = "emigration")

}


mod_births <- Model(reg_births ~ Exact(),
                    series = "births")

mod_deaths <- Model(reg_deaths ~ Exact(),
                    series = "deaths")


data_models <- list(mod_popn,
                    mod_births,
                    mod_deaths,
                    mod_immigration,
                    mod_emigration)

saveRDS(data_models,
        file = "out/data_models.rds")

