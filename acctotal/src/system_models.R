
library(demest)

mod_popn <- Model(population ~ Poisson(mean ~ 1,
                                       useExpose = FALSE),
                  `(Intercept)` ~ ExchFixed(mean = log(60000)),
                  jump = 0.003)

mod_births <- Model(births ~ Poisson(mean ~ 1),
                    jump = 0.05)

mod_deaths <- Model(deaths ~ Poisson(mean ~ 1),
                    jump = 0.05)

mod_immigration <- Model(immigration ~ Poisson(mean ~ 1),
                         jump = 0.05)

mod_emigration <- Model(emigration ~ Poisson(mean ~ 1),
                        jump = 0.05)


system_models <- list(mod_popn,
                      mod_births,
                      mod_deaths,
                      mod_immigration,
                      mod_emigration)

saveRDS(system_models,
        file = "out/system_models.rds")

