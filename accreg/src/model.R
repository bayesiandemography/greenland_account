
library(dembase)
library(demest)
library(dplyr)


account <- readRDS("out/account.rds")
system_models <- readRDS("out/system_models.rds")
data_models <- readRDS("out/data_models.rds")
datasets <- readRDS("out/datasets.rds")

filename <- "out/model.est"

set.seed(0)

Sys.time()
estimateAccount(account = account,
                systemModels = system_models,
                datasets = datasets,
                dataModels = data_models,
                filename = filename,
                nBurnin = 200,
                nSim = 200,
                nChain = 4,
                nThin = 5)
Sys.time()

options(width = 120)
fetchSummary(filename)
