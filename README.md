
<!-- README.md is generated from README.Rmd. Please edit that file -->

# greenland\_account

Code to make national-level demographic accounts for Greenland,
involving population, births, deaths, immigration, and emigration.

The data come from the Stats Bank online database on the Statistics
Greenland website (via the R package **pxweb**).

The repository contains the following
folders:

| Folder     | Description                                                                                                                                                                                                                   |
| :--------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `rawdata`  | Code to obtain data from the Statistics Greenland website, reformat the data, and create the csv files in the `data-raw`. If your main interest is how to construct a demographic account, you can safely ignore this folder. |
| `data`     | CSV files with data on population, births, deaths, immigration, and emigration                                                                                                                                                |
| `acctotal` | Account with no age-sex detail, where births and deaths data may have some errors                                                                                                                                             |
