
.PHONY: all
all: out/reg_popn_df.rds out/reg_births_df.rds out/reg_deaths_df.rds



## Make input datasets --------------------------------------------------------

## Pull data from Statistics Greenland website

out/pxweb_nonbirths.rds: src/pxweb_nonbirths.R
	Rscript $<


out/pxweb_births.rds: src/pxweb_births.R
	Rscript $<


## Tidy non-births data

out/nonbirths.rds: src/nonbirths.R \
  out/pxweb_nonbirths.rds
	Rscript $<


## Make data frames to hold registered population,
## registered births, registered deaths, registered
## immigration, and registered emigration

data/reg_popn_df.csv: src/reg_popn_df.R \
  out/nonbirths.rds
	Rscript $<

data/reg_births_df.csv: src/reg_births_df.R \
  out/pxweb_births.rds
	Rscript $<

data/reg_deaths_df.csv: src/reg_deaths_df.R \
  out/nonbirths.rds
	Rscript $<

data/reg_immigration_df.csv: src/reg_immigration_df.R \
  out/nonbirths.rds
	Rscript $<

data/reg_emigration_df.csv: src/reg_emigration_df.R \
  out/nonbirths.rds
	Rscript $<


## 



## Clean up -------------------------------------------------------------------

.PHONY: clean
clean:
	rm -rf out
	mkdir out



