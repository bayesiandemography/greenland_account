

## Code to create CSV files with Greenland data
## and put the in the 'data' folder

.PHONY: all
all: ../data/reg_popn_region.csv \
  ../data/reg_births_region.csv \
  ../data/reg_deaths_region.csv \
  ../data/reg_immigration_region.csv \
  ../data/reg_emigration_region.csv \
  ../data/reg_internal_region.csv


## Pull data from Statistics Greenland website and tidy -----------------------

out/pxweb_nonbirths.rds: src/pxweb_nonbirths.R
	Rscript $<

out/pxweb_births.rds: src/pxweb_births.R
	Rscript $<


## Create CSVs ----------------------------------------------------------------

../data/reg_popn_region.csv: src/reg_popn_region.R \
  out/pxweb_nonbirths.rds
	Rscript $<

../data/reg_births_region.csv: src/reg_births_region.R \
  out/pxweb_births.rds
	Rscript $<

../data/reg_deaths_region.csv: src/reg_deaths_region.R \
  out/pxweb_nonbirths.rds
	Rscript $<

../data/reg_immigration_region.csv: src/reg_immigration_region.R \
  out/pxweb_nonbirths.rds
	Rscript $<

../data/reg_emigration_region.csv: src/reg_emigration_region.R \
  out/pxweb_nonbirths.rds
	Rscript $<

../data/reg_internal_region.csv: src/reg_internal_region.R \
  out/pxweb_nonbirths.rds
	Rscript $<


## Clean up -------------------------------------------------------------------

.PHONY: clean
clean:
	rm -rf out
	mkdir out

