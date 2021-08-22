
.PHONY: all
all: out/fig_account.pdf

out/system_models.rds: src/system_models.R
	Rscript $<

out/data_models.rds: src/data_models.R
	Rscript $<

out/datasets.rds: src/datasets.R \
  ../data/reg_popn_region.csv \
  ../data/reg_births_region.csv \
  ../data/reg_deaths_region.csv \
  ../data/reg_immigration_region.csv \
  ../data/reg_emigration_region.csv \
  ../data/reg_internal_region.csv
	Rscript $<

out/account.rds: src/account.R \
  out/datasets.rds
	Rscript $<

out/model.est: src/model.R \
  out/account.rds \
  out/system_models.rds \
  out/data_models.rds \
  out/datasets.rds
	Rscript $<

out/fig_account.pdf: src/fig_account.R \
  out/model.est
	Rscript $<


## Clean up -------------------------------------------------------------------

.PHONY: clean
clean:
	rm -rf out
	mkdir out



