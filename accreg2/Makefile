
.PHONY: all
all: out/fig_account.pdf

out/datasets.rds: src/datasets.R \
  ../data/reg_popn_region.csv \
  ../data/reg_births_region.csv \
  ../data/reg_deaths_region.csv \
  ../data/reg_immigration_region.csv \
  ../data/reg_emigration_region.csv \
  ../data/reg_internal_region.csv
	Rscript $<

out/rates.rds: src/rates.R \
  out/datasets.rds
	Rscript $<

out/N0.rds: src/N0.R \
  out/datasets.rds
	Rscript $<

out/data.rds: src/data.R \
  out/datasets.rds \
  out/rates.rds
	Rscript $<

out/covar_df.rds: src/covar_df.R \
  out/datasets.rds \
  out/rates.rds
	Rscript $<

out/model.rds: src/model.R \
  out/N0.rds \
  out/data.rds \
  out/covar_df.rds
	Rscript $<

out/fig_account.pdf: src/fig_account.R \
  out/datasets.rds \
  out/model.rds
	Rscript $<


## Clean up -------------------------------------------------------------------

.PHONY: clean
clean:
	rm -rf out
	mkdir out



