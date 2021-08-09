
.PHONY: all
all: out/fig_account.pdf

out/datasets.rds: src/datasets.R \
  ../data/reg_popn.csv \
  ../data/reg_births.csv \
  ../data/reg_deaths.csv \
  ../data/reg_immigration.csv \
  ../data/reg_emigration.csv
	Rscript $<

out/data.rds: src/data.R \
  out/datasets.rds
	Rscript $<

out/rates_imem.rds: src/rates_imem.R \
  out/datasets.rds
	Rscript $<

out/covar_df.rds: src/covar_df.R \
  out/datasets.rds \
  out/rates_imem.rds
	Rscript $<

out/model.rds: src/model.R \
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



