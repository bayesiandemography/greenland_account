
.PHONY: all
all: out/fig_account.pdf

out/datasets.rds: src/datasets.R \
  ../data/reg_popn.csv \
  ../data/reg_births.csv \
  ../data/reg_deaths.csv \
  ../data/reg_immigration.csv \
  ../data/reg_emigration.csv
	Rscript $<



out/fig_account.pdf: src/fig_account.R \
  out/model.est
	Rscript $<

## Clean up -------------------------------------------------------------------

.PHONY: clean
clean:
	rm -rf out
	mkdir out



