

.PHONY: all
all: acctotal \
  acctotal2 \
  accagesex2


## Obtain raw data from Statistics Greenland website,
## and create CSV files in 'data' folder

.PHONY: rawdata
rawdata:
	$(MAKE) -C rawdata


## Estimate account with no age and sex, using 'demest'

.PHONY: acctotal
acctotal:
	$(MAKE) -C acctotal


## Estimate account with no age and sex, using 'pomp'

.PHONY: acctotal2
acctotal2:
	$(MAKE) -C acctotal2


## Estimate account with age and sex, using 'demest'

.PHONY: accagesex
accagesex:
	$(MAKE) -C accagesex


## Estimate account with age and sex, using 'pomp'

.PHONY: accagesex2
accagesex2:
	$(MAKE) -C accagesex2



## Clean up -------------------------------------------------------------------

.PHONY: clean
clean:
	rm -rf rawdata/out
	mkdir rawdata/out
	rm -rf data
	mkdir data
	rm -rf acctotal/out
	mkdir acctotal/out
	rm -rf acctotal2/out
	mkdir acctotal2/out
	rm -rf accagesex/out
	mkdir accagesex/out
	rm -rf accagesex2/out
	mkdir accagesex2/out



