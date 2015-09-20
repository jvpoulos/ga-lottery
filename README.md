Reproduce figures, tables, and empirical estimates in "Wealth, Officeholding, and Elite Ideology in Antebellum Georgia." Data sources are described in the [paper](http://ssrn.com/abstract=2484037).

#Contents
* `ga-lottery-online-appendix.pdf` is the Online Appendix
* `lottery.Rdata` contains the datasets used for the analysis 
* `codebook.txt` describes the `lottery.Rdata` datasets
* `analysis.R` creates table showing outcomes by treatment group and compliance status; implements randomization tests; creates summary plot for ATEs; and creates summary plot for sensitivity analyses 
* `balance-tests.R` estimates randomization *p* values for balance plot
* `balance-plot.R` creates balance plot
* `qq-plot.R` creates QQ plots for Online Appendix
* `SuperLearner.R` defines **SuperLearner** libraries
* `het-effects.R` create heterogeneous treatment effect plots and ensemble 
* `tax-records.R` creates pretreatment wealth distribution figures and tables
* `tax-digests.R` prepares tax digest data (`tax-digests.csv`) 
* `tax-digests.csv` tax digest data for legislators
* `descriptive-stats.R` create descriptive figures and tables for Online Appendix
* `ipums-ga-1850.csv` subset of complete--count Census data (1850)
* `counties-1800.csv` county--level Census data (1800)
* `counties-1850.csv` county--level Census data (1850)
* `county-maps.R` create maps of Georgia with 1807 county boundaries
* `GA_AtlasHCB` Georgia county boundary files

#Instructions
* Clone a copy of the repository to your working directory with the command
```
$ git clone https://github.com/jvpoulos/ga-lottery
```
* The code uses **R** version 3.2.2 (2015-08-14). To install the latest version of R on Ubuntu, use the command 
```
$ sudo apt-get install r-base-core=3.2.2-1trusty0
```
* Open `analysis.R` in a script editor
  * Verify that all required packages are installed in your **R** library
  * Change the file path specified by character vector `data.directory` to your working directory
  * Change logical vectors `patient.random` or `patient.het` to FALSE if you wish not to run either randomization tests or heterogeneous effects models. (N.b.: the total elapsed time of running `analysis.RData` with default settings on a machine with 8 vCPU is 11.29 hours.) 
  * Set the numeric vector `cores` to the number of cores to use for parallel execution. You can check the number of cores available with the R command `detectCores()`
  * Save your changes to `analysis.R`
* Open `SuperLearner.R` and `descriptive-stats.R` and verify required packages are installed
* Run `analysis.R` from the Linux/Unix command line using the command
```
$ R CMD BATCH analysis.R
```
* The script will output figures as .pdfs and a .RData file `analysis.RData`.
  * The batch command result `analysis.Rout` contains the result of the script and LaTeX table output.

