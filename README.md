Reproduce figures, tables, and empirical estimates in "Wealth, Officeholding, and Legislative Ideology"

#Contents
* `ga-lottery-online-appendix.pdf` is the Online Appendix
* `lottery.Rdata` contains the datasets used for the analysis 
* `codebook.txt` describes the `lottery.Rdata` datasets
* `analysis.sh` shell script for `analysis.R`
* `analysis.R` creates table showing outcomes by treatment group and compliance status; implements randomization tests; creates summary plot for ATEs; and creates summary plot for sensitivity analyses
	* `descriptive-stats.R` create descriptive figures and tables for Online Appendix
		* `ipums-ga-1850.csv` subset of complete--count Census data (1850)
		* `counties-1800.csv` county--level Census data (1800)
		* `counties-1850.csv` county--level Census data (1850)
	* `county-maps.R` create maps of Georgia with 1807 county boundaries
	* `utils.R` defines functions
	* `prepare.R` prepare lottery data for analysis
		* `balance-tests.R` estimates randomization *p* values for balance plot; outputs as `balance-tests.txt'
		* `balance-plot.R` creates balance plot
		* `qq-plot.R` creates QQ plots for Online Appendix
		* `tax-records.R` creates pretreatment wealth distribution figures and tables
			* `tax-digests.R` prepares tax digest data (`tax-digests.csv`) 
				* `tax-digests.csv` tax digest data for legislators
	* `SuperLearner.R` defines **SuperLearner** libraries
	* `het-effects.R` create heterogeneous treatment effect plots and ensemble 
* `power.sh` shell script for `power.R`
* `power.R` performs power analysis and outputs plot


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
  * Change logical vector `patient.het` to FALSE if you wish not to run heterogeneous effects models. Change `patient.descriptive', `patient.balance', or `patient.random` to TRUE to run descriptive analyses, balance tests, or randomization tests, resp. (N.b.: the total elapsed time of running `analysis.RD` with all vectors set to TRUE on a machine with 8 vCPU is 11.29 hours.) 
  * Save your changes to `analysis.R`
* Open `SuperLearner.R` and `county-maps.R` and verify required packages are installed
* Make shell file `analysis.sh` executable from the Linux/Unix command line:
```
$ chmod +x analysis.sh
```
* Execute the file:
```
$ ./analysis.sh > analysis.txt
```
* The script will output figures as .pdfs and a .RData file `analysis.RData`.
  * `analysis.txt` contains the result of the script and LaTeX table output.
* Repeat previous steps to run `power.R` via the executable `power.sh`.

