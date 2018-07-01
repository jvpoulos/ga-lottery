Reproduce figures, tables, and empirical estimates
======

Contents
------
* `lottery.Rdata` contains the datasets used for the analysis 
* `codebook.txt` describes the `lottery.Rdata` datasets
* `analysis.sh` shell script for `analysis.R`
* `analysis.R` main file; runs descriptive stats, balance tests, and ITT analyses
	* `descriptive-stats.R` create descriptive figures and tables for Online Appendix (OA)
		* `ipums-ga-1850.csv` subset of complete--count Census data (1850)
		* `counties-1800.csv` county--level Census data (1800)
		* `counties-1850.csv` county--level Census data (1850)
		* `counties-1870.csv` county--level Census data (1870)
	* `county-maps.R` create maps of Georgia with 1807 county boundaries
	* `utils.R` defines functions
	* `prepare.R` prepare lottery data for analysis
	* `balance-tests.R` estimates *p* values for balance plot
	* `balance-plot.R` creates Figure 1: balance plot with *p* values
	* `qq-plot.R` creates QQ plots for (for OA)
	* `summary-table.R` create Table 1: summary statistics (including pre-treatment variables, political outcomes, and wealth outcomes)
	* `slave-wealth-robust.R` robustness tests on slave wealth outcome (for OA)
	* `candidate-robust.R` robustness tests on candidacy outcome (for OA)
	* `qreg-plot.R` quantile regression estimates (for OA)
	* `power.R` performs power analysis and outputs plot (for OA)

Instructions
------
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
  * Change `patient.descriptive` , `patient.robust` , `patient.qreg` , or `patient.power` to TRUE to run descriptive and auxilliary analyses
<!--    (N.b.: the total elapsed time of running `analysis.RD` with all vectors set to TRUE on a machine with 8 vCPU is 11.29 hours.)  -->
  * Save your changes to `analysis.R`
* Open `county-maps.R` and verify required packages are installed
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