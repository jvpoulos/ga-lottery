Reproduce figures, tables, and empirical estimates in "Wealth, Officeholding, and Elite Ideology in Antebellum Georgia"

#Contents
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

#Instructions
* Clone a copy of the repository to your working directory with the command

```
$ git clone https://github.com/jvpoulos/ga-lottery
```
* The code uses **R** version 3.2.2 (2015-08-14). To install the latest version of R on Ubuntu, use the command 
```
$ sudo apt-get install r-base-core=3.2.2-1trusty0
````
To verify that the most recent version of **R** is installed, type `R` a the command line, and then
`R.Version()`
* Open `analysis.R` in a script editor
- verify that all required packages are installed in your **R** library
- change the file path specified by character vector `data.directory` to your working directory
- change logical vectors `patient.random` or `patient.het` to FALSE if you wish not to run either randomization tests or heterogeneous effects models because they are computationally costly 
- save your changes to `analysis.R`
*. Run `analysis.R` from the Linux/Unix command line using the command
```
R CMD BATCH analysis.R
```
4. The script will output figures as .pdfs and a .RData file `analysis.RData` that can be loaded into R using the R command
```
load(“analysis.RData”)
``` 
The batch command result analysis.Rout contains the result of the script and LaTeX table output.
