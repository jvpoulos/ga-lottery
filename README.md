Reproduce figures, tables, and empirical estimates in "Wealth, Officeholding, and Elite Ideology in Antebellum Georgia"

#Contents
* `analysis.R` creates table showing outcomes by treatment group and compliance status; implements randomization tests; creates summary plot for ATEs; and conducts sensitivity analyses
* `balance-tests.R` estimates randomization *p* values for balance plot
* `balance-plot.R` creates balance plot
* `SuperLearner.R` defines **SuperLearner** libraries
* `het-effects.R` create heterogeneous treatment effect plots and ensemble 
* `descriptive-stats.R` create descriptive statistics figures and tables in Online Appendix
* `county-map.R`

#Instructions
* Clone a copy of the repository to your working directory with the command
```
$ git clone https://github.com/jvpoulos/ga-lottery
```
* The code uses **R** version 3.2.2 (2015-08-14). To install the latest version of R on Ubuntu, use the command 
```
$ sudo apt-get install r-base-core=3.2.2-1trusty0
```
To verify that the most recent version of **R** is installed, type `R` a the command line, and then `R.Version()`
* Verify that all **R** packages in the `libraries` chunk are installed on your local machine
* Change the file path on line 80 to the local directory of the three image .txt files
* Knit the document and compile the .pdf using the following two commands
```
$ Rscript -e "library(knitr); knit('stat215a-lab4.Rnw')"
$ pdflatex stat215a-lab4.tex
```
