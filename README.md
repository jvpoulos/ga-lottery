Data and code for "Wealth, Officeholding, and Elite Ideology in Antebellum Georgia"

#Contents
* R code and writeup (*lab4-stat215a.Rnw*)
* .tex file of writeup and output (*lab4-stat215a.tex*)
* .pdf of report (*lab4-stat215a.pdf*)

#How to reproduce report
* Clone a copy of the repository to your working directory with the command
```
$ git clone https://github.com/jvpoulos/stat215a-lab4
```
* Open .Rnw file 
* Verify that all **R** packages in the `libraries` chunk are installed on your local machine
* Change the file path on line 80 to the local directory of the three image .txt files
* Knit the document and compile the .pdf using the following two commands
```
$ Rscript -e "library(knitr); knit('stat215a-lab4.Rnw')"
$ pdflatex stat215a-lab4.tex
```
