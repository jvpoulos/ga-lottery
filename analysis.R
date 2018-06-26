# Packages
require(reldist)
require(stats)
require(gdata)
require(ggplot2)
require(plyr)
require(dplyr)
require(weights)
require(scales)
require(ifultools)
require(splines)
require(MASS)
require(resample)
require(reporttools)
require(xtable)
require(reshape2)
require(memisc)
require(grid)
require(gridExtra)
require(gtools)
require(Hmisc)
require(scales)
require(stringr)
require(splines)
require(quantreg)
require(wesanderson)

# Set data directory 
data.directory <-"~/Dropbox/github/ga-lottery/"

# Load data
load(paste0(data.directory,"ga-lottery.RData"))

# Run descriptive analyses?
patient.descriptive <- FALSE

# Run robustness tests?
patient.robust <- FALSE

# Run quantile regression plots?
patient.qreg <- FALSE

# Run power analysis?
patient.power <- FALSE

# Define functions for analyses and plots
source(paste0(data.directory,"utils.R"))

# Produce descriptive statistics and maps
source(paste0(data.directory,"descriptive-stats.R"))
source(paste0(data.directory,"county-maps.R"))

## Prepare lottery data 
source(paste0(data.directory,"prepare.R"))

## Run balance tests and plot (Fig. 1)

source(paste0(data.directory,"balance-tests.R"))
source(paste0(data.directory,"balance-plot.R")) 

# Table 1: summary statistics (including pre-treatment variables, political outcomes, and wealth outcomes)
source(paste0(data.directory,"summary-table.R"))

## ITT analyses for officeholding 
## ITT analyses for candidacy 

# Results for candidacy

# 1805 winners and losers
candidate.lm <- lm(sub.oh.05$candidate~sub.oh.05$treat + sub.oh.05$n.draw)
candidate.CI <- list("CI" = confint(candidate.lm, "sub.oh.05$treat")[1:2],
                     "ATE" = candidate.lm$coefficients['sub.oh.05$treat'][[1]])

print(candidate.CI)

# 1805 winners
candidate.lm.winners <- lm(sub.oh.05.winners$candidate~sub.oh.05.winners$treat + sub.oh.05.winners$n.draw)
candidate.CI.winners <- list("CI" = confint(candidate.lm.winners, "sub.oh.05.winners$treat")[1:2],
                             "ATE" = candidate.lm.winners$coefficients['sub.oh.05.winners$treat'][[1]])

print(candidate.CI.winners)

# 1807 winners
candidate.lm.winners.07 <- lm(sub.oh.07.winners$candidate~sub.oh.07.winners$treat + sub.oh.07.winners$n.draw)
candidate.CI.winners.07 <- list("CI" = confint(candidate.lm.winners.07, "sub.oh.07.winners$treat")[1:2],
                             "ATE" = candidate.lm.winners.07$coefficients['sub.oh.07.winners$treat'][[1]])

print(candidate.CI.winners.07)

# Results for officeholding

# 1805 winners and losers
oh.lm <- lm(sub.oh.05$oh~sub.oh.05$treat + sub.oh.05$n.draw)
oh.CI <- list("CI" = confint(oh.lm, "sub.oh.05$treat")[1:2],
              "ATE" = oh.lm$coefficients['sub.oh.05$treat'][[1]])

print(oh.CI)

# 1805 winners
oh.lm.winners <- lm(sub.oh.05.winners$oh~sub.oh.05.winners$treat + sub.oh.05.winners$n.draw)
oh.CI.winners <- list("CI" = confint(oh.lm.winners, "sub.oh.05.winners$treat")[1:2],
              "ATE" = oh.lm.winners$coefficients['sub.oh.05.winners$treat'][[1]])

print(oh.CI.winners)

# 1807 winners
oh.lm.winners.07 <- lm(sub.oh.07.winners$oh~sub.oh.07.winners$treat + sub.oh.07.winners$n.draw)
oh.CI.winners.07 <- list("CI" = confint(oh.lm.winners.07, "sub.oh.07.winners$treat")[1:2],
                      "ATE" = oh.lm.winners.07$coefficients['sub.oh.07.winners$treat'][[1]])

print(oh.CI.winners.07)

# Results for officeholding match

# 1805 winners and losers
match.oh.lm <- lm(sub.oh.05$match.oh~sub.oh.05$treat + sub.oh.05$n.draw)
match.oh.CI <- list("CI" = confint(match.oh.lm, "sub.oh.05$treat")[1:2],
              "ATE" = match.oh.lm$coefficients['sub.oh.05$treat'][[1]])

print(match.oh.CI)

# 1805 winners
match.oh.lm.winners <- lm(sub.oh.05.winners$match.oh~sub.oh.05.winners$treat + sub.oh.05.winners$n.draw)
match.oh.CI.winners <- list("CI" = confint(match.oh.lm.winners, "sub.oh.05.winners$treat")[1:2],
                    "ATE" = match.oh.lm.winners$coefficients['sub.oh.05.winners$treat'][[1]])

print(match.oh.CI.winners)

# 1807 winners
match.oh.lm.winners.07 <- lm(sub.oh.07.winners$match.oh~sub.oh.07.winners$treat + sub.oh.07.winners$n.draw)
match.oh.CI.winners.07 <- list("CI" = confint(match.oh.lm.winners.07, "sub.oh.07.winners$treat")[1:2],
                            "ATE" = match.oh.lm.winners.07$coefficients['sub.oh.07.winners$treat'][[1]])

print(match.oh.CI.winners.07)


## ITT analyses for slave wealth 

# 1805 winners and losers
slaves.lm <- lm(sub.1820.05$slave.wealth.1820~sub.1820.05$treat + sub.1820.05$n.draw)
slaves.CI <- list("CI" = confint(slaves.lm, "sub.1820.05$treat")[1:2],
                           "ATE" = slaves.lm$coefficients['sub.1820.05$treat'][[1]])

print(slaves.CI)

# 1805 winners
slaves.lm.winners <- lm(sub.1820.05.winners$slave.wealth.1820~sub.1820.05.winners$treat + sub.1820.05.winners$n.draw)
slaves.CI.winners <- list("CI" = confint(slaves.lm.winners, "sub.1820.05.winners$treat")[1:2],
                  "ATE" = slaves.lm.winners$coefficients['sub.1820.05.winners$treat'][[1]])

print(slaves.CI.winners)

# 1807 winners
slaves.lm.winners.07 <- lm(sub.1820.07.winners$slave.wealth.1820~sub.1820.07.winners$treat + sub.1820.07.winners$n.draw)
slaves.CI.winners.07 <- list("CI" = confint(slaves.lm.winners.07, "sub.1820.07.winners$treat")[1:2],
                          "ATE" = slaves.lm.winners.07$coefficients['sub.1820.07.winners$treat'][[1]])

print(slaves.CI.winners.07)

## Robustness: slave wealth (for OA)
if(patient.robust){
  source(paste0(data.directory,"slave-wealth-robust.R"))
}

## Create quantile regression plot (for OA)
if(patient.qreg){
  source(paste0(data.directory,"qreg-plot.R"))
}

## Run power analysis (for OA)
if(patient.power){
  source(paste0(data.directory,"power.R"))
}

## Save data
save.image(paste0(data.directory,"results/analysis.RData"))
