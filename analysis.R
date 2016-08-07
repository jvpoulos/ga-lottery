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
require(AER)
require(memisc)
require(grid)
require(gridExtra)
require(gtools)
require(Hmisc)
require(scales)
require(stringr)
require(splines)
require(parallel)
require(doParallel)

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Set data directory 
data.directory <-"/home/ubuntu/github/ga-lottery/"

# Load data
load(paste0(data.directory,"ga-lottery.RData"))

# Run descriptive analyses?
patient.descriptive <- FALSE

# Run balance tests?
patient.balance <- FALSE

# Run randomization tests?
patient.random <- FALSE

# Run heterogeneous effects models?
patient.het <- TRUE

# Produce descriptive statistics and maps
source(paste0(data.directory,"descriptive-stats.R"))
source(paste0(data.directory,"county-maps.R"))

# Define functions for analyses and plots
source(paste0(data.directory,"utils.R"))

## Prepare 1805 lottery data 
source(paste0(data.directory,"prepare.R"))

## Create table showing outcomes by treatment group & compliance status
if(patient.descriptive){ 
my.stats <- list("n", "min", "mean", "max", "s") 
print(tableContinuous(vars = resp.dat[c("slave.wealth.1820")],
                      group = resp.dat$treat + resp.dat$rgb, 
                      prec = 3,
                      cumsum=FALSE,
                      stats=my.stats))

print(tableContinuous(vars = sub.prior[c("n.post.terms","slave.index","bank.index")], 
                      group = sub.prior$treat + sub.prior$rgb, 
                      prec = 3,
                      cumsum=FALSE,
                      stats=my.stats))

print(tableContinuous(vars = sub.oh[c("oh","match.oh")], 
                      group = sub.oh$treat + sub.oh$rgb, 
                      prec = 3,
                      cumsum=FALSE,
                      stats=my.stats))

print(tableContinuous(vars = sub.candidate[c("candidate")], 
                      group = sub.candidate$treat + sub.candidate$rgb, 
                      prec = 3,
                      cumsum=FALSE,
                      stats=my.stats))
}

## ITT/TOT analyses for all outcomes

# Results for candidacy
if(patient.random){
  perm.candidate <- PermutationTest(y=sub.candidate$candidate,
                                    treat=sub.candidate$treat,
                                    w=sub.candidate$weight,
                                    p.score=sub.candidate$p.score) 
  print(perm.candidate$p)
}

candidate.CI <- BootDiff(y=sub.candidate$candidate,
                         treat=sub.candidate$treat,
                         w=sub.candidate$weight,
                         beta.hat=beta.hat)

# Results for officeholding
if(patient.random){
  perm.oh <- PermutationTest(y=sub.oh$oh,
                             treat=sub.oh$treat,
                             w=sub.oh$weight,
                             p.score=sub.oh$p.score) 
  print(perm.oh$p)
}

oh.CI <- BootDiff(y=sub.oh$oh,
                  treat=sub.oh$treat,
                  w=sub.oh$weight,
                  beta.hat=beta.hat)

# Permutation results of slavery legislation
if(patient.random){
  perm.slavery <- PermutationTest(y=sub.prior[!is.na(sub.prior$slave.index),]$slave.index,
                                  treat=sub.prior[!is.na(sub.prior$slave.index),]$treat,
                                  w=sub.prior[!is.na(sub.prior$slave.index),]$weight,
                                  p.score=sub.prior[!is.na(sub.prior$slave.index),]$p.score)
  print(perm.slavery$p)
}

slavery.CI <- BootDiff(y=sub.prior[!is.na(sub.prior$slave.index),]$slave.index,
                       treat=sub.prior[!is.na(sub.prior$slave.index),]$treat,
                       w=sub.prior[!is.na(sub.prior$slave.index),]$weight,
                       beta.hat=beta.hat)

# Permutation results of banking legislation
if(patient.random){
  perm.bank <- PermutationTest(y=sub.prior[!is.na(sub.prior$bank.index),]$bank.index,
                                  treat=sub.prior[!is.na(sub.prior$bank.index),]$treat,
                                  w=sub.prior[!is.na(sub.prior$bank.index),]$weight,
                                  p.score=sub.prior[!is.na(sub.prior$bank.index),]$p.score)
  print(perm.bank$p)
}

bank.CI <- BootDiff(y=sub.prior[!is.na(sub.prior$bank.index),]$bank.index,
                       treat=sub.prior[!is.na(sub.prior$bank.index),]$treat,
                       w=sub.prior[!is.na(sub.prior$bank.index),]$weight,
                       beta.hat=beta.hat,
                    sc=15) # more smoothing

# Permutation results for term 
if(patient.random){
  perm.term <- PermutationTest(y=sub.prior$n.post.terms,
                               treat=sub.prior$treat,
                               w=sub.prior$weight,
                               p.score=sub.prior$p.score) 
  print(perm.term$p)
}

term.CI <- BootDiff(y=sub.prior$n.post.terms,
                     treat=sub.prior$treat,
                     w=sub.prior$weight,
                     beta.hat=beta.hat)
                    
# Permutation results for slaves 
if(patient.random){
  perm.slaves <- PermutationTest(y=resp.dat[!is.na(resp.dat$no.slaves.1820),]$no.slaves.1820,
                                 treat=resp.dat[!is.na(resp.dat$no.slaves.1820),]$treat,
                                 w=resp.dat[!is.na(resp.dat$no.slaves.1820),]$weight,
                                 p.score=resp.dat[!is.na(resp.dat$no.slaves.1820),]$p.score) 
  print(perm.slaves$p)
}

slaves.CI <- BootDiff(y=resp.dat[!is.na(resp.dat$no.slaves.1820),]$no.slaves.1820,
                      treat=resp.dat[!is.na(resp.dat$no.slaves.1820),]$treat,
                      w=resp.dat[!is.na(resp.dat$no.slaves.1820),]$weight,
                      beta.hat=beta.hat)

## Create summary plot for ATEs

# Create data for plot
plot.data <- data.frame(x = c("ITT","TOT"),
                        y = c(bank.CI[1],bank.CI[2],
                              candidate.CI[1],candidate.CI[2],
                              oh.CI[1],oh.CI[2],
                              slavery.CI[1],slavery.CI[2],
                              slaves.CI[1],slaves.CI[2],
                              term.CI[1],term.CI[2]),
                        y.lo = c(bank.CI[3],bank.CI[4],candidate.CI[3],candidate.CI[4],oh.CI[3],oh.CI[4],slavery.CI[3],slavery.CI[4],slaves.CI[3],slaves.CI[4],term.CI[3],term.CI[4]),
                        y.hi = c(bank.CI[5],bank.CI[6],candidate.CI[5],candidate.CI[6],oh.CI[5],oh.CI[6],slavery.CI[5],slavery.CI[6],slaves.CI[5],slaves.CI[6],term.CI[5],term.CI[6]))
plot.data <- transform(plot.data, y.lo = y.lo, y.hi=y.hi)
plot.data$Outcome <- c(rep(paste("Banking legislation, N =", 
                                 format(nrow(sub.prior[!is.na(sub.prior$bank.index),]),big.mark=",",scientific=FALSE,trim=TRUE)),2),
                       rep(paste("Candidacy, N =", 
                                 format(nrow(sub.candidate),big.mark=",",scientific=FALSE,trim=TRUE)),2),
                       rep(paste("Officeholding, N =",
                                 format(nrow(sub.oh),big.mark=",",scientific=FALSE,trim=TRUE)),2),
                       rep(paste("Slavery legislation, N =", 
                                 format(nrow(sub.prior[!is.na(sub.prior$slave.index),]),big.mark=",",scientific=FALSE,trim=TRUE)),2),
                       rep(paste("# Slaves, N =", 
                                 format(nrow(resp.dat[!is.na(resp.dat$no.slaves.1820),]),big.mark=",",scientific=FALSE,trim=TRUE)),2),
                       rep(paste("# Terms, N=", 
                                     format(nrow(sub.prior),big.mark=",",scientific=FALSE,trim=TRUE)),2))


# Plot forest plot
plot.data$x <- factor(plot.data$x, levels=rev(plot.data$x)) # reverse order
plot.data$Outcome <- factor(plot.data$Outcome, levels=plot.data$Outcome) # reverse order
summary.plot <- ForestPlot(plot.data,xlab="Treatment effect",ylab="Analysis")

ggsave(paste0(data.directory,"summary-plot.pdf"), summary.plot, width=8.5, height=11)

## Create heterogeneous treatment effect plots 
if(patient.het){
  source(paste0(data.directory,"SuperLearner.R"))
  source(paste0(data.directory,"het-effects.R"))
}

## Save  data
save.image(paste0(data.directory,"analysis.RData"))
