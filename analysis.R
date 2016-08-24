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
require(parallel)
require(doParallel)
require(quantreg)

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
patient.het <- FALSE

# Define functions for analyses and plots
source(paste0(data.directory,"utils.R"))

# Produce descriptive statistics and maps
source(paste0(data.directory,"descriptive-stats.R"))
source(paste0(data.directory,"county-maps.R"))

## Prepare 1805 lottery data 
source(paste0(data.directory,"prepare.R"))

## Create table showing outcomes by treatment group & compliance status
if(patient.descriptive){ 
  my.stats <- list("n", "min", "mean", "max", "s") 
  print(tableContinuous(vars = sub.1820[c("slave.wealth.1820")],
                        group = sub.1820$treat + sub.1820$rgb, 
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

## ITT/TOT analyses for officeholding 

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

# Results for officeholding match
match.oh.CI <- BootDiff(y=sub.oh$match.oh,
                        treat=sub.oh$treat,
                        w=sub.oh$weight,
                        beta.hat=beta.hat)

# Results for officeholding (treatment is winning single prize)
one.prize.CI <- BootDiff(y=sub.oh$oh,
                         treat=sub.oh$one.prize,
                         w=sub.oh$weight,
                         beta.hat=1-(length(grep("RGB",lot05$grant.book.x))/sum(lot05$one.prize))) # compliance rate among single-prize winners

# Results for officeholding (treatment is by county of prize)
ba.CI <- BootDiff(y=sub.oh$oh, 
                  treat=sub.oh$ba, # Baldwin county 
                  w=sub.oh$weight,
                  beta.hat=1-(sum(lot05$rgb & lot05$ba)/sum(lot05$ba))) # compliance rate among Baldwin prize winners

wa.CI <- BootDiff(y=sub.oh$oh, 
                  treat=sub.oh$wa, # Wayne county 
                  w=sub.oh$weight,
                  beta.hat=1-(sum(lot05$rgb & lot05$wa)/sum(lot05$wa))) # compliance rate among Wayne prize winners

wi.CI <- BootDiff(y=sub.oh$oh, 
                  treat=sub.oh$wi, # Wayne county 
                  w=sub.oh$weight,
                  beta.hat=1-(sum(lot05$rgb & lot05$wi)/sum(lot05$wi))) # compliance rate among Wilkinson prize winners

## ITT/TOT analyses for candidacy 

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
                  beta.hat=beta.hat,
                  sc=25) # more smoothing

## ITT/TOT analyses for auxilliary outcomes

# Results for slavery legislation
slavery.CI <- BootDiff(y=sub.prior[!is.na(sub.prior$slave.index),]$slave.index,
                       treat=sub.prior[!is.na(sub.prior$slave.index),]$treat,
                       w=sub.prior[!is.na(sub.prior$slave.index),]$weight,
                       beta.hat=beta.hat)

# Results for banking legislation
bank.CI <- BootDiff(y=sub.prior[!is.na(sub.prior$bank.index),]$bank.index,
                    treat=sub.prior[!is.na(sub.prior$bank.index),]$treat,
                    w=sub.prior[!is.na(sub.prior$bank.index),]$weight,
                    beta.hat=beta.hat,
                    sc=25) # more smoothing

# Results for term 
term.CI <- BootDiff(y=range01(sub.prior$n.post.terms), #transform to 0-1 continous variable
                    treat=sub.prior$treat,
                    w=sub.prior$weight,
                    beta.hat=beta.hat,
                    sc=25) # more smoothing

# Results for slave wealth 
if(patient.random){
  perm.slaves <- PermutationTest(y=range01(sub.1820$slave.wealth.1820),
                                    treat=sub.1820$treat,
                                    w=sub.1820$weight,
                                    p.score=sub.1820$p.score) 
  print(perm.slaves$p)
}
slaves.CI <- BootDiff(y=range01(sub.1820$slave.wealth.1820), #transform to 0-1 continous variable
                      treat=sub.1820$treat,
                      w=sub.1820$weight,
                      beta.hat=beta.hat)

## Create summary plots for ATEs

# Create data for plot for auxilliary analyses
plot.data.aux <- data.frame(x = c("ITT","TOT"),
                            y = c(candidate.CI[1],candidate.CI[2],
                                  bank.CI[1],bank.CI[2],
                                  slavery.CI[1],slavery.CI[2],
                                  slaves.CI[1],slaves.CI[2],
                                  term.CI[1],term.CI[2]),
                            y.lo = c(candidate.CI[3],candidate.CI[4],bank.CI[3],bank.CI[4],slavery.CI[3],slavery.CI[4],slaves.CI[3],slaves.CI[4],term.CI[3],term.CI[4]),
                            y.hi = c(candidate.CI[5],candidate.CI[6],bank.CI[5],bank.CI[6],slavery.CI[5],slavery.CI[6],slaves.CI[5],slaves.CI[6],term.CI[5],term.CI[6]))
plot.data.aux <- transform(plot.data.aux, y.lo = y.lo, y.hi=y.hi)
plot.data.aux$Outcome <- c(rep(paste("Candidacy, N=", 
                                     format(nrow(sub.candidate),big.mark=",",scientific=FALSE,trim=TRUE)),2),
                           rep(paste("Banking legislation, N =", 
                                     format(nrow(sub.prior[!is.na(sub.prior$bank.index),]),big.mark=",",scientific=FALSE,trim=TRUE)),2),
                           rep(paste("Slavery legislation, N =", 
                                     format(nrow(sub.prior[!is.na(sub.prior$slave.index),]),big.mark=",",scientific=FALSE,trim=TRUE)),2),
                           rep(paste("Slave wealth, N =", 
                                     format(nrow(resp.dat[!is.na(resp.dat$slave.wealth.1820),]),big.mark=",",scientific=FALSE,trim=TRUE)),2),
                           rep(paste("Terms, N=", 
                                     format(nrow(sub.prior),big.mark=",",scientific=FALSE,trim=TRUE)),2))

# Create data for plot for officeholding and sensitivity analyses
plot.data.oh.sens <- data.frame(x = c("ITT","TOT"),
                           y = c(oh.CI[1],oh.CI[2],
                                 match.oh.CI[1],match.oh.CI[2],
                                 one.prize.CI[1],one.prize.CI[2],
                                 ba.CI[1],ba.CI[2],
                                 wa.CI[1],wa.CI[2],
                                 wi.CI[1],wi.CI[2]),
                           y.lo = c(oh.CI[3],oh.CI[4],
                                    match.oh.CI[3],match.oh.CI[4],
                                    one.prize.CI[3],one.prize.CI[4],
                                    ba.CI[3],ba.CI[4],
                                    wa.CI[3],wa.CI[4],
                                    wi.CI[3],wi.CI[4]),
                           y.hi = c(oh.CI[5],oh.CI[6],
                                    match.oh.CI[5],match.oh.CI[6],
                                    one.prize.CI[5],one.prize.CI[6],
                                    ba.CI[5],ba.CI[6],
                                    wa.CI[5],wa.CI[6],
                                    wi.CI[5],wi.CI[6]))
plot.data.oh.sens <- transform(plot.data.oh.sens, y.lo = y.lo, y.hi=y.hi)
plot.data.oh.sens$Outcome <- c(rep(paste("Officeholding (binary)"),2),
                          rep(paste("Officeholding (match prob.)"),2),
                          rep(paste("Treatment: Single prize"),2),
                          rep(paste("Treatment: Baldwin"),2),
                          rep(paste("Treatment: Wayne"),2),
                          rep(paste("Treatment: Wilkinson"),2))

# Plot forest plots
plot.data.aux$x <- factor(plot.data.aux$x, levels=rev(plot.data.aux$x)) # reverse order
plot.data.aux$Outcome <- factor(plot.data.aux$Outcome, levels=plot.data.aux$Outcome) # reverse order
summary.plot.aux <- ForestPlot(plot.data.aux,xlab="Treatment effect",ylab="Analysis") + scale_y_continuous(labels = percent_format(), 
                                                                                                           limits = c(-0.5,0.5))

plot.data.oh.sens$x <- factor(plot.data.oh.sens$x, levels=rev(plot.data.oh.sens$x)) # reverse order
plot.data.oh.sens$Outcome <- factor(plot.data.oh.sens$Outcome, levels=plot.data.oh.sens$Outcome) # reverse order
summary.plot.oh.sens <- ForestPlot(plot.data.oh.sens,xlab="Treatment effect",ylab="Analysis") + scale_y_continuous(labels = percent_format(), 
                                                                                                         limits = c(-0.025,0.025))
ggsave(paste0(data.directory,"summary-plot-aux.pdf"), summary.plot.aux, width=8.5, height=11)
ggsave(paste0(data.directory,"summary-plot-oh-sens.pdf"), summary.plot.oh.sens, width=8.5, height=11)

## Create heterogeneous treatment effect plots 
if(patient.het){
  source(paste0(data.directory,"SuperLearner.R"))
  source(paste0(data.directory,"het-effects.R"))
}

## Create quantile regression plot
taus <- seq(0.005,0.995,0.005)
qreg.fits <- lapply(taus, function(t){
  rq(formula = slave.wealth.1820 ~ treat, 
     tau = t, 
     data = sub.1820)
})

qreg.plot.df <- data.frame("effect" = sapply(qreg.fits, "[[", 1)[2,],
                           "quantile"= taus)

qreg.plot <- ggplot(qreg.plot.df, aes(y=effect, x=quantile)) + 
  geom_point(shape=19, alpha=1/4) + 
  ylab("Treatment effect (1820$)") + 
  xlab("Quantile of slave wealth (1820$)") + 
  stat_smooth(method = "loess",se=TRUE) + 
  scale_y_continuous(labels = comma,limit=c(-1000,1000))

ggsave(paste0(data.directory,"qreg-plot.pdf"), qreg.plot, width=8.5, height=11)

## Save data
save.image(paste0(data.directory,"analysis.RData"))
