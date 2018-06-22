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
require(wesanderson)

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Set data directory 
data.directory <-"~/Dropbox/github/ga-lottery/"

# Load data
load(paste0(data.directory,"ga-lottery.RData"))

# Run descriptive analyses?
patient.descriptive <- FALSE

# Run balance tests?
patient.balance <- FALSE

# Run heterogeneous effects models?
patient.het <- FALSE

# Define functions for analyses and plots
source(paste0(data.directory,"utils.R"))

# Produce descriptive statistics and maps
source(paste0(data.directory,"descriptive-stats.R"))
source(paste0(data.directory,"county-maps.R"))

## Prepare lottery data 
source(paste0(data.directory,"prepare.R"))

## Create table showing outcomes by treatment group & compliance status
if(patient.descriptive){ 
  my.stats <- list("n", "min", "mean", "max", "s") 
  
  # 1805 winners and losers
  
  print(tableContinuous(vars = sub.oh.05[c("candidate","match.votes.05")], 
                        group = sub.oh.05$treat + sub.oh.05$rgb, 
                        prec = 2,
                        cumsum=FALSE,
                        stats=my.stats))
  
  print(tableContinuous(vars = sub.oh.05[c("oh","match.oh")], 
                        group = sub.oh.05$treat + sub.oh.05$rgb, 
                        prec = 2,
                        cumsum=FALSE,
                        stats=my.stats))
  
  print(tableContinuous(vars = sub.1820.05[c("slave.wealth.1820","slave.wealth.1820.w")],
                        group = sub.1820.05$treat + sub.1820.05$rgb, 
                        prec = 2,
                        cumsum=FALSE,
                        stats=my.stats))
  
  # 1805 winners
  print(tableContinuous(vars = sub.oh.05.winners[c("candidate","match.votes.05")],
                        group = sub.oh.05.winners$treat + sub.oh.05.winners$rgb,
                        prec = 2,
                        cumsum=FALSE,
                        stats=my.stats))

  print(tableContinuous(vars = sub.oh.05.winners[c("oh","match.oh")],
                        group = sub.oh.05.winners$treat + sub.oh.05.winners$rgb,
                        prec = 2,
                        cumsum=FALSE,
                        stats=my.stats))

  print(tableContinuous(vars = sub.1820.05.winners[c("slave.wealth.1820","slave.wealth.1820.w")],
                        group = sub.1820.05.winners$treat + sub.1820.05.winners$rgb,
                        prec = 2,
                        cumsum=FALSE,
                        stats=my.stats))
}

## ITT analyses for officeholding 

# Results for officeholding
if(patient.random){
  # 1805 winners and losers
  # perm.oh <- PermutationTest(y=sub.oh.05$oh,
  #                            treat=sub.oh.05$treat,
  #                            w=sub.oh.05$weight,
  #                            p.score=sub.oh.05$p.score)
  # print(perm.oh$p)
  
  # 1805 winners
  perm.oh.05.winners <- PermutationTest(y=sub.oh.05.winners$oh,
                             treat=sub.oh.05.winners$treat,
                             w=sub.oh.05.winners$weight,
                             p.score=sub.oh.05.winners$p.score)
  print(perm.oh.05.winners$p)
}

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

# Results for officeholding match

if(patient.random){
  # 1805 winners and losers
  # perm.match.oh <- PermutationTest(y=sub.oh.05$match.oh,
  #                            treat=sub.oh.05$treat,
  #                            w=sub.oh.05$weight,
  #                            p.score=sub.oh.05$p.score)
  # print(perm.match.oh$p)
  # 
  # 1805 winners
  perm.match.oh.winners <- PermutationTest(y=sub.oh.05.winners$match.oh,
                                   treat=sub.oh.05.winners$treat,
                                   w=sub.oh.05.winners$weight,
                                   p.score=sub.oh.05.winners$p.score)
  print(perm.match.oh.winners$p)
}

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

## ITT analyses for candidacy 

# Results for candidacy
if(patient.random){
  # 1805 winners and losers
  # perm.candidate <- PermutationTest(y=sub.oh.05$candidate,
  #                            treat=sub.oh.05$treat,
  #                            w=sub.oh.05$weight,
  #                            p.score=sub.oh.05$p.score)
  # print(perm.candidate$p)
  
  # 1805 winners
  perm.candidate.winners <- PermutationTest(y=sub.oh.05.winners$candidate,
                                    treat=sub.oh.05.winners$treat,
                                    w=sub.oh.05.winners$weight,
                                    p.score=sub.oh.05.winners$p.score)
  print(perm.candidate.winners$p)
}

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

# Results for candidacy match
if(patient.random){
  # 1805 winners and losers
  # perm.match.candidate <- PermutationTest(y=sub.oh.05$match.votes.05,
  #                                   treat=sub.oh.05$treat,
  #                                   w=sub.oh.05$weight,
  #                                   p.score=sub.oh.05$p.score)
  # print(perm.match.candidate$p)
  
  # 1805 winners
  perm.match.candidate.winners <- PermutationTest(y=sub.oh.05.winners$match.votes.05,
                                          treat=sub.oh.05.winners$treat,
                                          w=sub.oh.05.winners$weight,
                                          p.score=sub.oh.05.winners$p.score)
  print(perm.match.candidate.winners$p)
}

# 1805 winners and losers
match.candidate.lm <- lm(sub.oh.05$match.votes.05~sub.oh.05$treat + sub.oh.05$n.draw)
match.candidate.CI <- list("CI" = confint(match.candidate.lm, "sub.oh.05$treat")[1:2],
                     "ATE" = match.candidate.lm$coefficients['sub.oh.05$treat'][[1]])

print(match.candidate.CI)

# 1805 winners
match.candidate.lm.winners <- lm(sub.oh.05.winners$match.votes.05~sub.oh.05.winners$treat + sub.oh.05.winners$n.draw)
match.candidate.CI.winners <- list("CI" = confint(match.candidate.lm.winners, "sub.oh.05.winners$treat")[1:2],
                           "ATE" = match.candidate.lm.winners$coefficients['sub.oh.05.winners$treat'][[1]])

print(match.candidate.CI.winners)

## ITT analyses for slave wealth 
# Resuts for slave wealth

if(patient.random){
  # 1805 winners and losers
  # perm.slaves <- PermutationTest(y=sub.1820.05$slave.wealth.1820,
  #                                   treat=sub.1820.05$treat,
  #                                   w=sub.1820.05$weight,
  #                                   p.score=sub.1820.05$p.score)
  # print(perm.slaves$p)
  # 
  # 1805 winners
  perm.slaves.winners <- PermutationTest(y=sub.1820.05.winners$slave.wealth.1820,
                                 treat=sub.1820.05.winners$treat,
                                 w=sub.1820.05.winners$weight,
                                 p.score=sub.1820.05.winners$p.score)
  print(perm.slaves.winners$p)
}

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

# Resuts for slave wealth match
if(patient.random){
  # # 1805 winners and losers
  # perm.match.slaves <- PermutationTest(y=sub.1820.05$slave.wealth.1820.w,
  #                                treat=sub.1820.05$treat,
  #                                w=sub.1820.05$weight,
  #                                p.score=sub.1820.05$p.score)
  # print(perm.match.slaves$p)
  
  # 1805 winners
  perm.match.slaves.winners <- PermutationTest(y=sub.1820.05.winners$slave.wealth.1820.w,
                                       treat=sub.1820.05.winners$treat,
                                       w=sub.1820.05.winners$weight,
                                       p.score=sub.1820.05.winners$p.score)
  print(perm.match.slaves.winners$p)
}

# 1805 winners and losers
match.slaves.lm <- lm(sub.1820.05$slave.wealth.1820.w~sub.1820.05$treat + sub.1820.05$n.draw)
match.slaves.CI <- list("CI" = confint(match.slaves.lm, "sub.1820.05$treat")[1:2],
                  "ATE" = match.slaves.lm$coefficients['sub.1820.05$treat'][[1]])

print(match.slaves.CI)

# 1805 winners
match.slaves.lm.winners <- lm(sub.1820.05.winners$slave.wealth.1820.w~sub.1820.05.winners$treat + sub.1820.05.winners$n.draw)
match.slaves.CI.winners <- list("CI" = confint(match.slaves.lm.winners, "sub.1820.05.winners$treat")[1:2],
                        "ATE" = match.slaves.lm.winners$coefficients['sub.1820.05.winners$treat'][[1]])

print(match.slaves.CI.winners)

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
     data = sub.1820.05)
})

qreg.plot.df <- data.frame("effect" = sapply(qreg.fits, "[[", 1)[2,],
                           "se" = sapply(1:length(taus), 
                                    function(x) summary(qreg.fits[[x]], se="boot")[["coefficients"]][,2][[2]]),
                           "quantile"= taus)

qreg.plot <- ggplot(qreg.plot.df[qreg.plot.df$quantile <=0.951,], aes(y=effect, x=quantile)) + 
  geom_pointrange(aes(ymin = effect-(1.96*se), ymax = effect+(1.96*se)),shape=19, alpha=1/4) + 
  ylab("Treatment effect (1820$)") + 
  xlab("Quantile of slave wealth (1820$)") + 
  stat_smooth(method = "loess",se=FALSE) + 
  scale_y_continuous(labels = comma) 

ggsave(paste0(data.directory,"qreg-plot.pdf"), qreg.plot, width=8.5, height=11)

## Report results

#46%
qreg.plot.df[92,]$effect # effect
qreg.plot.df[92,]$effect - (1.96*qreg.plot.df[92,]$se) # lower 
qreg.plot.df[92,]$effect + (1.96*qreg.plot.df[92,]$se) # lower 

#50%
qreg.plot.df[100,]$effect # effect
qreg.plot.df[100,]$effect - (1.96*qreg.plot.df[100,]$se) # lower 
qreg.plot.df[100,]$effect + (1.96*qreg.plot.df[100,]$se) # lower 

## Save data
save.image(paste0(data.directory,"results/analysis.RData"))
