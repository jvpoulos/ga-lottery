# Packages
require(gdata)
require(ggplot2)
require(plyr)
require(dplyr)
require(weights)
require(scales)
require(ifultools)
require(splines)
require(MASS)
require(boot)
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

# Load record-link data
load("lottery.RData")

# Set data directory 
data.directory <-"/home/ubuntu/ga-lottery/"

# Run randomization tests?
patient.random <- TRUE

if(patient.random){
  require(parallel)
  require(doParallel)
  
  gc(reset=TRUE) # reset garbage collect
  
  cores <- 8 # specify number of cores to use for parallel execution
  
  registerDoParallel(cores) # register cores for parallel processing
  
  RNGkind("L'Ecuyer-CMRG") # ensure random number generation for parallel processing
}

# Run heterogeneous effects models?
patient.het <- TRUE

# Produce descriptive statistics and maps?
patient.desc <- TRUE

if(patient.desc){
  source(paste0(data.directory,"descriptive-stats.R"))
  source(paste0(data.directory,"county-maps.R"))
}

## Define functions for analyses

EstAte <- function(y,treat,w){
  # Calculate ATE using weighted difference-in-means.
  #
  # Args:
  #   y: Response vector.
  #   treat: Treatment assignment vector.
  #   w: Vector of weights. 
  #
  # Returns:
  #   Average treatment effect (ITT).
  return(weighted.mean(y[treat==1],w[treat==1]) - weighted.mean(y[treat==0],w[treat==0]))  
}

PermutationTest<-function(y,treat,w,p.score,L=10000,alternative="two.sided",allow.parallel=TRUE,workers=cores){
  # Calculate randomization p value.
  #
  # Args:
  #   y: Vector of non-missing responses.
  #   treat: Vector of non-missing treatment assignments (0/1). Must be equal in length to y and the sum must be nonzero. 
  #   w: Vector of non-missing weights. Must be equal in length to y. 
  #   p.score: Vector of non-missing propensity scores. Must be equal in length to y. 
  #   L: Number of iterations for the permutation. Default is L=10000. 
  #   alternative: Character string specifying alternative hypothesis. Must be one of "two.sided" (default), "greater" or "less". 
  #   allow.parallel: Character string specifying whether to use parallel backend. Default is TRUE.
  #   workers: Number of workers used for parallel execution. Default is set to number of cores.
  #
  # Returns:
  #   Randomization p value. 
  # Error handling
  if (sum(is.na(y))>0){
    stop("y contains missing values.")
  }
  if (sum(is.na(treat))>0 | length(treat)!=length(y)){
    stop("treat must be equal in length to y and contain non-missing values.")
  }
  if (sum(treat)==0){
    stop("Treatment group is empty.")
  }
  if (sum(treat)==length(treat)){
    warning("Control group is empty.")
  }
  if (sum(is.na(w))>0 | length(w)!=length(y)){
    stop("w must be equal in length to y and contain non-missing values.")
  }
  if (sum(is.na(p.score))>0 | length(p.score)!=length(y)){
    stop("p.score must be equal in length to y and contain non-missing values.")
  }
  # Apply permutation test L times
  if(allow.parallel){
    new.t.stats <- mclapply(1:L, function(i){
      # Create permutation assignment vector 
      treat.perm <-sample(c(rep(1,sum(treat)), rep(0,length(treat) - sum(treat))), 
                          length(treat),
                          prob=w, 
                          replace=FALSE) 
      # Create permutation weight vector
      w.perm <- (treat.perm)/(p.score) + (1-treat.perm)/(1-p.score)
      # Calculate permutation test statistic
      return(EstAte(y,treat.perm,w.perm))
    }, 
    mc.set.seed=FALSE,
    mc.cores=workers)
  }
  else{
    new.t.stats <- lapply(1:L, function(i){
      # Create permutation assignment vector 
      treat.perm <-sample(c(rep(1,sum(treat)), rep(0,length(treat) - sum(treat))), 
                          length(treat),
                          prob=w, 
                          replace=FALSE) 
      # Create permutation weight vector
      w.perm <- (treat.perm)/(p.score) + (1-treat.perm)/(1-p.score)
      # Calculate permutation test statistic
      return(EstAte(y,treat.perm,w.perm))
    })
  }
  new.t.stats <- unlist(new.t.stats)
  # Calculate p value
  if (alternative=="two.sided"){
    pvalue <- sum(abs(new.t.stats) >= abs(EstAte(y,treat,w)))/L 
  }
  if (alternative=="greater"){
    pvalue <- sum(new.t.stats > EstAte(y,treat,w))/L
  }
  if (alternative=="less"){
    pvalue <- sum(new.t.stats < EstAte(y,treat,w))/L
  }
  # Return p-value and permutation vectors
  return(list("p" = pvalue, "perm.t.stats" = new.t.stats))
}

BootDiff<- function(y,treat,w,R=10000,beta.hat) {
  # Function to compute 95% confidence interval for the weighted difference in two means.
  #
  # Args:
  #   y: Response vector.
  #   treat: Treatment vector
  #   w: Vector of weights.
  #   R: The number of bootstrap replicates. The default is 1,000. 
  #   beta.hat: The fraction of compliers in the experimental population.
  #
  # Returns:
  #   Vector containing weighted difference-in-means, and 95% nonparametric CI.
  treat <- as.factor(treat) 
  # Bootstrap weighted means for response y for each treatment group (A and B)
  a <- boot(y[treat==0], weighted.mean, R, w=w[treat==0])$t
  b <- boot(y[treat==1], weighted.mean, R, w=w[treat==1])$t
  # ITT
  meandif <- EstAte(y,treat,w) # calculate observed differences in means
  a.b <- quantile(b-a, c(.025,.975)) # calculate percentiles of the differences in bootstrapped means
  res.ITT <- c(meandif, a.b)
  # TOT
  meandif.TOT <- EstAte(y,treat,w)/beta.hat
  a.b.TOT <- quantile(b-a, c(.025,.975))/beta.hat
  res.TOT <- c(meandif.TOT, a.b.TOT)
  res <- rbind(res.ITT,res.TOT)
  colnames(res) <- c('Mean Difference','.025','.975')
  rownames(res) <- c('ITT','TOT')
  return(res)
}

BootDiffHet <- function(tau.i,g) {
  # Function to compute 95% confidence intervals for heterogeneous treatment effect plots. 
  #
  # Args:
  #   tau.i: Vector of bserved individual treatment effects. 
  #   g: Binary grouping variable. 
  #
  # Returns:
  #   Vector containing weighted difference-in-means, and 95% nonparametric CI.
  g <- as.factor(g[[1]])
  a <- attr(smean.cl.boot(tau.i[g==0],reps=TRUE),'reps')  # bootsrapped conditional mean differences
  b <- attr(smean.cl.boot(tau.i[g==1],reps=TRUE),'reps')  # default is R=1000
  meandif <- mean(tau.i[g==1]) - mean(tau.i[g==0]) # observed conditional mean difference
  a.b <- quantile(b-a, c(.025,.975))
  res <- c(meandif, a.b)
  names(res) <- c('Mean Difference','.025','.975')
  return(res)
}

# Forest plot for summary figure
ForestPlot <- function(d, xlab, ylab){
  p <- ggplot(d, aes(x=x, y=y, ymin=y.lo, ymax=y.hi,colour=Outcome)) + 
    geom_pointrange(size=1, alpha=0.8) + 
    coord_flip() +
    geom_hline(aes(x=0), lty=2) +
    theme(legend.position="none") +
    facet_grid(Outcome~.) +
    ylab(xlab) +
    xlab(ylab) #switch because of the coord_flip() above
  return(p)
}

## Run balance tests and plots

if(patient.random){
source(paste0(data.directory,"balance-tests.R"))
source(paste0(data.directory,"balance-plot.R")) 
}

source(paste0(data.directory,"qq-plot.R")) 

## Prepare 1805 lottery data 

# Create dummy for never-treat
lot05$rgb <- 0 
lot05$rgb[c(grep("RGB",lot05$grant.book.x),grep("RGB",lot05$grant.book.y))] <- 1

# Calculate compliance rate
beta.hat <- 1-(sum(lot05$rgb)/sum(lot05$treat))

# Merge officeholder info with lottery data
resp.dat <- merge(lot05[c("row.no","county","treat","treat2","prize","prize2","orphan","widow","rgb","match","n.draw","no.slaves.1820")],
                  officeholders[c("row.no","prior.office","slave.index","bank.index","oh","n.post.terms")],by="row.no", all.x=TRUE)

resp.dat$prior.office[is.na(resp.dat$prior.office)] <- 0 # make binary
resp.dat$oh[is.na(resp.dat$oh)] <- 0

# Count # of prizes
prizes <- sum(resp.dat$treat) + sum(resp.dat$treat2)

# Count # tickets in box
tickets <- nrow(resp.dat) + nrow(resp.dat[resp.dat$n.draw==2,])

# Calculate P(Z=1) 
resp.dat$p.score <- ifelse(resp.dat$n.draw==2,2*(prizes/tickets),prizes/tickets) # calculated using all participants

# Create column of weights 
resp.dat$weight <- (resp.dat$treat)/(resp.dat$p.score) + (1-resp.dat$treat)/(1-resp.dat$p.score)

# Create sample exclusions
sub.oh <- resp.dat[(resp.dat$prior.office!=1) & (resp.dat$orphan!=1) & (resp.dat$widow!=1),] # exclude orphans, widows, pretreatment officeholders
sub.prior <- resp.dat[(resp.dat$prior.office==1),] # only pretreatment officeholders

## Create table showing outcomes by treatment group & compliance status
my.stats <- list("n", "min", "mean", "max", "s") 
print(tableContinuous(vars = resp.dat[c("slave.index","bank.index","no.slaves.1820")], # no sample exclusions
                      group = resp.dat$treat + resp.dat$rgb, 
                      prec = 3,
                      cumsum=FALSE,
                      stats=my.stats))

print(tableContinuous(vars = sub.prior[c("n.post.terms")], 
                      group = sub.prior$treat + sub.prior$rgb, 
                      prec = 3,
                      cumsum=FALSE,
                      stats=my.stats))

print(tableContinuous(vars = sub.oh[c("oh")], 
                      group = sub.oh$treat + sub.oh$rgb, 
                      prec = 3,
                      cumsum=FALSE,
                      stats=my.stats))

## ITT/TOT analyses for all outcomes

# Permutation results for OH 
if(patient.random){
  perm.oh <- PermutationTest(y=sub.oh$oh,
                             treat=sub.oh$treat,
                             w=sub.oh$weight,
                             p.score=sub.oh$p.score) 
  perm.oh$p
}

oh.CI <- BootDiff(y=sub.oh$oh,
                  treat=sub.oh$treat,
                  w=sub.oh$weight,
                  beta.hat=beta.hat)

oh.CI

# Permutation results of slavery legislation
if(patient.random){
  perm.slavery <- PermutationTest(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                                  treat=resp.dat[!is.na(resp.dat$slave.index),]$treat,
                                  w=resp.dat[!is.na(resp.dat$slave.index),]$weight,
                                  p.score=resp.dat[!is.na(resp.dat$slave.index),]$p.score)
  perm.slavery$p  
}

slavery.CI <-   BootDiff(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                         treat=resp.dat[!is.na(resp.dat$slave.index),]$treat,
                         w=resp.dat[!is.na(resp.dat$slave.index),]$weight,
                         beta.hat=beta.hat)

slavery.CI

# Permutation results of bank votes
if(patient.random){
  perm.bank <- PermutationTest(y=resp.dat[!is.na(resp.dat$bank.index),]$bank.index,
                               treat=resp.dat[!is.na(resp.dat$bank.index),]$treat,
                               w=resp.dat[!is.na(resp.dat$bank.index),]$weight,
                               p.score=resp.dat[!is.na(resp.dat$bank.index),]$p.score)
  perm.bank$p  
}

bank.CI <-   BootDiff(y=resp.dat[!is.na(resp.dat$bank.index),]$bank.index,
                      treat=resp.dat[!is.na(resp.dat$bank.index),]$treat,
                      w=resp.dat[!is.na(resp.dat$bank.index),]$weight,
                      beta.hat=beta.hat)

bank.CI

# Permutation results for term 
if(patient.random){
  perm.term <- PermutationTest(y=sub.prior$n.post.terms,
                               treat=sub.prior$treat,
                               w=sub.prior$weight,
                               p.score=sub.prior$p.score) 
  perm.term$p
}

term.CI <-   BootDiff(y=sub.prior$n.post.terms,
                      treat=sub.prior$treat,
                      w=sub.prior$weight,
                      beta.hat=beta.hat)

term.CI

# Permutation results for slaves 
if(patient.random){
  perm.slaves <- PermutationTest(y=resp.dat[!is.na(resp.dat$no.slaves.1820),]$no.slaves.1820,
                                 treat=resp.dat[!is.na(resp.dat$no.slaves.1820),]$treat,
                                 w=resp.dat[!is.na(resp.dat$no.slaves.1820),]$weight,
                                 p.score=resp.dat[!is.na(resp.dat$no.slaves.1820),]$p.score) 
  perm.slaves$p
}

slaves.CI <-   BootDiff(y=resp.dat[!is.na(resp.dat$no.slaves.1820),]$no.slaves.1820,
                        treat=resp.dat[!is.na(resp.dat$no.slaves.1820),]$treat,
                        w=resp.dat[!is.na(resp.dat$no.slaves.1820),]$weight,
                        beta.hat=beta.hat)

slaves.CI

## Create summary plot for ATEs

# Create data for plot
plot.data <- data.frame(x = c("ITT","TOT"),
                        y = c(oh.CI[1],oh.CI[2],
                              slavery.CI[1],slavery.CI[2],
                              bank.CI[1],bank.CI[2],
                              term.CI[1],term.CI[2],
                              slaves.CI[1],slaves.CI[2]),
                        y.lo = c(oh.CI[3],oh.CI[4],slavery.CI[3],slavery.CI[4],bank.CI[3],bank.CI[4],term.CI[3],term.CI[4],slaves.CI[3],slaves.CI[4]),
                        y.hi = c(oh.CI[5],oh.CI[6],slavery.CI[5],slavery.CI[6],bank.CI[5],bank.CI[6],term.CI[5],term.CI[6],slaves.CI[5],slaves.CI[6]))
plot.data <- transform(plot.data, y.lo = y.lo, y.hi=y.hi)
plot.data$Outcome <- c(rep("Officeholding",2),
                       rep("Slavery legislation",2),
                       rep("State banking policy",2),
                       rep("# terms after lottery",2),
                       rep("# slaves held (1820)",2))
# Plot forest plot
summary.plot <- ForestPlot(plot.data,xlab="Treatment effect",ylab="Analysis")

ggsave(paste0(data.directory,"summary-plot.pdf"), summary.plot, width=11, height=8.5)

## Create heterogeneous treatment effect plots 
if(patient.het){
  source(paste0(data.directory,"SuperLearner.R"))
  source(paste0(data.directory,"het-effects.R"))
}

## Sensitivity analyses for slavery legislation

# Vote index weighted by match probability 
resp.dat$w.slave.index <- resp.dat$slave.index*resp.dat$match

if(patient.random){
  perm.slavery.match <- PermutationTest(y=resp.dat[!is.na(resp.dat$w.slave.index),]$w.slave.index,
                                        treat=resp.dat[!is.na(resp.dat$w.slave.index),]$treat,
                                        w=resp.dat[!is.na(resp.dat$w.slave.index),]$weight,
                                        p.score=resp.dat[!is.na(resp.dat$w.slave.index),]$p.score)
  perm.slavery.match$p 
}

slavery.match.CI <-   BootDiff(y=resp.dat[!is.na(resp.dat$w.slave.index),]$w.slave.index,
                               treat=resp.dat[!is.na(resp.dat$w.slave.index),]$treat,
                               w=resp.dat[!is.na(resp.dat$w.slave.index),]$weight,
                               beta.hat=beta.hat)

slavery.match.CI

# Two treatments: 1 prize
resp.dat$treat1 <- resp.dat$treat - resp.dat$treat2 # win exactly 1 prize

resp.dat$weight.treat1 <- (resp.dat$treat1)/(resp.dat$p.score) + (1-resp.dat$treat1)/(1-resp.dat$p.score) # create new weight vectors

beta.hat.treat1 <- 1-(sum(lot05$rgb[grep("RGB",lot05$grant.book.x)])/sum(resp.dat$treat1)) # calculate new compliance rate

if(patient.random){ # 1 prize
  perm.slavery.treat1 <- PermutationTest(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                                         treat=resp.dat[!is.na(resp.dat$slave.index),]$treat1,
                                         w=resp.dat[!is.na(resp.dat$slave.index),]$weight.treat1,
                                         p.score=resp.dat[!is.na(resp.dat$slave.index),]$p.score)
  perm.slavery.treat1$p 
}

slavery.treat1.CI <-   BootDiff(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                                treat=resp.dat[!is.na(resp.dat$slave.index),]$treat1,
                                w=resp.dat[!is.na(resp.dat$slave.index),]$weight.treat1,
                                beta.hat=beta.hat.treat1)

slavery.treat1.CI

# Three treatments: treatment is winning at least 1 prize in given county
resp.dat$treat.Ba <- ifelse(substr(resp.dat$prize,1,2)=="Ba" | substr(resp.dat$prize2,1,2)=="Ba",1,0) # create new treatment dummies
resp.dat$treat.Ba[is.na(resp.dat$treat.Ba)] <- 0
resp.dat$treat.Wa <- ifelse(substr(resp.dat$prize,1,2)=="Wa" | substr(resp.dat$prize2,1,2)=="Wa",1,0)
resp.dat$treat.Wa[is.na(resp.dat$treat.Wa)] <- 0
resp.dat$treat.Wi <- ifelse(substr(resp.dat$prize,1,2)=="Wi" | substr(resp.dat$prize2,1,2)=="Wi",1,0)
resp.dat$treat.Wi[is.na(resp.dat$treat.Wi)] <- 0

resp.dat$weight.Ba <- (resp.dat$treat.Ba)/(resp.dat$p.score) + (1-resp.dat$treat.Ba)/(1-resp.dat$p.score) # create new weight vectors
resp.dat$weight.Wa <- (resp.dat$treat.Wa)/(resp.dat$p.score) + (1-resp.dat$treat.Wa)/(1-resp.dat$p.score)
resp.dat$weight.Wi <- (resp.dat$treat.Wi)/(resp.dat$p.score) + (1-resp.dat$treat.Wi)/(1-resp.dat$p.score)

beta.hat.Ba <- 1-(sum(resp.dat$rgb)/sum(resp.dat$treat.Ba)) # calculate new compliance rates
beta.hat.Wa <- 1-(sum(resp.dat$rgb)/sum(resp.dat$treat.Wa)) 
beta.hat.Wi <- 1-(sum(resp.dat$rgb)/sum(resp.dat$treat.Wi)) 

if(patient.random){ # Ba
  perm.slavery.Ba <- PermutationTest(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                                     treat=resp.dat[!is.na(resp.dat$slave.index),]$treat.Ba,
                                     w=resp.dat[!is.na(resp.dat$slave.index),]$weight.Ba,
                                     p.score=resp.dat[!is.na(resp.dat$slave.index),]$p.score)
  perm.slavery.Ba$p  
}

slavery.Ba.CI <-   BootDiff(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                            treat=resp.dat[!is.na(resp.dat$slave.index),]$treat.Ba,
                            w=resp.dat[!is.na(resp.dat$slave.index),]$weight.Ba,
                            beta.hat=beta.hat.Ba)

slavery.Ba.CI

if(patient.random){ # Wa
  perm.slavery.Wa <- PermutationTest(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                                     treat=resp.dat[!is.na(resp.dat$slave.index),]$treat.Wa,
                                     w=resp.dat[!is.na(resp.dat$slave.index),]$weight.Wa,
                                     p.score=resp.dat[!is.na(resp.dat$slave.index),]$p.score)
  perm.slavery.Wa$p  
}

slavery.Wa.CI <-   BootDiff(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                            treat=resp.dat[!is.na(resp.dat$slave.index),]$treat.Wa,
                            w=resp.dat[!is.na(resp.dat$slave.index),]$weight.Wa,
                            beta.hat=beta.hat.Wa)

slavery.Wa.CI

if(patient.random){ # Wi
  perm.slavery.Wi <- PermutationTest(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                                     treat=resp.dat[!is.na(resp.dat$slave.index),]$treat.Wi,
                                     w=resp.dat[!is.na(resp.dat$slave.index),]$weight.Wi,
                                     p.score=resp.dat[!is.na(resp.dat$slave.index),]$p.score)
  perm.slavery.Wi$p  
}

slavery.Wi.CI <-   BootDiff(y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index,
                            treat=resp.dat[!is.na(resp.dat$slave.index),]$treat.Wi,
                            w=resp.dat[!is.na(resp.dat$slave.index),]$weight.Wi,
                            beta.hat=beta.hat.Wi)

slavery.Wi.CI

# Create data for plot
s.plot.data <- data.frame(x = c("ITT","TOT"),
                          y = c(slavery.match.CI[1],slavery.match.CI[2],
                                slavery.treat1.CI[1],slavery.treat1.CI[2],
                                slavery.Ba.CI[1],slavery.Ba.CI[2],
                                slavery.Wa.CI[1],slavery.Wa.CI[2],
                                slavery.Wi.CI[1],slavery.Wi.CI[2]),
                          y.lo = c(slavery.match.CI[3],slavery.match.CI[4],slavery.treat1.CI[3],slavery.treat1.CI[4],slavery.Ba.CI[3],slavery.Ba.CI[4],slavery.Wa.CI[3],slavery.Wa.CI[4],slavery.Wi.CI[3],slavery.Wi.CI[4]),
                          y.hi = c(slavery.match.CI[5],slavery.match.CI[6],slavery.treat1.CI[5],slavery.treat1.CI[6],slavery.Ba.CI[5],slavery.Ba.CI[6],slavery.Wa.CI[5],slavery.Wa.CI[6],slavery.Wi.CI[5],slavery.Wi.CI[6]))
s.plot.data <- transform(s.plot.data, y.lo = y.lo, y.hi=y.hi)
s.plot.data$Outcome <- c(rep("Weighted response",2),
                         rep("Treatment: 1 prize",2),
                         rep("Treatment: Baldwin",2),
                         rep("Treatment: Wayne",2),
                         rep("Treatment: Wilkinson",2))
# Plot forest plot
slavery.sensitivity <- ForestPlot(s.plot.data,xlab="Treatment effect",ylab="Analysis")

ggsave(paste0(data.directory,"slavery-sensitivity.pdf"), slavery.sensitivity, width=11, height=8.5)

## Save  data
save.image(paste0(data.directory,"analysis.RData"))
