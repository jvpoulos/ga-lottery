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
cores <- 8 # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Set data directory 
data.directory <-"/home/ubuntu/ga-lottery/"

# Load data
load(paste0(data.directory,"lottery.RData"))

# Run randomization tests?
patient.random <- TRUE

# Run heterogeneous effects models?
patient.het <- TRUE

# Produce descriptive statistics and maps
source(paste0(data.directory,"descriptive-stats.R"))
source(paste0(data.directory,"county-maps.R"))
source(paste0(data.directory,"tax-records.R"))

## Define functions for analyses

EstAte <- function(y,treat,w,beta.hat=NULL){ 
  # Calculate weighted difference-in-means.
  #
  # Args:
  #   y: Response vector.
  #   treat: Treatment assignment vector.
  #   w: Vector of weights. 
  #   beta.hat: The fraction of compliers in the experimental population. Default is NULL (ITT).
  #
  # Returns:
  #   ITT weighted difference-in-means or TOT weighted difference-in-means if beta.hat supplied.
  if(is.null(beta.hat)){ 
    return(weighted.mean(y[treat==1],w[treat==1]) - weighted.mean(y[treat==0],w[treat==0]))  
  } else{ 
    return((weighted.mean(y[treat==1],w[treat==1]) - weighted.mean(y[treat==0],w[treat==0]))/beta.hat) 
  }
} 

PermutationTest<-function(y,treat,w,p.score,L=10000,alternative="two.sided",allow.parallel=TRUE,workers=cores){
  # Calculate randomization p value for ITT weighted difference-in-means.
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
  # Bootstrap weighted means for response y for each treatment group 
  itt.diff <- bootstrap(y, EstAte,R=R, args.stat=list(treat,w[treat])) #ITT
  tot.diff <- bootstrap(y, EstAte,R=R, args.stat=list(treat,w[treat],beta.hat)) #TOT
  # Calculate percentiles of the differences in bootstrapped means
  itt.per <- CI.percentile(itt.diff, probs = c(0.025, 0.975))
  tot.per <- CI.percentile(tot.diff, probs = c(0.025, 0.975))
  # Calculate observed differences in means
  meandif.itt <- EstAte(y,treat,w) 
  meandif.tot <- EstAte(y,treat,w,beta.hat)
  # Make table for estimates
  res.itt <- c(meandif.itt, itt.per) 
  res.tot <- c(meandif.tot, tot.per)
  res <- rbind(res.itt,res.tot) 
  colnames(res) <- c('Mean Difference','.025','.975') 
  rownames(res) <- c('ITT','TOT') 
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


## Prepare 1805 lottery data 

# Create dummy for never-treat
lot05$rgb <- 0 
lot05$rgb[c(grep("RGB",lot05$grant.book.x),grep("RGB",lot05$grant.book.y))] <- 1

# Calculate compliance rate
beta.hat <- 1-(sum(lot05$rgb)/sum(lot05$treat))

# Merge officeholder info with lottery data
resp.dat <- merge(lot05[c("row.no","county","treat","treat2","prize","prize2","orphan","widow","rgb","n.draw","no.slaves.1820")],
                  officeholders[c("row.no","prior.office","slave.index","match","oh","n.post.terms")],by="row.no", all.x=TRUE)

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

## Run balance tests and plots

source(paste0(data.directory,"balance-tests.R"))
source(paste0(data.directory,"balance-plot.R")) 
source(paste0(data.directory,"qq-plot.R")) 

## Create table showing outcomes by treatment group & compliance status
my.stats <- list("n", "min", "mean", "max", "s") 
print(tableContinuous(vars = resp.dat[c("no.slaves.1820")],
                      group = resp.dat$treat + resp.dat$rgb, 
                      prec = 3,
                      cumsum=FALSE,
                      stats=my.stats))

print(tableContinuous(vars = sub.prior[c("n.post.terms","slave.index")], 
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
                        y = c(oh.CI[1],oh.CI[2],
                              slavery.CI[1],slavery.CI[2],
                              term.CI[1],term.CI[2],
                              slaves.CI[1],slaves.CI[2]),
                        y.lo = c(oh.CI[3],oh.CI[4],slavery.CI[3],slavery.CI[4],term.CI[3],term.CI[4],slaves.CI[3],slaves.CI[4]),
                        y.hi = c(oh.CI[5],oh.CI[6],slavery.CI[5],slavery.CI[6],term.CI[5],term.CI[6],slaves.CI[5],slaves.CI[6]))
plot.data <- transform(plot.data, y.lo = y.lo, y.hi=y.hi)
plot.data$Outcome <- c(rep("Officeholding",2),
                       rep("Slavery legislation",2),
                       rep("# terms after lottery",2),
                       rep("# slaves held (1820)",2))

# Plot forest plot
plot.data$x <- factor(plot.data$x, levels=rev(plot.data$x)) # reverse order
summary.plot <- ForestPlot(plot.data,xlab="Treatment effect",ylab="Analysis")

ggsave(paste0(data.directory,"summary-plot.pdf"), summary.plot, width=8.5, height=11)

## Create heterogeneous treatment effect plots 
if(patient.het){
  source(paste0(data.directory,"SuperLearner.R"))
  source(paste0(data.directory,"het-effects.R"))
}

## Save  data
save.image(paste0(data.directory,"analysis.RData"))
