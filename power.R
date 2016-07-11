#####################################
### Power analysis                ###
#####################################

require(parallel)
require(doParallel)
require(reshape)
require(ggplot2)

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Set data directory 
data.directory <-"/home/ubuntu/github/ga-lottery/"

# Define functions for analyses and plots
source(paste0(data.directory,"utils.R"))

# Load data
load(paste0(data.directory,"ga-lottery.RData"))

# Prepare 1805 lottery data 
patient.balance <- FALSE
patient.descriptive <- FALSE
source(paste0(data.directory,"prepare.R"))

# Define simulation parameters
alpha <- 0.05
iterations <- 100 # no. iterations 
r.prob <- seq(0.001,0.018,0.001)
s.size <- c(21750) # sample size

# Create grid for parameters
grid.bin <- expand.grid("r.prob"=r.prob, "s.size"=s.size)

SimR <- function(r.prob,s.size,y,treat,w,p.score,L=1000){
  #   r.prob: Effect size (integer).
  #   s.size: Sample size (integer).
  #   y: Vector of non-missing responses.
  #   treat: Vector of non-missing treatment assignments (0/1). Must be equal in length to y and the sum must be nonzero. 
  #   w: Vector of non-missing weights. Must be equal in length to y. 
  #   p.score: Vector of non-missing propensity scores. Must be equal in length to y. 
  #   L: Number of iterations for the permutation. Default is L=1000.
  
  # Simulate data
  design <-data.frame("treat"=sample(treat, s.size, replace=FALSE, prob=p.score),
                      "response"=NA)
  design$response[design$treat==1] <- rbinom(nrow(design[design$treat==1,]), 1, mean(y) + r.prob)
  design$response[design$treat==0] <- rbinom(nrow(design[design$treat==0,]), 1, mean(y))
  
  # Run permutation test
  perm.oh <- PermutationTest(y=design$response,
                             treat=design$treat,
                             w=w,
                             p.score=p.score,
                             L=L)
  # Return p value
  return(perm.oh$p) 
}

p.vals.bin <- replicate(iterations,
                        sapply(1:nrow(grid.bin), function(i){
                          SimR(r.prob=grid.bin$r.prob[i],
                               s.size=grid.bin$s.size[i],
                               y=sub.oh$oh,
                               treat=sub.oh$treat,
                               w=sub.oh$weight,
                               p.score=sub.oh$p.score)}))

p.vals.bin.array <- t(sapply(1:iterations, function(i) array(p.vals.bin[,i])))
saveRDS(p.vals.bin.array, paste0(data.directory,"power_p_values_bin_fixed.rds"))

grid.bin$power <- apply(p.vals.bin.array, 2, function (x) length(which(x < alpha))/iterations)

#Create plots
power.plot.bin <- ggplot(data=grid.bin, aes(x=r.prob, 
                                            y=power)) +
  geom_line() +
  #scale_x_continuous(breaks=r.prob, labels = c("0.025", "0.05", "0.1", "0.2", "0.4", "0.5")) +
  scale_y_continuous(breaks=c(0.25,0.50,0.75,0.8,1), labels = c("25%", "50%", "75%","80%","100%")) +
  geom_hline(yintercept = 0.8, colour="black", linetype = "longdash") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("") +
  ggtitle(rep(paste("Officeholding, N =",
                    format(nrow(sub.oh),big.mark=",",scientific=FALSE,trim=TRUE)),2)) + 
  ThemeBw1()

ggsave(paste0(data.directory,"power-plot-bin.pdf"), power.plot.bin, width=8.5, height=11) 