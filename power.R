#####################################
### Power analysis                ###
#####################################

require(reshape)
require(ggplot2)

# Define simulation parameters
alpha <- 0.05
iterations <- 100 # no. iterations 
r.prob <- seq(0.001,0.05,0.01)
s.size <- c(nrow(sub.oh.05)) # sample size

# Create grid for parameters
grid.bin <- expand.grid("r.prob"=r.prob, "s.size"=s.size)

SimLm <- function(r.prob,s.size,y,treat,n.draw,p.score){
  #   r.prob: Effect size (integer).
  #   s.size: Sample size (integer).
  #   y: Vector of non-missing responses.
  #   treat: Vector of non-missing treatment assignments (0/1). Must be equal in length to y and the sum must be nonzero. 
  #   n.draw: Vector of non-missing number of draws. Must be equal in length to y. 
  #   p.score: Vector of non-missing number of propensity scores. Must be equal in length to y. 
    
  # Simulate data
  design <-data.frame("treat"=sample(treat, s.size, replace=FALSE, prob=p.score),
                      "response"=NA, "n.draw"=NA)
  design$response[design$treat==1] <- rbinom(nrow(design[design$treat==1,]), 1, mean(y) + r.prob)
  design$response[design$treat==0] <- rbinom(nrow(design[design$treat==0,]), 1, mean(y))
  
  # Fit linear model
  lm.oh <- lm(design$response ~ design$treat + n.draw)
  
  # Return p value
  return(summary(lm.oh)$coefficients[11]) 
}

# Calculate p-score 
# Count # of prizes
prizes <- sum(lot05$treat) + sum(lot05$treat2)

# Count # tickets in box
tickets <- nrow(lot05) + nrow(lot05[lot05$n.draw==2,])

# Calculate P(Z=1) 
lot05$p.score <- ifelse(lot05$n.draw==2,2*(prizes/tickets),prizes/tickets) # calculated using all participants

p.vals.bin <- replicate(iterations,
                        sapply(1:nrow(grid.bin), function(i){
                          SimLm(r.prob=grid.bin$r.prob[i],
                               s.size=grid.bin$s.size[i],
                               y=sub.oh.05$oh,
                               treat=sub.oh.05$treat,
                               n.draw = sub.oh.05$n.draw,
                               p.score=lot05[lot05$row.no %in% sub.oh.05$row.no,]$p.score)}))

p.vals.bin.array <- t(sapply(1:iterations, function(i) array(p.vals.bin[,i])))

grid.bin$power <- apply(p.vals.bin.array, 2, function (x) length(which(x < alpha))/iterations)

#Create plots

ThemeBw2 <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
}

power.plot.bin <- ggplot(data=grid.bin, aes(x=r.prob, 
                                            y=power)) +
  geom_line() +
  scale_x_continuous(breaks=r.prob, 
                     labels = percent_format()) +
  scale_y_continuous(breaks=c(0.25,0.50,0.75,0.8,1), labels = c("25%", "50%", "75%","80%","100%")) +
  geom_hline(yintercept = 0.8, colour="black", linetype = "longdash") +
  ylab("Power") +
  xlab("Treatment effect") +
  ggtitle(rep(paste("Officeholding, N =",
                    format(nrow(sub.oh.05),big.mark=",",scientific=FALSE,trim=TRUE)),2)) + 
  ThemeBw2() 

ggsave(paste0(data.directory,"plots/power-plot-bin.png"), power.plot.bin, width=11, height=8.5) 