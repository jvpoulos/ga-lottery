## Functions for estimation of ATEs, p-vales, CIs

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

BootDiff<- function(y,treat,w,R=10000,beta.hat,sc=1) {
  # Function to compute 95% confidence interval for the weighted difference in two means.
  #
  # Args:
  #   y: Response vector.
  #   treat: Treatment vector
  #   w: Vector of weights.
  #   R: The number of bootstrap replicates. The default is 10,000. 
  #   beta.hat: The fraction of compliers in the experimental population.
  #   sc. Smoothing constant. Default is 1. 
  #
  # Returns:
  #   Vector containing weighted difference-in-means, and 95% nonparametric CI.
  treat <- as.factor(treat) 
  # Bootstrap weighted means for response y for each treatment group 
  itt.diff <- bootstrap(y, EstAte,R=R, args.stat=list(treat,w[treat])) #ITT
  tot.diff <- bootstrap(y, EstAte,R=R, args.stat=list(treat,w[treat],beta.hat)) #TOT
  # Smooth bootstrap replicates by adding random normal variate independently to each observation
  itt.diff[[2]] <- sapply(1:R, function(x) {
    itt.diff[[2]][x] + rnorm(1,0,sc*sd(itt.diff[[2]])/sqrt(length(y)))
  })
  tot.diff[[2]] <- sapply(1:R, function(x) {
    tot.diff[[2]][x] + rnorm(1,0,sc*sd(tot.diff[[2]])/sqrt(length(y)))
  })
  # Calculate percentiles of the differences in bootstrapped means
  itt.per <- quantile(itt.diff[[2]], probs = c(0.025, 0.975))
  tot.per <- quantile(tot.diff[[2]], probs = c(0.025, 0.975))
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
    geom_pointrange(size=1.5, alpha=0.9) + 
    coord_flip() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
    theme(legend.position="none") +
    facet_grid(Outcome~.) +
    ylab(xlab) +
    xlab(ylab) #switch because of the coord_flip() above
  return(p)
}

## Functions for balance plot

# Create function for balance plot theme
ThemeBw1 <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_blank(),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}

## Functions for for heterogeneous treatment effects plots

BootDiffHet <- function(tau.i,g) {
  # Function to compute 95% confidence intervals for heterogeneous treatment effect plots. 
  #
  # Args:
  #   tau.i: Vector of observed individual treatment effects. 
  #   g: Binary grouping variable. 
  #
  # Returns:
  #   Vector containing difference-in-means, and 95% nonparametric CI.
  g <- as.factor(g[[1]])
  a <- attr(smean.cl.boot(tau.i[g==0],reps=TRUE),'reps')  # bootsrapped conditional mean differences
  b <- attr(smean.cl.boot(tau.i[g==1],reps=TRUE),'reps')  # default is R=1000
  meandif <- mean(tau.i[g==1]) - mean(tau.i[g==0]) # observed conditional mean difference
  a.b <- quantile(b-a, c(.025,.975))
  res <- c(meandif, a.b)
  names(res) <- c('Mean Difference','.025','.975')
  return(res)
}

# Faceted dot plot (pretreatment wealth as features)
DotPlotF <- function(plot.df,title){
  options(scipen=999)
  p <- ggplot(plot.df, aes(x=x,y=y,ymin=y.lo, ymax=y.hi,colour=Measure,group=Measure)) + 
    coord_flip() +
    geom_pointrange(size=1.5, alpha=0.9) +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
    facet_grid(Measure ~.) +
    theme(legend.position="none",plot.title = element_text(face="bold")) +
    ylab("") +
    xlab("") + #switch because of the coord_flip() above 
    ggtitle(title)
  return(p)
}

# Dot plot (balance covariates as features)
DotPlot <- function(plot.df,title,missing.occ=FALSE,missing.gen=FALSE){
  # Create function for plot theme
  ThemeBw1 <- function(base_size = 12, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
        axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
        axis.ticks =        element_blank(),
        axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
        plot.title = element_text(face="bold"),
        legend.position = "none"
      )
  }
  # Group variables
  Generational  <- c("Junior","Senior") 
  Surname    <- c("Surname frequency","Surname length")
  Occupations       <- c("Blacksmith","Bricklayer","Hatter","Lawyer","Merchant","Military","Physician","Reverend","Teacher")
  Counties       <- c("Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")
  
  # Reformat plot data to match balance plot
  plot.df$group <- NA
  plot.df$group[plot.df$x %in% Generational]       <- "Generational titles"
  plot.df$group[plot.df$x %in% surname.splits]       <- "Surname characteristics"
  plot.df$group[plot.df$x %in% Occupations]       <- "Occupations"
  plot.df$group[plot.df$x %in% Counties]       <- "County of registration"
  
  covars.reorder <- c("Junior","Senior", # reorder
                      "Surname frequency Q5","Surname frequency Q4","Surname frequency Q3","Surname frequency Q2","Surname frequency Q1",
                      "Surname length Q5","Surname length Q4","Surname length Q3","Surname length Q2","Surname length Q1",
                      "Blacksmith","Bricklayer","Hatter","Lawyer","Merchant","Military","Physician","Reverend","Teacher",
                      "Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")
  
  plot.df <- plot.df[match(covars.reorder, plot.df$x),]
  
  offset <- c("   ")
  
  plot.df$x <- paste(offset,plot.df$x) # make offset in x var name
  
  plot.df$order <- 1:nrow(plot.df)   
  order.dot <- data.frame(x= c("Generational titles:",
                               "  ",
                               "Surname characteristics (quintiles):",
                               "   ",
                               "Occupations:",
                               "    ",
                               "County of registration:"),
                          order=c(.5,2.1,2.5,12.1,12.5,21.1,21.5),
                          y=NA,
                          y.lo=NA,
                          y.hi=NA,
                          group=NA)
  plot.df <- rbind(plot.df,order.dot)
  plot.df <-plot.df[order(plot.df$order),]
  plot.df$x <- factor(plot.df$x,levels=unique(plot.df$x)[length(plot.df$x):1])
  
  plot.df <- plot.df[!is.nan(plot.df$y),] # remove lines with empty estimates 
  plot.df <- plot.df[(!is.na(plot.df$y.lo)) | (is.na(plot.df$group)),] # remove lines with empty CIs
  if(missing.occ==TRUE){
    plot.df <- plot.df[plot.df$order!=12.1 & plot.df$order!=12.5,]
  }
  if(missing.gen==TRUE){
    plot.df <- plot.df[plot.df$order!=2.1 & plot.df$order!=2.5,]
  }
  
  # Plot data
  options(scipen=10000)
  plot.df$x <- factor(plot.df$x, levels=rev(plot.df$x)) # reverse order
  p <- ggplot(plot.df, aes(x=x, y=y,ymin=y.lo, ymax=y.hi,colour=group)) + 
    geom_pointrange(size=1.5, alpha=0.9) + 
    coord_flip() +
    geom_line() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
    ggtitle(title) +
    ylab("") +
    xlab("") #switch because of the coord_flip() above 
  return(p+ ThemeBw1())
}

# Create dummies for pretreatment vars
DummiesQCut <- function(x) {
  # Cut continuous variable into deciles
  var <- quantcut(x, q=seq(0,1,by=0.20), na.rm=FALSE)
  return(dummify(var, keep.na=TRUE))
}