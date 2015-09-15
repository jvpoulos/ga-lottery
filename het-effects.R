## Define functions for for Heterogeneous treatment effects plots

# Faceted dot plot (pretreatment wealth as features)
DotPlotF <- function(plot.df,title){
  p <- ggplot(plot.df, aes(x=x,y=y,ymin=y.lo, ymax=y.hi,colour=Measure,group=Measure)) + 
    coord_flip() +
    geom_pointrange(size=1, alpha=0.8) +
    geom_hline(aes(x=0), lty=2) +
    facet_grid(Measure ~.) +
    theme(legend.position="none") +
    ylab("") +
    xlab("") + #switch because of the coord_flip() above 
    ggtitle(title)
  return(p)
}

# Dot plot (balance covariates as features)
DotPlot <- function(plot.df,title){
  # Create function for plot theme
  ThemeBw1 <- function(base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
        axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
        axis.ticks =        element_blank(),
        axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
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
  
  # Plot data
  options(scipen=10000)
  plot.df$x <- factor(plot.df$x, levels=rev(plot.df$x)) # reverse order
  p <- ggplot(plot.df, aes(x=x, y=y,ymin=y.lo, ymax=y.hi,colour=group)) + 
    geom_pointrange(size=1, alpha=0.8) + 
    coord_flip() +
    geom_line() +
    geom_hline(aes(x=0), lty=2) +
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

## Create matrix of common features
two.draws <- NA  # dummy for two draws
two.draws[resp.dat$n.draw==1] <- 0 
two.draws[resp.dat$n.draw==2] <- 1

surname.length <- DummiesQCut(lot05$surname.length) # quintiles for surname length

surname.freq <- DummiesQCut(lot05$surname.freq) # quintiles for surname frequency

surname.splits <- c("Surname length Q1", "Surname length Q2", "Surname length Q3", "Surname length Q4","Surname length Q5",
                    "Surname frequency Q1", "Surname frequency Q2", "Surname frequency Q3", "Surname frequency Q4","Surname frequency Q5")

common.features <- cbind(resp.dat[c("treat")],
                         two.draws,
                         lot05[covars.balance[!covars.balance %in% c("surname.length","surname.freq")]],
                         surname.length,
                         surname.freq)

## Heterogeneous effects on slavery legislation (pretreatment wealth)

# Source tax info
source(paste0(data.directory,"tax-records.R")) 

wealth.features <- c("slaves.pre","acres.pre","ptax.pre") # define wealth features

# Impute missing features using proximity from randomForest
set.seed(42) 
assembly.impute.slave <- rfImpute(x=cbind(common.features[!is.na(resp.dat$slave.index),], 
                                          resp.dat[!is.na(resp.dat$slave.index),][wealth.features]),
                                  y=resp.dat[!is.na(resp.dat$slave.index),]$slave.index)[-1] # remove response 

slaves.pre.slave <- DummiesQCut(assembly.impute.slave$slaves.pre)
acres.pre.slave <- DummiesQCut(assembly.impute.slave$acres.pre)
ptax.pre.slave <- DummiesQCut(assembly.impute.slave$ptax.pre)

# Create features and outcomes vectors
x07.assembly.slave <- data.frame(common.features[!is.na(resp.dat$slave.index),],
                                 "slaves.pre.slave" = slaves.pre.slave,
                                 "acres.pre.slave" = acres.pre.slave,
                                 "ptax.pre.slave"= ptax.pre.slave)

y07.assembly.slave <- as.matrix(resp.dat[!is.na(resp.dat$slave.index),]$slave.index)

# Run regression model
set.seed(42)
fitSL.assembly.slave <- SuperLearner(Y=y07.assembly.slave[,1],
                                     X=x07.assembly.slave,
                                     SL.library=SL.library.reg,
                                     family=gaussian()) # gaussian for continuous response

print(fitSL.assembly.slave) # summarize

# Use response model to estimate potential outcomes 
assembly.slave.tr.counterfactual <- cbind("treat" = rep(1, nrow(x07.assembly.slave)),
                                          x07.assembly.slave[, !names(x07.assembly.slave) %in% c("treat")])
assembly.slave.ctrl.counterfactual <- cbind("treat" = rep(0, nrow(x07.assembly.slave)),
                                            x07.assembly.slave[, !names(x07.assembly.slave) %in% c("treat")])

Y.hat.1.assembly.slave <- suppressWarnings(predict(fitSL.assembly.slave, assembly.slave.tr.counterfactual)$pred)
Y.hat.0.assembly.slave <- suppressWarnings(predict(fitSL.assembly.slave, assembly.slave.ctrl.counterfactual)$pred)

# Calculate differences in potential outcomes for treated and combine with pretreatment vars
assembly.covs.slave <- colnames(x07.assembly.slave[, !names(x07.assembly.slave) %in% c("treat","two.draws")]) # exclude treatment and draws
assembly.pred.slave <- data.frame("tau.i"=Y.hat.1.assembly.slave-Y.hat.0.assembly.slave,
                                  x07.assembly.slave[assembly.covs.slave])

# Estimate ITT effect for each covariate group
tau.assembly.slave <- lapply(assembly.covs.slave, 
                             function(x) BootDiffHet(tau.i=assembly.pred.slave$tau.i, g=assembly.pred.slave[x]))

# Create data for heterogenous wealth plot
assembly.slave <- data.frame(x=rep(c("Q1","Q2","Q3","Q4","Q5"),3),
                             y = tail(sapply(tau.assembly.slave, "[[", 1), 15),   # plot just wealth quintiles
                             y.lo= tail(sapply(tau.assembly.slave, "[[", 2), 15),  
                             y.hi= tail(sapply(tau.assembly.slave, "[[", 3), 15),  
                             Measure= c(rep("Slaves held",5), 
                                        rep("Land (acres)",5),
                                        rep("Person tax ($)",5))) 
# Plot forest plot
het.plot.assembly.slave <- DotPlotF(assembly.slave, 
                                    title=paste("Slavery legislation, N =", 
                                                format(length(fitSL.assembly.slave$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + ylim(-.15, .15)

## Heterogeneous effects on banking policy (pretreatment wealth)

# Impute missing features using proximity from randomForest
set.seed(42)
assembly.impute.bank <- rfImpute(x=cbind(common.features[!is.na(resp.dat$bank.index),], 
                                         resp.dat[!is.na(resp.dat$bank.index),][wealth.features]),
                                 y=resp.dat[!is.na(resp.dat$bank.index),]$bank.index)[-1] # remove response 

slaves.pre.bank <- DummiesQCut(assembly.impute.bank$slaves.pre)
acres.pre.bank <- DummiesQCut(assembly.impute.bank$acres.pre)
ptax.pre.bank <- DummiesQCut(assembly.impute.bank$ptax.pre)

# Create features and outcomes vectors
x07.assembly.bank <- data.frame(common.features[!is.na(resp.dat$bank.index),],
                                slaves.pre.bank,
                                acres.pre.bank,
                                ptax.pre.bank)

y07.assembly.bank <- as.matrix(resp.dat[!is.na(resp.dat$bank.index),]$bank.index)

# Run regression model
set.seed(42)
fitSL.assembly.bank <- SuperLearner(Y=y07.assembly.bank[,1],
                                    X=x07.assembly.bank,
                                    SL.library=SL.library.reg,
                                    family=gaussian()) # gaussian for continuous resp

print(fitSL.assembly.bank) # summarize

# Use response model to estimate potential outcomes 
assembly.bank.tr.counterfactual <- cbind("treat" = rep(1, nrow(x07.assembly.bank)),
                                         x07.assembly.bank[, !names(x07.assembly.bank) %in% c("treat")])
assembly.bank.ctrl.counterfactual <- cbind("treat" = rep(0, nrow(x07.assembly.bank)),
                                           x07.assembly.bank[, !names(x07.assembly.bank) %in% c("treat")])

Y.hat.1.assembly.bank <- suppressWarnings(predict(fitSL.assembly.bank, assembly.bank.tr.counterfactual)$pred)
Y.hat.0.assembly.bank <- suppressWarnings(predict(fitSL.assembly.bank, assembly.bank.ctrl.counterfactual)$pred) 

# Calculate differences in potential outcomes for treated and combine with pretreatment vars
assembly.covs.bank <- colnames(x07.assembly.bank[, !names(x07.assembly.bank) %in% c("treat","two.draws")]) # exclude treatment and draws
assembly.pred.bank <- data.frame("tau.i"=Y.hat.1.assembly.bank-Y.hat.0.assembly.bank,
                                 x07.assembly.bank[assembly.covs.bank])

# Estimate ITT effect for each covariate group
tau.assembly.bank <- lapply(assembly.covs.bank, 
                            function(x) BootDiffHet(tau.i=assembly.pred.bank$tau.i, g=assembly.pred.bank[x]))

# Create data for plot
assembly.bank <- data.frame(x=c(rep(c("Q1","Q2","Q3","Q4","Q5"),3)), 
                            y = tail(sapply(tau.assembly.bank, "[[", 1), 15), 
                            y.lo= tail(sapply(tau.assembly.bank, "[[", 2), 15),
                            y.hi= tail(sapply(tau.assembly.bank, "[[", 3), 15),
                            Measure= c(rep("Slaves held",5),
                                       rep("Land (acres)",5),
                                       rep("Person tax ($)",5)))
# Plot forest plot
het.plot.assembly.bank <- DotPlotF(assembly.bank, 
                                   title=paste("State banking policy, N =", 
                                               format(length(fitSL.assembly.bank$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + ylim(-.15, .15)

## Combine legislator-participant plots
pdf(paste0(data.directory,"het-wealth-plots.pdf"), width=11, height=8.5)
grid.arrange(het.plot.assembly.slave,het.plot.assembly.bank,
             ncol=2, nrow=1, left="Pretreatment measure of wealth (quintiles)", bottom="Heterogeneous treatment effect")
dev.off() 

## Heterogeneous treatment effects on officeholding 

# Create features and outcomes vectors
x05.oh <- data.frame(common.features[rownames(sub.oh),])
y05.oh <- as.matrix(sub.oh$oh) 

# Run model
set.seed(42) 
fitSL.oh <- SuperLearner(Y=y05.oh[,1],X=x05.oh,
                         SL.library=SL.library.class,
                         family="binomial") # glmnet response is 2-level factor 

# Print summary table
print(fitSL.oh) 

# Use response model to estimate potential outcomes 
oh.tr.counterfactual <- cbind("treat" = rep(1, nrow(x05.oh)),
                              x05.oh[, !names(x05.oh) %in% c("treat")])
oh.ctrl.counterfactual <- cbind("treat" = rep(0, nrow(x05.oh)),
                                x05.oh[, !names(x05.oh) %in% c("treat")])

Y.hat.1.oh <- suppressWarnings(predict(fitSL.oh, oh.tr.counterfactual)$pred)   
Y.hat.0.oh <- suppressWarnings(predict(fitSL.oh, oh.ctrl.counterfactual)$pred) 

# Calculate differences in potential outcomes for treated and combine with pretreatment vars
oh.covs <- colnames(x05.oh[, !names(x05.oh) %in% c("treat","two.draws")]) # exclude treatment & two draws  
oh.pred <- data.frame("tau.i"=Y.hat.1.oh-Y.hat.0.oh,
                      x05.oh[oh.covs])  

# Estimate mean difference and CIs for each covariate group
tau.oh.het <- lapply(oh.covs, function(x) BootDiffHet(tau.i=oh.pred$tau.i, g=oh.pred[x]))   

# Create data for plot
oh.plot <- data.frame(x=c(covars.names[-c(3,4)],  
                          surname.splits), 
                      y = sapply(tau.oh.het, "[[", 1), 
                      y.lo= sapply(tau.oh.het, "[[", 2),
                      y.hi= sapply(tau.oh.het, "[[", 3))  

# Plot dot plot
het.plot.oh <- DotPlot(oh.plot, 
                       title=paste("Officeholding, N =", 
                                   format(length(fitSL.oh$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + ylim(-.2, .2)

## Heterogeneous treatment effects on slavery legislation

# Create data for plot
slave.plot <- data.frame(x=c(covars.names[-c(3,4)], 
                             surname.splits),
                         y = head(sapply(tau.assembly.slave, "[[", 1), 47), # don't plot wealth measures
                         y.lo= head(sapply(tau.assembly.slave, "[[", 2), 47),
                         y.hi= head(sapply(tau.assembly.slave, "[[", 3), 47))

# Plot forest plot
het.plot.slave <- DotPlot(slave.plot, 
                          title=paste("Slavery legislation, N =", 
                                      format(length(fitSL.assembly.slave$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + ylim(-.15, .15)

## Heterogeneous treatment effects on state banking policy

# Create data for plot
bank.plot <- data.frame(x=c(covars.names[-c(3,4)], 
                            surname.splits), 
                        y = head(sapply(tau.assembly.bank, "[[", 1), 47), # don't plot wealth measures
                        y.lo= head(sapply(tau.assembly.bank, "[[", 2), 47),
                        y.hi= head(sapply(tau.assembly.bank, "[[", 3), 47))  

# Plot forest plot
het.plot.bank <- DotPlot(bank.plot, 
                         title=paste("State banking policy, N =", 
                                     format(length(fitSL.assembly.bank$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + ylim(-.15, .15)  

## Heterogeneous treatment effects on number of terms held after lottery

# Create features and outcomes vectors
x05.term <- data.frame(common.features[rownames(sub.prior),])
y05.term <- as.matrix(sub.prior$n.post.terms)

# Run model
set.seed(42)
fitSL.term <- SuperLearner(Y=y05.term[,1],X=x05.term,
                           SL.library=SL.library.reg,
                           family=gaussian()) # gaussian for continuous resp

# Print summary table
print(fitSL.term)

# Use response model to estimate potential outcomes 
term.tr.counterfactual <- cbind("treat" = rep(1, nrow(x05.term)),
                                x05.term[, !names(x05.term) %in% c("treat")])
term.ctrl.counterfactual <- cbind("treat" = rep(0, nrow(x05.term)),
                                  x05.term[, !names(x05.term) %in% c("treat")])

Y.hat.1.term <- suppressWarnings(predict(fitSL.term, term.tr.counterfactual)$pred)
Y.hat.0.term <- suppressWarnings(predict(fitSL.term, term.ctrl.counterfactual)$pred)

# Calculate differences in potential outcomes for treated and combine with pretreatment vars
term.covs <- colnames(x05.term[, !names(x05.term) %in% c("treat","two.draws")])  
term.pred <- data.frame("tau.i"=Y.hat.1.term-Y.hat.0.term,
                        x05.term[term.covs]) 

# Estimate mean difference and CIs for each covariate group
tau.term.het <- lapply(term.covs, function(x) BootDiffHet(tau.i=term.pred$tau.i, g=term.pred[x]))  

# Create data for plot
term.plot <- data.frame(x = c(covars.names[-c(3,4)], 
                              surname.splits), 
                        y = sapply(tau.term.het, "[[", 1), 
                        y.lo= sapply(tau.term.het, "[[", 2),
                        y.hi= sapply(tau.term.het, "[[", 3)) 

# Plot forest plot
het.plot.term <- DotPlot(term.plot, 
                         title=paste("# terms after lottery, N =", 
                                     format(length(fitSL.term$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + ylim(-.2, .2) 

## Heterogeneous treatment effects on number of slaves held (1820)

# Create features and outcomes vectors
x05.slaves <- data.frame(common.features[!is.na(resp.dat$no.slaves.1820),])
y05.slaves <- as.matrix(resp.dat[!is.na(resp.dat$no.slaves.1820),]$no.slaves.1820)

# Run model
set.seed(42)
fitSL.slaves <- SuperLearner(Y=y05.slaves[,1],X=x05.slaves,
                             SL.library=SL.library.reg,
                             family=gaussian()) # gaussian for continuous resp

# Print summary table
print(fitSL.slaves)

# Use response model to estimate potential outcomes 
slaves.tr.counterfactual <- cbind("treat" = rep(1, nrow(x05.slaves)),
                                  x05.slaves[, !names(x05.slaves) %in% c("treat")])
slaves.ctrl.counterfactual <- cbind("treat" = rep(0, nrow(x05.slaves)),
                                    x05.slaves[, !names(x05.slaves) %in% c("treat")])

Y.hat.1.slaves <- suppressWarnings(predict(fitSL.slaves, slaves.tr.counterfactual)$pred)
Y.hat.0.slaves <- suppressWarnings(predict(fitSL.slaves, slaves.ctrl.counterfactual)$pred)

# Calculate differences in potential outcomes for treated and combine with pretreatment vars
slaves.covs <- colnames(x05.slaves[, !names(x05.slaves) %in% c("treat","two.draws")]) 
slaves.pred <- data.frame("tau.i"=Y.hat.1.slaves-Y.hat.0.slaves,
                          x05.slaves[slaves.covs])

# Estimate mean difference and CIs for each covariate group
tau.slaves.het <- lapply(slaves.covs, function(x) BootDiffHet(tau.i=slaves.pred$tau.i, g=slaves.pred[x])) 

# Create data for plot
slaves.plot <- data.frame(x=c(covars.names[-c(3,4)], 
                              surname.splits), 
                          y = sapply(tau.slaves.het, "[[", 1), 
                          y.lo= sapply(tau.slaves.het, "[[", 2),
                          y.hi= sapply(tau.slaves.het, "[[", 3))

# Plot forest plot
het.plot.slaves <- DotPlot(slaves.plot, 
                           title=paste("# slaves held (1820), N =", 
                                       format(length(fitSL.slaves$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + ylim(-.2, .2)

## Combine legislator-participant plots
pdf(paste0(data.directory,"het-wealth-plots.pdf"), width=11, height=8.5)
grid.arrange(het.plot.slave, 
             het.plot.bank, 
             ncol=2, nrow=1, left="Pretreatment covariate", bottom="Heterogeneous treatment effect")
dev.off()

## Combine plots for other outcomes
pdf(paste0(data.directory,"het-plots.pdf"), width=11, height=16)
grid.arrange(het.plot.term, 
             het.plot.slaves,
             het.plot.oh, 
             ncol=2, nrow=2, left="Pretreatment covariate", bottom="Heterogeneous treatment effect")
dev.off() 
