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

## Heterogeneous effects on slavery legislation /state banking policy (pretreatment wealth)

wealth.features <- c("slaves.pre","acres.pre","ptax.pre") # define wealth features

# Impute missing features using proximity from randomForest
set.seed(42) 
assembly.impute.slave <- rfImpute(x=cbind(common.features[rownames(sub.prior),][!is.na(sub.prior$slave.index),], 
                                          sub.prior[!is.na(sub.prior$slave.index),][wealth.features]),
                                  y=sub.prior[!is.na(sub.prior$slave.index),]$slave.index)[-1] # remove response 

slaves.pre.slave <- DummiesQCut(assembly.impute.slave$slaves.pre)
acres.pre.slave <- DummiesQCut(assembly.impute.slave$acres.pre)
ptax.pre.slave <- DummiesQCut(assembly.impute.slave$ptax.pre)

set.seed(42) 
assembly.impute.bank <- rfImpute(x=cbind(common.features[rownames(sub.prior),][!is.na(sub.prior$bank.index),], 
                                         sub.prior[!is.na(sub.prior$bank.index),][wealth.features]),
                                 y=sub.prior[!is.na(sub.prior$bank.index),]$bank.index)[-1] # remove response 

slaves.pre.bank <- DummiesQCut(assembly.impute.bank$slaves.pre)
acres.pre.bank <- DummiesQCut(assembly.impute.bank$acres.pre)
ptax.pre.bank <- DummiesQCut(assembly.impute.bank$ptax.pre)

# Create features and outcomes vectors
x07.assembly.slave <- data.frame(common.features[rownames(sub.prior),][!is.na(sub.prior$slave.index),],
                                 "slaves.pre.slave" = slaves.pre.slave,
                                 "acres.pre.slave" = acres.pre.slave,
                                 "ptax.pre.slave"= ptax.pre.slave)

y07.assembly.slave <- as.matrix(sub.prior[!is.na(sub.prior$slave.index),]$slave.index)

x07.assembly.bank <- data.frame(common.features[rownames(sub.prior),][!is.na(sub.prior$bank.index),],
                                "slaves.pre.bank" = slaves.pre.bank,
                                "acres.pre.bank" = acres.pre.bank,
                                "ptax.pre.bank"= ptax.pre.bank)

y07.assembly.bank <- as.matrix(sub.prior[!is.na(sub.prior$bank.index),]$bank.index)

# Run regression model
set.seed(42)
fitSL.assembly.slave <- SuperLearner(Y=y07.assembly.slave[,1],
                                     X=x07.assembly.slave,
                                     SL.library=SL.library.reg[c(1:3,5,11:14)],
                                     family=gaussian()) # gaussian for continuous response

print(fitSL.assembly.slave) # summarize

set.seed(42)
fitSL.assembly.bank <- SuperLearner(Y=y07.assembly.bank[,1],
                                    X=x07.assembly.bank,
                                    SL.library=SL.library.reg[c(1:3,5,11:14)],
                                    family=gaussian()) # gaussian for continuous response

print(fitSL.assembly.bank) # summarize

# Use response model to estimate potential outcomes 
assembly.slave.tr.counterfactual <- cbind("treat" = rep(1, nrow(x07.assembly.slave)),
                                          x07.assembly.slave[, !names(x07.assembly.slave) %in% c("treat")])
assembly.slave.ctrl.counterfactual <- cbind("treat" = rep(0, nrow(x07.assembly.slave)),
                                            x07.assembly.slave[, !names(x07.assembly.slave) %in% c("treat")])

Y.hat.1.assembly.slave <- suppressWarnings(predict(fitSL.assembly.slave, assembly.slave.tr.counterfactual)$pred)
Y.hat.0.assembly.slave <- suppressWarnings(predict(fitSL.assembly.slave, assembly.slave.ctrl.counterfactual)$pred)

assembly.bank.tr.counterfactual <- cbind("treat" = rep(1, nrow(x07.assembly.bank)),
                                         x07.assembly.bank[, !names(x07.assembly.bank) %in% c("treat")])
assembly.bank.ctrl.counterfactual <- cbind("treat" = rep(0, nrow(x07.assembly.bank)),
                                           x07.assembly.bank[, !names(x07.assembly.bank) %in% c("treat")])

Y.hat.1.assembly.bank <- suppressWarnings(predict(fitSL.assembly.bank, assembly.bank.tr.counterfactual)$pred)
Y.hat.0.assembly.bank <- suppressWarnings(predict(fitSL.assembly.bank, assembly.bank.ctrl.counterfactual)$pred)

# Calculate differences in potential outcomes for treated and combine with pretreatment vars
assembly.covs.slave <- colnames(x07.assembly.slave[, !names(x07.assembly.slave) %in% c("treat","two.draws")]) # exclude treatment and draws
assembly.pred.slave <- data.frame("tau.i"=Y.hat.1.assembly.slave-Y.hat.0.assembly.slave,
                                  x07.assembly.slave[assembly.covs.slave])

assembly.covs.bank <- colnames(x07.assembly.bank[, !names(x07.assembly.bank) %in% c("treat","two.draws")]) # exclude treatment and draws
assembly.pred.bank <- data.frame("tau.i"=Y.hat.1.assembly.bank-Y.hat.0.assembly.bank,
                                 x07.assembly.bank[assembly.covs.bank])

# Estimate ITT effect for each covariate group
tau.assembly.slave <- lapply(assembly.covs.slave, 
                             function(x) BootDiffHet(tau.i=assembly.pred.slave$tau.i, g=assembly.pred.slave[x]))

tau.assembly.bank <- lapply(assembly.covs.bank, 
                            function(x) BootDiffHet(tau.i=assembly.pred.bank$tau.i, g=assembly.pred.bank[x]))

# Create data for heterogenous wealth plot
assembly.slave <- data.frame(x=rep(c("Q1","Q2","Q3","Q4","Q5"),3),
                             y = tail(sapply(tau.assembly.slave, "[[", 1), 15),   # plot just wealth quintiles
                             y.lo= tail(sapply(tau.assembly.slave, "[[", 2), 15),  
                             y.hi= tail(sapply(tau.assembly.slave, "[[", 3), 15),  
                             Measure= c(rep("Slaves held",5), 
                                        rep("Land (acres)",5),
                                        rep("Person tax ($)",5))) 

assembly.bank <- data.frame(x=rep(c("Q1","Q2","Q3","Q4","Q5"),3),
                            y = tail(sapply(tau.assembly.bank, "[[", 1), 15),   # plot just wealth quintiles
                            y.lo= tail(sapply(tau.assembly.bank, "[[", 2), 15),  
                            y.hi= tail(sapply(tau.assembly.bank, "[[", 3), 15),  
                            Measure= c(rep("Slaves held",5), 
                                       rep("Land (acres)",5),
                                       rep("Person tax ($)",5))) 

# Plot forest plot
het.plot.assembly.slave <- DotPlotF(assembly.slave, 
                                    title=paste("Slavery legislation, N =", 
                                                format(nrow(assembly.pred.slave),big.mark=",",scientific=FALSE,trim=TRUE))) + 
  scale_y_continuous(labels = percent_format())

het.plot.assembly.bank <- DotPlotF(assembly.bank, 
                                   title=paste("Banking legislation, N =", 
                                               format(nrow(assembly.pred.bank),big.mark=",",scientific=FALSE,trim=TRUE))) + 
  scale_y_continuous(labels = percent_format())

pdf(paste0(data.directory,"het-wealth-plots.pdf"), width=8.5, height=11)
grid.arrange(het.plot.assembly.slave,
             het.plot.assembly.bank, 
             ncol=2, nrow=1, left="Pretreatment measure of wealth (quintiles)", bottom="Heterogeneous treatment effect")
dev.off() 

## Heterogeneous treatment effects on candidacy

# Create features and outcomes vectors
x05.candidate <- data.frame(common.features[rownames(sub.candidate),])
y05.candidate <- as.matrix(sub.candidate$candidate) 

# Run model
set.seed(42) 
fitSL.candidate <- SuperLearner(Y=y05.candidate[,1],X=x05.candidate,
                                SL.library=SL.library.class[c(1,8:11)],
                                family="binomial") # glmnet response is 2-level factor 

# Print summary table
print(fitSL.candidate) 

# Use response model to estimate potential outcomes 
candidate.tr.counterfactual <- cbind("treat" = rep(1, nrow(x05.candidate)),
                                     x05.candidate[, !names(x05.candidate) %in% c("treat")])
candidate.ctrl.counterfactual <- cbind("treat" = rep(0, nrow(x05.candidate)),
                                       x05.candidate[, !names(x05.candidate) %in% c("treat")])

Y.hat.1.candidate <- suppressWarnings(predict(fitSL.candidate, candidate.tr.counterfactual)$pred)   
Y.hat.0.candidate <- suppressWarnings(predict(fitSL.candidate, candidate.ctrl.counterfactual)$pred) 

# Calculate differences in potential outcomes for treated and combine with pretreatment vars
candidate.covs <- colnames(x05.candidate[, !names(x05.candidate) %in% c("treat","two.draws")]) # exclude treatment & two draws  
candidate.pred <- data.frame("tau.i"=Y.hat.1.candidate-Y.hat.0.candidate,
                             x05.candidate[candidate.covs])  

# Estimate mean difference and CIs for each covariate group
tau.candidate.het <- lapply(candidate.covs, function(x) BootDiffHet(tau.i=candidate.pred$tau.i, g=candidate.pred[x]))   

# Create data for plot
candidate.plot <- data.frame(x=c(covars.names[-c(3,4)], 
                                 surname.splits), 
                             y = sapply(tau.candidate.het, "[[", 1), 
                             y.lo= sapply(tau.candidate.het, "[[", 2),
                             y.hi= sapply(tau.candidate.het, "[[", 3))

# Plot dot plot
het.plot.candidate <- DotPlot(candidate.plot, 
                              title=paste("Candidacy, N =", 
                                          format(length(fitSL.candidate$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + 
  scale_y_continuous(labels = percent_format())

## Heterogeneous treatment effects on officeholding 

# Create features and outcomes vectors
x05.oh <- data.frame(common.features[rownames(sub.oh),])
y05.oh <- as.matrix(sub.oh$oh) 

# Run model
set.seed(42) 
fitSL.oh <- SuperLearner(Y=y05.oh[,1],X=x05.oh,
                         SL.library=SL.library.class[c(1,8:11)],
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
                                   format(length(fitSL.oh$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + 
  scale_y_continuous(labels = percent_format())

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
                                      format(length(fitSL.assembly.slave$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE)),
                          missing.occ=TRUE,
                          missing.gen=TRUE) + scale_y_continuous(labels = percent_format())

## Heterogeneous treatment effects on slavery legislation

# Create data for plot
bank.plot <- data.frame(x=c(covars.names[-c(3,4)], 
                            surname.splits),
                        y = head(sapply(tau.assembly.bank, "[[", 1), 47), # don't plot wealth measures
                        y.lo= head(sapply(tau.assembly.bank, "[[", 2), 47),
                        y.hi= head(sapply(tau.assembly.bank, "[[", 3), 47))

# Plot forest plot
het.plot.bank <- DotPlot(bank.plot, 
                         title=paste("Banking legislation, N =", 
                                     format(length(fitSL.assembly.bank$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE)),
                         missing.occ=TRUE,
                         missing.gen=TRUE) + scale_y_continuous(labels = percent_format())

## Heterogeneous treatment effects on number of terms held after lottery

# Create features and outcomes vectors
x05.term <- data.frame(common.features[rownames(sub.prior),])
y05.term <- as.matrix(sub.prior$n.post.terms)

# Run model
set.seed(42)
fitSL.term <- SuperLearner(Y=range01(y05.term[,1]), #transform to 0-1 continous variable
                           X=x05.term, 
                           SL.library=SL.library.reg[c(1:3,5,11:14)],
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
                         title=paste("Terms, N =", 
                                     format(length(fitSL.term$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + 
  scale_y_continuous(labels = percent_format())

## Heterogeneous treatment effects on slave wealth

# Create features and outcomes vectors
x05.slaves <- data.frame(common.features[rownames(sub.1820),])
y05.slaves <- as.matrix(sub.1820$slave.wealth.1820)

# Run model
set.seed(42)
fitSL.slaves <- SuperLearner(Y=range01(y05.slaves[,1]), #transform to 0-1 continous variable
                             X=x05.slaves, 
                             SL.library=SL.library.reg[c(1:3,5,11:14)],
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
                           title=paste("Slave wealth, N =", 
                                       format(length(fitSL.slaves$SL.predict),big.mark=",",scientific=FALSE,trim=TRUE))) + 
  scale_y_continuous(labels = percent_format())

## Save OH and candidate plots

ggsave(paste0(data.directory,"het-plot-oh.pdf"), het.plot.oh, width=8.5, height=11) 
ggsave(paste0(data.directory,"het-plot-candidate.pdf"), het.plot.candidate, width=8.5, height=11) 

## Combine plots for legislative outcomes
pdf(paste0(data.directory,"het-assembly-plots.pdf"), width=8.5, height=11)
grid.arrange( het.plot.bank,
              het.plot.slave,
              ncol=1, nrow=2, left="Pretreatment measure of wealth (quintiles)", bottom="Heterogeneous treatment effect")
dev.off() 

## Combine plots for other outcomes
pdf(paste0(data.directory,"het-plots.pdf"), width=8.5, height=13)
grid.arrange(het.plot.slaves,
             het.plot.term, 
             ncol=1, nrow=2,
             left="Pretreatment covariate", bottom="Heterogeneous treatment effect")
dev.off() 

## Plot OH het treatment effects vs. 1820/50 Ginis

county.oh.df <- merge(oh.plot[12:37,],
                      counties.1820, 
                      by.x = c("x"), 
                      by.y = c("name"),
                      all.x=TRUE)

county.oh.df$above.state.gini <- ifelse(county.oh.df$gini > 0.7886069,1,0)
county.oh.df$above.state.slave <- ifelse(county.oh.df$slave.poppc > 0.41222123,1,0)

county.oh.plot.gini <- ggplot(county.oh.df, aes(gini, y, colour = above.state.gini)) + 
  geom_pointrange(aes(ymax =county.oh.df$y.hi, ymin=county.oh.df$y.lo)) +
  xlab("Slave wealth Gini") +
  ylab("") +
  stat_smooth(method = "loess",se=TRUE) + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent_format())

county.oh.plot.slave <- ggplot(county.oh.df, aes(slave.poppc, y, colour = above.state.slave)) + 
  geom_pointrange(aes(ymax =county.oh.df$y.hi, ymin=county.oh.df$y.lo)) +
  xlab("Slave pop. (%)") +
  ylab("") +
  stat_smooth(method = "loess",se=TRUE) + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent_format())

# Combine plots
pdf(paste0(data.directory,"county-oh-plot.pdf"), width=8.5, height=13)
grid.arrange(county.oh.plot.gini,
             county.oh.plot.slave, 
             ncol=1, nrow=2,
             left="Heterogeneous treatment effect")
dev.off() 

## Plot slave wealth het treatment effects vs. 1820/50 Ginis

county.slaves.df <- merge(slaves.plot[12:37,],
                      counties.1820, 
                      by.x = c("x"), 
                      by.y = c("name"),
                      all.x=TRUE)

county.slaves.df$above.state.gini <- ifelse(county.slaves.df$gini > 0.7886069,1,0)
county.slaves.df$above.state.slave <- ifelse(county.slaves.df$slave.poppc > 0.41222123,1,0)

county.slaves.plot.gini <- ggplot(county.slaves.df, aes(gini, y, colour = above.state.gini)) + 
  geom_pointrange(aes(ymax =county.slaves.df$y.hi, ymin=county.slaves.df$y.lo)) +
  xlab("Slave wealth Gini") +
  ylab("") +
  stat_smooth(method = "loess",se=TRUE) + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent_format())

county.slaves.plot.slave <- ggplot(county.slaves.df, aes(slave.poppc, y, colour = above.state.slave)) + 
  geom_pointrange(aes(ymax =county.slaves.df$y.hi, ymin=county.slaves.df$y.lo)) +
  xlab("Slave pop. (%)") +
  ylab("") +
  stat_smooth(method = "loess",se=TRUE) + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent_format())

# Combine plots
pdf(paste0(data.directory,"county-slaves-plot.pdf"), width=8.5, height=13)
grid.arrange(county.slaves.plot.gini,
             county.slaves.plot.slave, 
             ncol=1, nrow=2,
             left="Heterogeneous treatment effect")
dev.off() 