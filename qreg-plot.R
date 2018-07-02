## Create quantile regression plot

taus <- seq(0.4,0.995,0.005)

# 1805 winners and losers

qreg.fits <- lapply(taus, function(t){
  rq(formula = slave.wealth.1820 ~ treat + n.draw, 
     tau = t, 
     data = sub.1820.05)
})

qreg.plot.df <- data.frame("effect" = sapply(qreg.fits, "[[", 1)[2,],
                           "se" = sapply(1:length(taus), 
                                         function(x) summary(qreg.fits[[x]], se="boot")[["coefficients"]][,2][[2]]),
                           "quantile"= taus)

winners.losers.05.title <- paste("1805 winners & losers, N =", 
                                 format(nrow(sub.1820.05),big.mark=",",scientific=FALSE,trim=TRUE),"\n")

qreg.plot <- ggplot(qreg.plot.df[qreg.plot.df$quantile <0.985,], aes(y=effect, x=quantile)) + 
  geom_pointrange(aes(ymin = effect-(1.96*se), ymax = effect+(1.96*se)),shape=19, alpha=1/4) + 
  ylab("Treatment effect") + 
  xlab("Quantile of slave wealth (1820$)") + 
  ggtitle(winners.losers.05.title) +
  stat_smooth(method = "loess",se=FALSE) + 
  scale_y_continuous(labels = comma) + 
  scale_y_continuous(labels = dollar) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(data.directory,"plots/qreg-plot.png"), qreg.plot, width=11, height=8.5)

## Report results

#46%
qreg.plot.df[92,]$effect # effect
qreg.plot.df[92,]$effect - (1.96*qreg.plot.df[92,]$se) # lower 
qreg.plot.df[92,]$effect + (1.96*qreg.plot.df[92,]$se) # upper

#50%
qreg.plot.df[100,]$effect # effect
qreg.plot.df[100,]$effect - (1.96*qreg.plot.df[100,]$se) # lower 
qreg.plot.df[100,]$effect + (1.96*qreg.plot.df[100,]$se) # upper

# 1805 winners
qreg.fits.05.winners <- lapply(taus, function(t){
  rq(formula = slave.wealth.1820 ~ treat + n.draw, 
     tau = t, 
     data = sub.1820.05.winners)
})

qreg.05.winners.plot.df <- data.frame("effect" = sapply(qreg.fits.05.winners, "[[", 1)[2,],
                           "se" = sapply(1:length(taus), 
                                         function(x) summary(qreg.fits.05.winners[[x]], se="boot")[["coefficients"]][,2][[2]]),
                           "quantile"= taus)

winners.05.title <- paste("1805 lottery winners, N =", 
                          format(nrow(sub.1820.05.winners),big.mark=",",scientific=FALSE,trim=TRUE),"\n")


qreg.05.winners.plot <- ggplot(qreg.05.winners.plot.df[qreg.05.winners.plot.df$quantile <0.985,], aes(y=effect, x=quantile)) + 
  geom_pointrange(aes(ymin = effect-(1.96*se), ymax = effect+(1.96*se)),shape=19, alpha=1/4) + 
  ylab("Treatment effect") + 
  xlab("Quantile of slave wealth (1820$)") + 
  ggtitle(winners.05.title) +
  stat_smooth(method = "loess",se=FALSE) + 
  scale_y_continuous(labels = comma) + 
  scale_y_continuous(labels = dollar) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(data.directory,"plots/qreg-plot-05-winners.png"), qreg.05.winners.plot, width=11, height=8.5)

# 1807 winners
qreg.fits.07.winners <- lapply(taus, function(t){
  rq(formula = slave.wealth.1820 ~ treat + n.draw, 
     tau = t, 
     data = sub.1820.07.winners)
})

qreg.07.winners.plot.df <- data.frame("effect" = sapply(qreg.fits.07.winners, "[[", 1)[2,],
                                      "se" = sapply(1:length(taus), 
                                                    function(x) summary(qreg.fits.07.winners[[x]], se="boot")[["coefficients"]][,2][[2]]),
                                      "quantile"= taus)

winners.07.title <- paste("1807 lottery winners, N =", 
                          format(nrow(sub.1820.07.winners),big.mark=",",scientific=FALSE,trim=TRUE))

qreg.07.winners.plot <- ggplot(qreg.07.winners.plot.df[qreg.07.winners.plot.df$quantile <0.985,], aes(y=effect, x=quantile)) + 
  geom_pointrange(aes(ymin = effect-(1.96*se), ymax = effect+(1.96*se)),shape=19, alpha=1/4) + 
  ylab("Treatment effect") + 
  xlab("Quantile of slave wealth (1820$)") + 
  ggtitle(winners.07.title) +
  stat_smooth(method = "loess",se=FALSE) + 
  scale_y_continuous(labels = comma) + 
  scale_y_continuous(labels = dollar) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(data.directory,"plots/qreg-plot-07-winners.png"), qreg.07.winners.plot, width=11, height=8.5)