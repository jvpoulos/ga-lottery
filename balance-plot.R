# Create balance plot data
covars.names <- c("1820 Census Match","Candidate","Officeholder","Junior","Senior","Surname Frequency","Surname Length","Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")

covars <- data.frame("covars"=covars.names,
                     "1805.ATE"= unlist(lapply(balance, '[[', 1)),
                     "1805.p"= unlist(lapply(balance, '[[', 2)),
                     "1805.winners.ATE"= unlist(lapply(balance.05.winners, '[[', 1)),
                     "1805.winners.p"= unlist(lapply(balance.05.winners, '[[', 2)),
                     "1807.winners.ATE"= unlist(lapply(balance.07.winners, '[[', 1)),
                     "1807.winners.p"= unlist(lapply(balance.07.winners, '[[', 2)))

Attrition <- c("1820 Census Match") # group vars
Political  <- c("Candidate","Officeholder")
Titles  <- c("Junior", "Senior")
Surname  <- c("Surname Frequency", "Surname Length")
Counties       <- c("Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")

covars$group <- NA
covars$group[covars$covars %in% Attrition]       <- "Attrition"
covars$group[covars$covars %in% Political]       <- "Political activity"
covars$group[covars$covars %in% Titles]       <- "Generational titles"
covars$group[covars$covars %in% Surname]       <- "Surname characteristics"
covars$group[covars$covars %in% Counties]       <- "County of registration"

offset <- c("   ")
covars$covars <- paste(offset,covars$covars)

covars$order <- 1:nrow(covars)  # reorder  
order <- data.frame(covars= c("Attrition:",
                              " ", # need different size gaps
                              "Political activity:",
                              "  ",
                              "Generational titles:",
                              "   ",
                               "Surname characteristics:",
                              "    ",
                               "County of registration:"
),"X1805.ATE"=NA,"X1805.p"=NA,"X1805.winners.ATE"=NA,"X1805.winners.p"=NA,  
"X1807.winners.ATE"=NA,"X1807.winners.p"=NA,"group"=NA,order=c(.5,1.1,1.5,3.1,3.5,5.1,5.5,7.1,7.5))
covars <- rbind(covars,order)
covars <-covars[order(covars$order),]
covars$covars <- factor(covars$covars,levels=unique(covars$covars)[length(covars$covars):1])

# Create plot 

# Create function for balance plot theme
ThemeBw1 <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_blank(),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1)
    )
}

y.title <- expression(paste(italic("p"), " value"))

winners.losers.05.label <- paste("1805 winners & losers, \n N =", 
                     format(nrow(lot05),big.mark=",",scientific=FALSE,trim=TRUE),"\n")

winners.05.label <- paste("1805 winners, \n N =", 
                           format(nrow(sub.05.winners),big.mark=",",scientific=FALSE,trim=TRUE),"\n")

winners.07.label <- paste("1807 winners, \n N =", 
                          format(nrow(lot07),big.mark=",",scientific=FALSE,trim=TRUE))

p <- ggplot(covars,aes(x=covars)) +  
  coord_flip(ylim = c(0.03, 0.97)) + 
  geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0.05), colour="black", lty=2) +
  geom_point(aes(y=X1805.p, colour="x1805", shape="x1805"), size=3, alpha=0.8) + 
  geom_point(aes(y=X1805.winners.p, colour="x1805.winners", shape="x1805.winners"), size=3, alpha=0.8) + 
  geom_point(aes(y=X1807.winners.p, colour="x1807.winners", shape="x1807.winners"), size=3, alpha=0.8) + 
  scale_y_continuous(name=y.title,breaks=c(0,0.05,0.10,1),labels=c("0","0.05","0.10","1")) + 
  scale_colour_manual(name="Sample",
                      values=c(x1805=wes_palette("Darjeeling")[3], x1805.winners=wes_palette("Darjeeling")[2], x1807.winners=wes_palette("Darjeeling")[1]),
                      label=c(winners.losers.05.label,
                              winners.05.label,
                              winners.07.label)) +
  scale_shape_manual(name="Sample",
                      values=c(19, 17, 15),
                      label=c(winners.losers.05.label,
                              winners.05.label,
                              winners.07.label)) +
  scale_x_discrete(name="") + 
  ThemeBw1()

ggsave(paste0(data.directory,"plots/balance-plot.png"), p, width=11, height=8.5)