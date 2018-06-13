# Create balance plot data
covars.names <- c("Candidate","Officeholder","Junior","Senior","Surname frequency","Surname length","Blacksmith","Bricklayer","Hatter","Lawyer","Merchant","Military","Physician","Reverend","Teacher","Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")

if(patient.balance){ 
  balance.tests <- read.table(paste0(data.directory,"results/balance-tests.txt"), quote="\"", comment.char="") # upload balance test p values
  
  balance.tests$adjusted.p <- p.adjust(balance.tests$V3, method="hommel")
  
  covars <- data.frame("covars"=covars.names,
                       "p"=balance.tests[,3])
  
  Political  <- c("Candidate","Officeholder") # group vars
  Surname    <- c("Junior","Senior","Surname frequency","Surname length")
  Occupations       <- c("Blacksmith","Bricklayer","Hatter","Lawyer","Merchant","Military","Physician","Reverend","Teacher")
  Counties       <- c("Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")
  
  covars$group <- NA
  covars$group[covars$covars %in% Political]       <- "Political activity"
  covars$group[covars$covars %in% Surname]       <- "Names"
  covars$group[covars$covars %in% Occupations]       <- "Occupations"
  covars$group[covars$covars %in% Counties]       <- "County of registration"
  
  offset <- c("   ")
  covars$covars <- paste(offset,covars$covars)
  
  covars$order <- 1:nrow(covars)  # reorder  
  order <- data.frame(covars= c("Political activity:",
                                "  ",
                                "Names:",
                                "   ",
                                "Occupations:",
                                "    ",
                                "County of registration:"
  ),order=c(.5,2.1,2.5,6.1,6.5,15.1,15.5),
  p=NA,group=NA)
  covars <- rbind(covars,order)
  covars <-covars[order(covars$order),]
  covars$covars <- factor(covars$covars,levels=unique(covars$covars)[length(covars$covars):1])
  
  # Create plot 
  
  p <- ggplot(covars,aes(y=p,x=covars,colour=group)) +  
    coord_flip(ylim = c(0.03, 0.97)) + 
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0.05), colour="black", lty=2) +
    geom_point(size=4, alpha=0.9) + 
    scale_y_continuous(name="p-value",breaks=c(0,0.05,0.10,1),labels=c("0","0.05","0.10","1")) + 
    scale_x_discrete(name="") + 
    ThemeBw1()
  
  ggsave(paste0(data.directory,"plots/balance-plot.png"), p, width=11, height=8.5)
}