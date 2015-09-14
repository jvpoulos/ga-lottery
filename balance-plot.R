# Create function for plot theme

ThemeBw1 <- function(base_size = 11, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_blank(),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}

# Create balance plot data

balance.tests <- read.table(paste0(data.directory,"balance-tests.txt"), quote="\"", comment.char="") # upload balance test p values

covars.names <- c("Junior","Senior","Surname frequency","Surname length","Blacksmith","Bricklayer","Hatter","Lawyer","Merchant","Military","Physician","Reverend","Teacher","Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")

covars <- data.frame("covars"=covars.names,
                     "p"=balance.tests[,3])

Generational  <- c("Junior","Senior") # group vars
Surname    <- c("Surname frequency","Surname length")
Occupations       <- c("Blacksmith","Bricklayer","Hatter","Lawyer","Merchant","Military","Physician","Reverend","Teacher")
Counties       <- c("Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")

covars$group <- NA
covars$group[covars$covars %in% Generational]       <- "Generational titles"
covars$group[covars$covars %in% Surname]       <- "Surname characteristics"
covars$group[covars$covars %in% Occupations]       <- "Occupations"
covars$group[covars$covars %in% Counties]       <- "County of registration"

offset <- c("   ")
covars$covars <- paste(offset,covars$covars)

covars$order <- 1:nrow(covars)  # reorder  
order <- data.frame(covars= c("Generational titles:",
                              "  ",
                              "Surname characteristics:",
                              "   ",
                              "Occupations:",
                              "    ",
                              "County of registration:"
),order=c(.5,2.1,2.5,4.1,4.5,13.1,13.5),
p=NA,group=NA)
covars <- rbind(covars,order)
covars <-covars[order(covars$order),]
covars$covars <- factor(covars$covars,levels=unique(covars$covars)[length(covars$covars):1])

# Create plot 

p <- ggplot(covars,aes(y=p,x=covars,colour=group)) +  
  coord_flip(ylim = c(0, 1)) + 
  geom_hline(yintercept = 0.05,size=.5,colour="blue",linetype="dotted") +
  geom_point() + 
  scale_y_continuous(name="Randomization p value",breaks=c(0,0.05,0.10,1),labels=c("0","0.05","0.10","1")) + 
  scale_x_discrete(name="") + 
  ThemeBw1()

ggsave(paste0(data.directory,"balance-plot.pdf"), p, width=8.5, height=11)