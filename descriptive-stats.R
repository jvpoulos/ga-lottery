## Descriptive statistics: individual--level Census data

# Use the 1850 complete--count Census. Restrict sample to male heads of households aged 21 and over who were born in GA and have non--missing surnames and property value. 
# Historical CPI found here: http://www.measuringworth.com/datasets/uscpi/.


# Import Census data
ipums <- read.csv(paste0(data.directory,"ipums-ga-1850.csv"),header=TRUE, sep = ",")

# Remove non-alphabetic characters from surname and make all uppercase
ipums$surname<- trimws(toupper(gsub("[^[:alnum:] ]", "",ipums$NAMELAST)))

# Trim spaces
ipums$surname <- gsub(" ","",ipums$surname)

# Create variable for length of surname
ipums$surname.length <- nchar(ipums$surname)

# Drop surname lengths <3
ipums <- subset(ipums, ipums$surname.length>2)

# Create variable for (relative) frequency of surname
ipums <- transform(ipums, surname.freq = (ave(seq(nrow(ipums)), ipums$surname, FUN=length)))
ipums$surname.relfreq <- ipums$surname.freq/length(ipums$surname)

# Log prop
ipums$logrealprop<- log(ipums$REALPROP+0.001)

# Check correlations
summary(lm(ipums$logrealprop~ipums$surname.length))
summary(lm(ipums$logrealprop~ipums$surname.freq))

# What % under property threshold?
sum(ipums$REALPROP <250) /nrow(ipums)
sum(ipums$REALPROP <500) /nrow(ipums)

# Create summary statistics table.
ipums$literate <- ifelse(ipums$LIT==4,1,0) # create variables
ipums$school <- ifelse(ipums$SCHOOL==2,1,0)
ipums <- cbind(ipums,dummify(as.factor(ipums$OCC)))

ipums$AGE[ipums$AGE==999] <-NA # clean vars

my.stats <- list("n", "min", "mean", "max", "s") # create table
if(patient.descriptive){ 
tableContinuous(vars =ipums[c("surname.length","surname.freq","AGE","REALPROP","literate","school","5","22","39","41","49","54","97","136","157","203","266")], prec = 3,stats=my.stats,cap = "`Surname length' is the character length of surnames. `Surname frequency' is the number of times surnames appear in the sample. `Literate' is a binary variable indicating literacy (can read and write). `In school' is an indicator variable for individuals currently in school. Sample is drawn from the 1850 full--count Census. The occupations dummies indicate contemporary occupational categories. Sample is restricted to male heads of households aged 21 and over who living in Georgia at the time of the census, were born in Georgia, and have non--missing surnames and property value.", lab = "sum-1850")
}

# Calc property wealth deciles
wealth.dec <- subset(ipums, select=c("SERIAL","REALPROP")) %>%
  mutate(quantile = ntile(REALPROP, 10))

sum(wealth.dec$REALPROP[wealth.dec$quantile==10])/sum(wealth.dec$REALPROP)

head(sort(table(ipums$OCCSTR[which(wealth.dec$quantile==10)]), decreasing = TRUE)) # see which occupations are in top decile of wealth
head(sort(table(ipums$COUNTY[which(wealth.dec$quantile==10)]), decreasing = TRUE)) # see which counties are in top decile of wealth

## Descriptive statistics: county--level Census data

##1800 

# Load data and keep GA
counties.00 <- read.csv(paste0(data.directory,"counties-1800.csv"),header=TRUE, sep = ",")
counties.00 <- subset(counties.00, state==44)

# Make county names proper
counties.00$name <- properCase(as.character(counties.00$name))

# Slave population / total population
counties.00$slave.poppc <- counties.00$stot/counties.00$totpop

# Create table 
if(patient.descriptive){ 
print(xtable(counties.00[c("name","wm1625", "wm2644", "wm45","wmtot","wftot","slave.poppc")],digits= 3,caption = "Summary statistics on selected county--level characteristics in the 1800 Census. `Slave pop.' is the slave population over the total population.", lab = "sum-counties-00"),
      include.rownames = FALSE,
      booktabs = TRUE,
      tabular.environment = "longtable",
      floating=FALSE)
}

# Approx pc. eligible white males
(nrow(petd[petd$orphan!=1 & petd$widow!=1,])/(counties.00$wm1625[counties.00$county==0] + counties.00$wm2644[counties.00$county==0] + counties.00$wm45[counties.00$county==0]))*100

## 1820

# Create slave share per county
slave.share.1820 <- ddply(census.1820,~County,summarise,slave.poppc=sum(TotalSlaves)/sum(TotalAllPersons))
slave.share.1820 <- rbind(slave.share.1820, c("Georgia", sum(census.1820$TotalSlaves)/sum(census.1820$TotalAllPersons))) # statewide

# Create Gini Coefficient for slave wealth per county
gini.counties.1820 <- ddply(census.1820,~County,summarise,gini=Gini(slave.wealth))
gini.counties.1820 <- rbind(gini.counties.1820, c("Georgia", Gini(census.1820$slave.wealth))) # statewide

# Make df for table
counties.1820 <- data.frame("name"=gini.counties.1820$County,
                            "gini"=as.numeric(gini.counties.1820$gini),
                            "slave.poppc"=as.numeric(slave.share.1820$slave.poppc))

counties.1820.1805 <- subset(counties.1820, name=="Baldwin" | name== "Bryan"| name=="Bulloch"| name=="Burke"| name=="Camden"| name=="Chatham"| name=="Clarke"| name=="Columbia"| name=="Effingham"| name=="Elbert"| name=="Franklin"| name=="Glynn"| name=="Greene"| name=="Hancock"| name=="Jackson"| name=="Jefferson"| name=="Liberty"| name=="Lincoln"| name=="McIntosh"| name=="Montgomery"| name=="Oglethorpe"| name=="Richmond"| name=="Screven"| name=="Tattnall"| name=="Warren"| name=="Washington"| name=="Wayne" | name=="Wilkes" | name=="Wilkinson" | name=="Georgia")

# Create table 
if(patient.descriptive){ 
  print(xtable(counties.1820.1805[c("name","slave.poppc","gini")],digits= 3,caption = "Summary statistics on selected county--level characteristics in the 1820 Census. `Slave pop.' is the slave population over the total population.  `Wealth Gini' is based on imputed slave wealth.", lab = "sum-counties-20"),
        include.rownames = FALSE,
        booktabs = TRUE,
        tabular.environment = "longtable",
        floating=FALSE)
}

## 1850

# Use historical county--level data to calculate per--acre average farm values for Baldwin, Wayne, and Wilksonson counties in 1850. 
# Deflate to 1805/1807 prices.

# Load data and keep GA
counties <- read.csv(paste0(data.directory,"counties-1850.csv"),header=TRUE, sep = ",")
counties <- subset(counties, state==44)

# Make county names proper
counties$name <- properCase(as.character(counties$name))

# Calculate average farm values (1850$)
counties$avgfarm <- (counties$farmval-counties$equipval)/counties$farms

# Calculate per acre value
counties$avgacre <- (counties$farmval-counties$equipval)/(counties$acunimp+counties$acimp)
round(((counties$avgacre[counties$name=="Baldwin"]+counties$avgacre[counties$name=="Wayne"]+counties$avgacre[counties$name=="Wilkinson"])/3)/(7.57/11.36),2)

# Estimate values of lots in 1805 counties (1805$)
round((counties$avgacre[counties$name=="Baldwin"]*202.5)/(7.57/11.36),2)
round((counties$avgacre[counties$name=="Wayne"]*490)/(7.57/11.36),2)
round((counties$avgacre[counties$name=="Wilkinson"]*202.5)/(7.57/11.36),2)
avg.prize.05 <- round(((counties$avgacre[counties$name=="Baldwin"]*202.5+counties$avgacre[counties$name=="Wayne"]*490+counties$avgacre[counties$name=="Wilkinson"]*202.5)/3)/(7.57/11.36),2)

# Estimate values of lots in 1807 counties (1807$)
round((counties$avgacre[counties$name=="Baldwin"]*202.5)/(7.57/11.2),2)
round((counties$avgacre[counties$name=="Wilkinson"]*202.5)/(7.57/11.2),2)
avg.prize.07 <- round(((counties$avgacre[counties$name=="Baldwin"]*202.5+counties$avgacre[counties$name=="Wilkinson"]*202.5)/2)/(7.57/11.2),2)

# What is ratio of 1805 prize to median income?
avg.prize.05/median(ipums$REALPROP/(7.57/11.36))

# Create Gini Coefficient for realprop per county
gini.counties.1850 <- ddply(ipums,~COUNTY,summarise,gini=Gini(REALPROP))

counties <- merge(counties, gini.counties.1850, by.x=c("county"), by.y=c("COUNTY"), all.x=TRUE) # merge with 1850 county file
counties$gini[counties$name=="Georgia"] <- Gini(ipums$REALPROP) # Statewide gini
  
# Make table for county--level data. 
counties$logfarmval <- log(counties$farmval) # log farm value
counties$logequipval <- log(counties$equipval) # log equipment value
counties$logfarms <- log(counties$farms)   # log farms
counties$logavgfarm <- log(counties$avgfarm) # log avg farm value
counties$logtotalfarmacres <- log(counties$acunimp+counties$acimp) # log total farm acres
counties$slave.poppc <- counties$stot/counties$totpop # slave population / total population

# Keep counties that existed in 1805 + 3 new counties + Georgia
counties1805 <- subset(counties, name=="Baldwin" | name== "Bryan"| name=="Bulloch"| name=="Burke"| name=="Camden"| name=="Chatham"| name=="Clarke"| name=="Columbia"| name=="Effingham"| name=="Elbert"| name=="Franklin"| name=="Glynn"| name=="Greene"| name=="Hancock"| name=="Jackson"| name=="Jefferson"| name=="Liberty"| name=="Lincoln"| name=="Mcintosh"| name=="Montgomery"| name=="Oglethorpe"| name=="Richmond"| name=="Screven"| name=="Tattnall"| name=="Warren"| name=="Washington"| name=="Wayne" | name=="Wilkes" | name=="Wilkinson" | name=="Georgia")

if(patient.descriptive){ 
# Make table
print(xtable(counties1805[c("name","logfarmval","logequipval","logfarms","logavgfarm","logtotalfarmacres","avgacre","slave.poppc","gini")],digits= 3, lab = "sum-counties"),
      include.rownames = FALSE,
      booktabs = TRUE,
      tabular.environment = "longtable",
      floating=FALSE)
}

## 1870

counties.1870 <- read.csv(paste0(data.directory,"counties-1870.csv"), stringsAsFactors=FALSE)

# Create table 
if(patient.descriptive){ 
  print(xtable(counties.1870[c("county","tax","pop","tax.pc")],digits= 3,caption = "Summary statistics on selected county--level characteristics in the 1870 Census.", lab = "sum-counties-70"),
        include.rownames = FALSE,
        booktabs = TRUE,
        tabular.environment = "longtable",
        floating=FALSE)
}
## Plot slave wealth densities (1820) by treatment status

# Make data for histogram
slave.wealth.plot.df <- melt(data=lot05[!is.na(lot05$slave.wealth.1820),][c("slave.wealth.1820","treat")], 
                          id.vars="treat") 

if(patient.descriptive){ 
  slave.wealth.plot <- ggplot(slave.wealth.plot.df, aes(x=value, fill=as.factor(treat))) + 
    geom_density(alpha=.3) +
    ylab("Density") + 
    xlab("Slave wealth (1820$)") +
#     ggtitle(paste("Distribution of slave wealth by treatment status, N =", 
#                   format(nrow(lot05[!is.na(lot05$slave.wealth.1820),]),big.mark=",",scientific=FALSE,trim=TRUE))) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma, limits = c(0,15000)) +
    scale_fill_manual(values = c("red","blue"), labels= c("Control", "Treated"), name="Treatment status") +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1),legend.background = element_rect(colour = "black"))
  
  ggsave(paste0(data.directory,"plots/slave-wealth-1820.png"), slave.wealth.plot, width=11, height=8.5)
}


## Plot densities of time lag in filing grants by lottery year and county in which land is drawn

# Convert grant dates to POSIXct
fdg05$grant.date <- as.POSIXct(fdg05$grant.date,format="%Y-%m-%d")
fdg07$grant.date <- as.POSIXct(fdg07$grant.date,format="%Y-%m-%d")

# Make variable for county in which land is drawn
fdg05$county.drawn <- NA
fdg05$county.drawn[substr(fdg05$prize,1,2)=="Ba"] <- "Baldwin"
fdg05$county.drawn[substr(fdg05$prize,1,2)=="Wa"] <- "Wayne"
fdg05$county.drawn[substr(fdg05$prize,1,2)=="Wi"] <- "Wilkinson"

fdg07$county.drawn <- NA
fdg07$county.drawn[substr(fdg07$prize,1,2)=="Ba"] <- "Baldwin"
fdg07$county.drawn[substr(fdg07$prize,1,2)=="Wi"] <- "Wilkinson"

# Make variable for reverted lot
fdg05$rgb <- 0 
fdg05$rgb[c(grep("RGB",fdg05$grant.book))] <- 1

fdg07$rgb <- 0 
fdg07$rgb[c(grep("Rev",fdg07$grant.book))] <- 1

# Calculate days from first day of grant claiming
fdg05$time.lapse <- as.numeric(round(difftime(fdg05$grant.date, 
                                              as.POSIXct("1805-09-03",format="%Y-%m-%d"), units = "days")))

fdg07$time.lapse <- as.numeric(round(difftime(fdg07$grant.date, 
                                              as.POSIXct("1807-09-30",format="%Y-%m-%d"), units = "days")))

# Make data for histogram
time.lapse.plot05 <- melt(data=fdg05[fdg05$rgb!=1,][c("time.lapse","county.drawn")], # exclude reverted lots
                          id.vars="county.drawn") 
time.lapse.plot07 <- melt(data=fdg07[fdg07$rgb!=1,][c("time.lapse","county.drawn")], # exclude reverted lots
                          id.vars="county.drawn") 

if(patient.descriptive){ 
# Plot the overlaid density of time lapse by county drawn for each lottery
time.lapse.hist05 <- ggplot(time.lapse.plot05, aes(x=value, fill=county.drawn)) + 
  geom_density(alpha=.3) +
  ylab("") + 
  xlab("") +
  ggtitle(paste("1805 lottery, N =", 
                format(nrow(fdg05),big.mark=",",scientific=FALSE,trim=TRUE))) +
  xlim(c(0,3000)) +
  scale_fill_manual(values = c("red","yellow","green"), name="County drawn") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),legend.background = element_rect(colour = "black"))

time.lapse.hist07 <- ggplot(time.lapse.plot07, aes(x=value, fill=county.drawn)) + 
  geom_density(alpha=.3) +
  ylab("") + 
  xlab("") +
  ggtitle(paste("1807 lottery, N =", 
                format(nrow(fdg07),big.mark=",",scientific=FALSE,trim=TRUE))) +
  xlim(c(0,3000)) +
  scale_fill_manual(values = c("red","green"), name="County drawn") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),legend.background = element_rect(colour = "black"))

# Combine plots
ggsave(paste0(data.directory,"plots/time-lapse.png"), grid.arrange(time.lapse.hist05, time.lapse.hist07,
                                                                   ncol=2, nrow=1, left="Density", bottom="# of days since start of grant claiming"), 
       width=11, height=8.5)
}