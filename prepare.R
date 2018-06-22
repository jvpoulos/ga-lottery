## Prepare 1805 lottery data 

# Create dummy for never-treat
lot05$rgb <- 0 
lot05$rgb[c(grep("RGB",lot05$grant.book.x),grep("RGB",lot05$grant.book.y))] <- 1

# Calculate compliance rate
beta.hat <- 1-(sum(lot05$rgb)/sum(lot05$treat))

# Make indicators binary

lot05$prior.office[is.na(lot05$prior.office)] <- 0 
lot05$oh[is.na(lot05$oh)] <- 0

lot05$prior.run[is.na(lot05$prior.run)] <- 0
lot05$candidate[is.na(lot05$candidate)] <- 0

# Match NAs 0

lot05$match.votes.05[is.na(lot05$match.votes.05)] <- 0
lot05$match.census.05[is.na(lot05$match.census.05)] <- 0

# Count # of prizes
prizes <- sum(lot05$treat) + sum(lot05$treat2)

# Count # tickets in box
tickets <- nrow(lot05) + nrow(lot05[lot05$n.draw==2,])

# Calculate P(Z=1) 
lot05$p.score <- ifelse(lot05$n.draw==2,2*(prizes/tickets),prizes/tickets) # calculated using all participants

# Create column of weights 
lot05$weight <- (lot05$treat)/(lot05$p.score) + (1-lot05$treat)/(1-lot05$p.score)

# Create # prizes
lot05$n.prizes <- lot05$treat + lot05$treat2

## Prepare 1807 lottery data 

# Calculate # prizes and rm multiple entries
lot07 <- fdg07 %>% 
  add_count(fortunate.drawer,county)

lot07 <- lot07[!duplicated(lot07[c("fortunate.drawer","county")]),]

# Rm empty surname
lot07 <- lot07[!is.na(lot07$surname),]

# n prizes max to one
colnames(lot07)[42] <- "n.prizes"

lot07$n.prizes[lot07$n.prizes>2] <- 2

# treatment indicator
lot07$treat <- NA
lot07$treat[lot07$n.prizes==1] <- 0  # reformat treat among winners
lot07$treat[lot07$n.prizes==2] <- 1

# impute # draws
lot07$n.draw<- NA
lot07$n.draw <- 1
lot07$n.draw[lot07$orphan==1] <- 2
lot07$n.draw[lot07$n.prizes==2] <- 2

# Make indicators binary

lot07$prior.office[is.na(lot07$prior.office)] <- 0 
lot07$oh[is.na(lot07$oh)] <- 0

lot07$prior.run[is.na(lot07$prior.run)] <- 0
lot07$candidate[is.na(lot07$candidate)] <- 0

# Match census NAs 0

lot07$match.votes.07[is.na(lot07$match.votes.07)] <- 0
lot07$match.census.07[is.na(lot07$match.census.07)] <- 0

# Count # of prizes
prizes.07 <- sum(lot07$n.prizes) #  PK:  11,411

# Count # tickets in box
tickets.07 <- nrow(lot07) + nrow(lot07[lot07$n.draw==2,])

# Calculate P(Z=1) 
lot07$p.score <- ifelse(lot07$n.draw==2,2*(prizes.07/tickets.07),prizes.07/tickets.07) # calculated using all participants

# Create column of weights 
lot07$weight <- (lot07$n.prizes)/(lot07$p.score) + (1-lot07$n.prizes)/(1-lot07$p.score)

# Make df
lot07 <- data.frame(lot07)

# If FD county missing, fill in with census matched county

lot07$county[is.na(lot07$county)] <- lot07$county.census[is.na(lot07$county)] 

# create military variable
lot07$military <- ifelse(lot07$captain==1 | lot07$general==1,1,0) 

# recode Junior and Senior
lot07$junior <- as.numeric(lot07$junior) 
lot07$junior[lot07$junior==1] <- 0
lot07$junior[lot07$junior==2] <- 1

lot07$senior <- as.numeric(lot07$senior) 
lot07$senior[lot07$senior==1] <- 0
lot07$senior[lot07$senior==2] <- 1

# create county of registration dummies 
lot07 <- cbind(lot07,dummify(as.factor(lot07$county))) 

## Run balance tests and plots

source(paste0(data.directory,"balance-tests.R"))
source(paste0(data.directory,"balance-plot.R")) 

## Create sample exclusions
sub.oh.05 <- lot05[(lot05$orphan!=1) & (lot05$widow!=1),] # exclude orphans, widows
sub.1820.05 <- lot05[!is.na(lot05$match.census),] # matched to 1820 Census

sub.oh.05.winners <- lot05[(lot05$orphan!=1) & (lot05$widow!=1) & (lot05$treat==1),] # winners only
sub.1820.05.winners <- lot05[!is.na(lot05$match.census) & (lot05$treat==1),] 

sub.oh.05.winners$treat[sub.oh.05.winners$n.prizes==1] <- 0  # reformat treat among winners
sub.oh.05.winners$treat[sub.oh.05.winners$n.prizes==2] <- 1

sub.oh.07.winners <- lot07[(lot07$orphan!=1) & (lot07$widow!=1),] # winners only
sub.1820.07.winners <- lot07[!is.na(lot07$match.census.07),] 

sub.oh.07.winners$treat<- NA
sub.oh.07.winners$treat[sub.oh.07.winners$n.prizes==1] <- 0  # reformat treat among winners
sub.oh.07.winners$treat[sub.oh.07.winners$n.prizes==2] <- 1

## Create weighted slave wealth

sub.1820.05$slave.wealth.1820.w <- sub.1820.05$slave.wealth.1820 * sub.1820.05$match.census.05
sub.1820.05.winners$slave.wealth.1820.w <- sub.1820.05.winners$slave.wealth.1820 * sub.1820.05.winners$match.census.05

sub.1820.05.winners$treat[sub.1820.05.winners$n.prizes==1] <- 0  # reformat treat among winners
sub.1820.05.winners$treat[sub.1820.05.winners$n.prizes==2] <- 1

sub.1820.07.winners$slave.wealth.1820.w <- sub.1820.07.winners$slave.wealth.1820 * sub.1820.07.winners$match.census.07

sub.1820.07.winners$treat <- NA
sub.1820.07.winners$treat[sub.1820.07.winners$n.prizes==1] <- 0  # reformat treat among winners
sub.1820.07.winners$treat[sub.1820.07.winners$n.prizes==2] <- 1