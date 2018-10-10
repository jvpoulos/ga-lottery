## Prepare 1805 lottery data 

# Make indicators binary

lot05$prior.office05[is.na(lot05$prior.office05)] <- 0 
lot05$oh[is.na(lot05$oh)] <- 0

lot05$prior.run05[is.na(lot05$prior.run05)] <- 0
lot05$candidate[is.na(lot05$candidate)] <- 0

# Match NAs 0

lot05$match.votes.05[is.na(lot05$match.votes.05)] <- 0
lot05$match.census.05[is.na(lot05$match.census.05)] <- 0

# Create # prizes
lot05$n.prizes <- lot05$treat + lot05$treat2

# female participant
lot05$female <- 0
lot05$female[lot05$first.name %in% female.name] <- 1

## Prepare 1807 lottery data 

# Calculate # prizes and rm multiple entries
lot07 <- fdg07 %>% 
  add_count(fortunate.drawer,county)

lot07 <- lot07[!duplicated(lot07[c("fortunate.drawer","county")]),]

# Rm empty surname
lot07 <- lot07[!is.na(lot07$surname),]

# n prizes max to one
colnames(lot07)[ncol(lot07)] <- "n.prizes"

lot07$n.prizes[lot07$n.prizes>2] <- 2

# treatment indicator
lot07$treat <- NA
lot07$treat[lot07$n.prizes==1] <- 0  # reformat treat among winners
lot07$treat[lot07$n.prizes==2] <- 1

# Make indicators binary

lot07$prior.office07[is.na(lot07$prior.office07)] <- 0 
lot07$oh[is.na(lot07$oh)] <- 0

lot07$prior.run07[is.na(lot07$prior.run07)] <- 0
lot07$candidate[is.na(lot07$candidate)] <- 0

# Match NAs 0

lot07$match.votes.07[is.na(lot07$match.votes.07)] <- 0
lot07$match.census.07[is.na(lot07$match.census.07)] <- 0

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

# female participant
lot07$female <- 0
lot07$female[lot07$first.name %in% female.name] <- 1

# impute # draws
lot07$n.draw<- NA
lot07$n.draw <- 2
lot07$n.draw[lot07$orphan==1] <- 1
lot07$n.draw[lot07$widow==1] <- 1
lot07$n.draw[lot07$female==1] <- 1
lot07$n.draw[lot07$n.prizes==2] <- 2 

## Create sample exclusions
sub.oh.05 <- lot05[(lot05$orphan!=1) & (lot05$widow!=1) & (lot05$female!=1),] # exclude orphans, widows, females
sub.1820.05 <- lot05[lot05$match.census>0,] # matched to 1820 Census

sub.oh.05.winners <- lot05[(lot05$orphan!=1) & (lot05$widow!=1) & (lot05$female!=1) & (lot05$treat==1),] # winners only
sub.1820.05.winners <- lot05[(lot05$match.census>0) & (lot05$treat==1),] 

sub.oh.05.winners$treat[sub.oh.05.winners$n.prizes==1] <- 0  # reformat treat among winners
sub.oh.05.winners$treat[sub.oh.05.winners$n.prizes==2] <- 1

sub.oh.07.winners <- lot07[(lot07$orphan!=1) & (lot07$widow!=1) & (lot07$female!=1),] # winners only
sub.1820.07.winners <- lot07[lot07$match.census.07>0,] 

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