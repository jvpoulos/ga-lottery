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

# Count # of prizes
prizes <- sum(lot05$treat) + sum(lot05$treat2)

# Count # tickets in box
tickets <- nrow(lot05) + nrow(lot05[lot05$n.draw==2,])

# Calculate P(Z=1) 
lot05$p.score <- ifelse(lot05$n.draw==2,2*(prizes/tickets),prizes/tickets) # calculated using all participants

# Create column of weights 
lot05$weight <- (lot05$treat)/(lot05$p.score) + (1-lot05$treat)/(1-lot05$p.score)

## Run balance tests and plots

source(paste0(data.directory,"balance-tests.R"))
source(paste0(data.directory,"balance-plot.R")) 
source(paste0(data.directory,"qq-plot.R")) 

## Create sample exclusions * Don't exclude pretreatment officeholders
sub.oh <- lot05[(lot05$orphan!=1) & (lot05$widow!=1),] # exclude orphans, widows
sub.candidate <- lot05[(lot05$orphan!=1) & (lot05$widow!=1),] # exclude orphans, widows
sub.1820 <- lot05[!is.na(lot05$match.census),] # matched to 1820 Census