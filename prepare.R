## Prepare 1805 lottery data 

# Create dummy for never-treat
lot05$rgb <- 0 
lot05$rgb[c(grep("RGB",lot05$grant.book.x),grep("RGB",lot05$grant.book.y))] <- 1

# Calculate compliance rate
beta.hat <- 1-(sum(lot05$rgb)/sum(lot05$treat))

# Merge officeholder info with lottery data
resp.dat <- merge(lot05[c("row.no","county","treat","treat2","prize","prize2","orphan","widow","rgb","n.draw","no.slaves.1820","candidate","prior.run","match.votes","match.census")],
                  officeholders[c("row.no","prior.office","slave.index","bank.index","oh","n.post.terms","match.oh")],by="row.no", all.x=TRUE)

resp.dat$prior.office[is.na(resp.dat$prior.office)] <- 0 # make binary
resp.dat$oh[is.na(resp.dat$oh)] <- 0

resp.dat$prior.run[is.na(resp.dat$prior.run)] <- 0
resp.dat$candidate[is.na(resp.dat$candidate)] <- 0

# Count # of prizes
prizes <- sum(resp.dat$treat) + sum(resp.dat$treat2)

# Count # tickets in box
tickets <- nrow(resp.dat) + nrow(resp.dat[resp.dat$n.draw==2,])

# Calculate P(Z=1) 
resp.dat$p.score <- ifelse(resp.dat$n.draw==2,2*(prizes/tickets),prizes/tickets) # calculated using all participants

# Create column of weights 
resp.dat$weight <- (resp.dat$treat)/(resp.dat$p.score) + (1-resp.dat$treat)/(1-resp.dat$p.score)

## Run balance tests and plots

source(paste0(data.directory,"balance-tests.R"))
source(paste0(data.directory,"balance-plot.R")) 
source(paste0(data.directory,"qq-plot.R")) 
source(paste0(data.directory,"tax-records.R"))

## Create sample exclusions
sub.oh <- resp.dat[(resp.dat$prior.office!=1) & (resp.dat$orphan!=1) & (resp.dat$widow!=1),] # exclude orphans, widows, pretreatment officeholders
sub.candidate <- resp.dat[(resp.dat$prior.run!=1) & (resp.dat$prior.office!=1) & (resp.dat$orphan!=1) & (resp.dat$widow!=1),] # exclude orphans, widows, pretreatment candidates + officeholders
sub.prior <- resp.dat[(resp.dat$prior.office==1),] # only pretreatment officeholders