# Get ATEs and p values

covars.balance <- c("junior","senior","surname.freq","surname.length","Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")

# 1805 winners and losers
covars.balance.05 <- c("match.census.05","prior.run05","prior.office05",covars.balance)

balance <- lapply(covars.balance.05, 
                  function(x){
                    lm05 <- lm(lot05[,x] ~ lot05$treat + lot05$n.draw)
                    return(list("ATE"=lm05$coefficients['lot05$treat'][[1]],
                                "p" = summary(lm05)$coefficients[11]))})

# 1805 winners
sub.05.winners <- lot05[(lot05$treat==1),] # winners only

sub.05.winners$treat[sub.05.winners$n.prizes==1] <- 0  # reformat treat among winners
sub.05.winners$treat[sub.05.winners$n.prizes==2] <- 1

covars.balance.05.winners <- c("match.census.05","prior.run05","prior.office05",covars.balance)

balance.05.winners <- lapply(covars.balance.05.winners, function(x){
  lm05.winners <- lm(sub.05.winners[,x] ~ sub.05.winners$treat + sub.05.winners$n.draw)
  return(list("ATE"=lm05.winners$coefficients['sub.05.winners$treat'][[1]],
              "p" = summary(lm05.winners)$coefficients[11]))})

# 1807 winners
covars.balance.07.winners <- c("match.census.07","prior.run07","prior.office07",covars.balance)
covars.balance.07.winners <- covars.balance.07.winners[!covars.balance.07.winners %in% setdiff(covars.balance.07.winners, colnames(lot07))] # doesn't have: "bricklayer"   "merchant"     "schoolmaster"

balance.07.winners <- lapply(covars.balance.07.winners, function(x){
  lm07 <- lm(lot07[,x] ~ lot07$treat + lot07$n.draw)
  return(list("ATE"=lm07$coefficients['lot07$treat'][[1]],
              "p" = summary(lm07)$coefficients[11]))})
