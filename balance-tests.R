# Estimate p values for balance plot

covars.balance <- c("junior","senior","surname.freq","surname.length","blacksmith","bricklayer","hatter","esquire","merchant","military","doctor","reverend","schoolmaster","Bryan","Bulloch","Burke","Camden","Chatham","Clarke","Columbia","Effingham","Elbert","Franklin","Glynn","Greene","Hancock","Jackson","Jefferson","Liberty","Lincoln","McIntosh","Montgomery","Oglethorpe","Richmond","Screven","Tattnall","Warren","Washington","Wilkes")

balance.p <- sapply(covars.balance, function(x) PermutationTest(y=lot05[,x],
                                                                treat=resp.dat$treat,
                                                                w=resp.dat$weight,
                                                                p.score=resp.dat$p.score)$p)
# Output p values as .txt file
write.table(cbind(covars.balance,balance.p), paste0(data.directory,"balance-tests.txt"), col.names=FALSE)
