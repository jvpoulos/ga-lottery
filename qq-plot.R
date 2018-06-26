## Make QQ plots for continuous name variables. 

if(patient.descriptive){ 

## 1805 winners and losers
  
# surname frequency

png(paste0(data.directory,"plots/qq-freq.png"))
qqplot(lot05$surname.freq[lot05$treat==1],lot05$surname.freq[lot05$treat==0],xlim=c(50,350),ylim=c(50,350),ylab="Control",xlab="Treated",main="Surname frequency")
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()  

# surname length

png(paste0(data.directory,"plots/qq-length.png"))
qqplot(lot05$surname.length[lot05$treat==1],lot05$surname.length[lot05$treat==0],xlim=c(1,15),ylim=c(1,15),ylab="Control",xlab="Treated",main="Surname length")
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()  

# 1820 Census Match

png(paste0(data.directory,"plots/qq-census.png"))
qqplot(lot05$match.census.05[lot05$treat==1],lot05$match.census.05[lot05$treat==0],xlim=c(0,1),ylim=c(0,1),ylab="Control",xlab="Treated",main="1820 Census Match")
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()  

## 1805 winners

# surname frequency

png(paste0(data.directory,"plots/qq-freq-05-winners.png"))
qqplot(sub.05.winners$surname.freq[sub.05.winners$treat==1],sub.05.winners$surname.freq[sub.05.winners$treat==0],xlim=c(50,350),ylim=c(50,350),ylab="Control",xlab="Treated",main="Surname frequency")
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()  

# surname length

png(paste0(data.directory,"plots/qq-length-05-winners.png"))
qqplot(sub.05.winners$surname.length[sub.05.winners$treat==1],sub.05.winners$surname.length[sub.05.winners$treat==0],xlim=c(1,15),ylim=c(1,15),ylab="Control",xlab="Treated",main="Surname length")
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()  

# 1820 Census Match

png(paste0(data.directory,"plots/qq-census-05-winners.png"))
qqplot(sub.05.winners$match.census.05[sub.05.winners$treat==1],sub.05.winners$match.census.05[sub.05.winners$treat==0],xlim=c(0,1),ylim=c(0,1),ylab="Control",xlab="Treated",main="1820 Census Match")
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()  

## 1807 winners and losers

# surname frequency

png(paste0(data.directory,"plots/qq-freq-07-winners.png"))
qqplot(lot07$surname.freq[lot07$treat==1],lot07$surname.freq[lot07$treat==0],xlim=c(50,350),ylim=c(50,350),ylab="Control",xlab="Treated",main="Surname frequency")
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()  

# surname length

png(paste0(data.directory,"plots/qq-length-07-winners.png"))
qqplot(lot07$surname.length[lot07$treat==1],lot07$surname.length[lot07$treat==0],xlim=c(1,15),ylim=c(1,15),ylab="Control",xlab="Treated",main="Surname length")
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()  

# 1820 Census Match

png(paste0(data.directory,"plots/qq-census-07-winners.png"))
qqplot(lot07$match.census.07[lot07$treat==1],lot07$match.census.07[lot07$treat==0],xlim=c(0,1),ylim=c(0,1),ylab="Control",xlab="Treated",main="1820 Census Match")
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()  
}