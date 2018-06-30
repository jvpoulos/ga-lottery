## Create table showing outcomes by sample
if(patient.descriptive){ 
  my.stats <- list("n", "min", "mean", "max", "s") 
  
  # Pretreatment Variables

  pretreatment.desc <- rbind(lot05[covars.balance], sub.05.winners[covars.balance], lot07[covars.balance])
  
  pretreatment.desc$sample <- c(rep("1805 winners & losers", nrow(lot05)), 
                                rep("1805 winners", nrow(sub.05.winners)),
                                rep("1807 winners", nrow(lot07)))
  
  pretreatment.desc$sample <- factor(pretreatment.desc$sample)
  
  pretreatment.desc$census.match <- unlist(c(lot05["match.census.05"], sub.05.winners["match.census.05"], lot07["match.census.07"]))
  
  print(tableContinuous(vars = pretreatment.desc[c("census.match",covars.balance)], 
                        group = pretreatment.desc$sample, 
                        prec = 2,
                        cumsum=FALSE,
                        nams = covars.names,
                        stats=my.stats))
  
  # treatment Variables
  
  treatment.desc <- rbind(lot05[c("treat","n.draw")], sub.05.winners[c("treat","n.draw")], lot07[c("treat","n.draw")])
  
  treatment.desc$sample <- c(rep("1805 winners & losers", nrow(lot05)), 
                                rep("1805 winners", nrow(sub.05.winners)),
                                rep("1807 winners", nrow(lot07)))
  
  treatment.desc$sample <- factor(treatment.desc$sample)
  
  print(tableContinuous(vars = treatment.desc[c(c("treat","n.draw"))], 
                        group = treatment.desc$sample, 
                        prec = 2,
                        cumsum=FALSE,
                        nams = c("Treated", "# draws"),
                        stats=my.stats))
  
  # Outcomes
  # Political outcomes exlude widows, orphans, female 
  
  political.desc <- rbind(sub.oh.05[c("candidate","oh","match.oh")], 
                          sub.oh.05.winners[c("candidate","oh","match.oh")],
                          sub.oh.07.winners[c("candidate","oh","match.oh")])
  
  political.desc$sample <- c(rep("1805 winners & losers", nrow(sub.oh.05)), 
                                rep("1805 winners", nrow(sub.oh.05.winners)),
                                rep("1807 winners", nrow(sub.oh.07.winners)))
  
  political.desc$sample <- factor(political.desc$sample)
  
  print(tableContinuous(vars = political.desc[c("candidate","oh","match.oh")], 
                  group = political.desc$sample, 
                  prec = 2,
                  cumsum=FALSE,
                  nams = c("Candidate", "Officeholder", "Officeholder (match prob.)"),
                  stats=my.stats))
  
  # Census wealth (no exclusions)
  
  census.desc <- rbind(sub.1820.05[c("slave.wealth.1820")], 
                          sub.1820.05.winners[c("slave.wealth.1820")],
                          sub.1820.07.winners[c("slave.wealth.1820")])
  
  census.desc$sample <- c(rep("1805 winners & losers", nrow(sub.1820.05)), 
                             rep("1805 winners", nrow(sub.1820.05.winners)),
                             rep("1807 winners", nrow(sub.1820.07.winners)))
  
  census.desc$sample <- factor(census.desc$sample)
  
  print(tableContinuous(vars = census.desc[c("slave.wealth.1820")], 
                  group = census.desc$sample, 
                  prec = 2,
                  cumsum=FALSE,
                  nams = ("Slave wealth (1820$)"),
                  stats=my.stats))
}