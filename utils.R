# Forest plot for summary figure
ForestPlot <- function(d, xlab, ylab){
  p <- ggplot(d, aes(x=x, y = y, ymin=y.lo, ymax=y.hi,colour=Outcome)) + 
    geom_pointrange(size=1.2, alpha=0.9) + 
    coord_flip() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
    theme(legend.position="none") +
    facet_grid(Outcome~.) +
    ylab(xlab) +
    xlab(ylab) #switch because of the coord_flip() above
  return(p)
}

# Gini function

Gini <- function (x, weights = rep(1, length = length(x))) {
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}

# Transform continuous variable to 0-1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# Create list of female names
female.name <- c("Mary","Ann","Anne","Fanny","Elizabeth","Jane","Holly","Hannah","Agnes","Rebecca","Rachel","Frances",
                 "Dolly","Judith","Nancy","Margaret","Lucy","Catharine","Sally","Peggy","Rose","Milly","Priscilla","Charity",
                 "Ruth","Sally","Celia","Martha","Penelope","Delilah","Susannah","Louisa","Temperence","Avey","Patsy","Lydia",
                 "Drucilla","Mildred","Eliza","Rhoda","Cynthia","Abigail","Bridgett","Elkanah","Deborah","Phebe","Elenor",
                 "Winfred","Kitty","Seley","Aby","Jemimah","Winnefred","Betsey","Esther","Aley","Rosemon","Caty","Dorrotha",
                 "Prudence","Lucinda","Darcus","Misseniah","Christianna","Olevia","Lethee","Henrietta","Lucy","Silvany",
                 "Isabella","Violet","Anna","Pancy","Evey","Tabitha","Avera","Keziah","Harriett","Polly","Kezie")

female.name <- c(female.name, "Abigail","Aby", "Agnes",  "Aley","Ann", "Anna","Annis",  "Arney", # 1805 widow names
                 "Arum","Avera",  "Avey","Betsy",  "Bridgett", "Catern",
                 "Catharine","Catherine","Caty","Cecelia","Celia",  "Charity","Chloe", 
                 "Christianna", "Cynthia", "Darcus", "Deborah","Delilah","Dicy","Dolly", 
                 "Dorcas", "Dorrotha", "Drucilla", "Edith",  "Elenor", "Eliza",  "Elizabeth","Elkanah", 
                 "Esther", "Etheldred","Evey","Fanny",  "Frances","Francinia","Francis", 
                 "Frankey","Ganett", "Ginsey", "Hannah", "Henrietta","Hester", "Holly",  "Isaa",  
                 "Isabella",  "Jane","Jemimah","Joanna", "Judith", "Keziah", "Kitty", 
                 "Leety",  "Lethee", "Louisa", "Louisa", "Lucinda","Luckey", "Lucy",  
                 "Lydia", "Macy","Magaret","Margaret", "Margarett","Marget", "Martha",
                 "Mary","Mildred","Milly",  "Morning","Nancy",  "Olevia",  "Pancy", 
                 "Patience", "Patsy",  "Patty",  "Peggy",  "Penelope", "Phebe",  "Polly",  "Priscilla",  
                 "Prudence", "Rachel", "Rebecca","Rhoda",  "Rosamond", "Rose","Rosemon", 
                 "Ruth", "Sally",  "Saloma", "Sara","Sarah",  "Satcey", "Seley", 
                 "Sevillity","Silvany","Susannah", "Syntha", "Tabitha","Temperence",  "Tempy",  "Verlinda",
                 "Violet", "Widow",  "Winfred","Winnefred")

female.name <- c(female.name, "Abigail","Acceth", "Agatha", "Aley",   "Alice",  "Amelia", "Amy","Ann",   # 1807 widow first names
                 "Anna",   "Anne",   "Arney",  "Barbara","Betsey", "Betsy",  "Biddy",  "Catharine",  
                 "Cecelia","Celia",  "Chaney", "Charity","Christiana",  "Christina",   "Darcas",
                 "Darcus", "Delila", "Dinah",  "Drusilla","Easter", "Eleanor","Eliza",  "Elizabeth",  
                 "Ellender","Esther", "Eve","Faithy", "Farney", "Frances","Hannah", "Harmah",
                 "Isabella","Jane",  "Jinny",  "Judith", "Judy",   "Julia",  "Keziah",
                 "Leander","Lettice","Levine", "Liza",   "Lucind", "Lucretia","Lucy",   "Lydia", 
                 "Margaret","Margarett",   "Mariam", "Martha", "Mary",   "Massey", "Mazy",   "Milly", 
                 "Miram",  "Molly",  "Nancy",  "Nelly",  "Olive",  "Patience","Patsy",  "Patty", 
                 "Peggy",  "Penelope","Phebe",  "Polly",  "Priscilla",   "Rachel", "Rebecca","Rhoda", 
                 "Rosanna","Rose",   "Ruth",   "Sally",  "Sarah",  "Serlester",   "Susannah","Susannahnes",
                 "Sylvanyne",   "Thamer", "Unity",  "Urshla", "Winne",  "Winney", "Winnifred")

female.name <- sort(unique(female.name))