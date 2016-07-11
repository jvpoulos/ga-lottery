# Import and merge tax records
source(paste0(data.directory,"tax-digests.R"))
resp.dat <- merge(resp.dat,oh.taxes.pre,by="row.no",all.x=TRUE)
resp.dat <- merge(resp.dat,oh.taxes.post,by="row.no",all.x=TRUE)

if(patient.descriptive){ 
## Create descriptive table
print(tableContinuous(oh.taxes.pre[c("slaves.pre","acres.pre","ptax.pre")], prec = 3,cumsum=FALSE,stats=my.stats)) # pretreatment
print(tableContinuous(oh.taxes.post[c("slaves.post","acres.post","ptax.post")], prec = 3,cumsum=FALSE,stats=my.stats)) # posttreatment
}

## Create pretreatment wealth distribution figure
# Specify wealth variables
wealth.vars <- c("slaves.pre","acres.pre","ptax.pre","slaves.post","acres.post","ptax.post") 

# Make data for histogram
slaves <- melt(data=resp.dat[c("row.no","slaves.pre","slaves.post")], id.vars="row.no")  
acres <- melt(data=resp.dat[c("row.no","acres.pre","acres.post")], id.vars="row.no")  
ptax <- melt(data=resp.dat[c("row.no","ptax.pre","ptax.post")], id.vars="row.no")  

if(patient.descriptive){ 
# Plot the overlaid density of pre- and post-treatment wealth for each measure
slaves.hist <- ggplot(slaves, aes(x=value, fill=variable)) + # slaves
  geom_density(alpha=.3) +
  ylab("") + 
  xlab("Slaves held") +
  xlim(c(0,40)) +
  theme(legend.position="none") 
acres.hist <- ggplot(acres, aes(x=value, fill=variable)) + # acres
  geom_density(alpha=.3) +
  ylab("") + 
  xlab("Land (acres)") +
  xlim(c(0,4000)) +
  theme(legend.position="none") 
ptax.hist <- ggplot(ptax, aes(x=value, fill=variable)) + # ptax
  geom_density(alpha=.3) +
  ylab("") + 
  xlab("Person tax ($)") +
  xlim(c(0,30)) +
  scale_fill_discrete(name="Measurement",
                      labels=c("Pretreatment", "Posttreatment")) + 
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),legend.background = element_rect(colour = "black")) 

# Combine plots
pdf(paste0(data.directory,"assembly-wealth-plot.pdf"), width=8.5, height=11)
grid.arrange(slaves.hist, acres.hist, ptax.hist,
             ncol=3, nrow=1, left="Density", bottom="Measure of wealth")
dev.off() 
}
