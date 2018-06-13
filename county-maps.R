# Note: descriptive-stats.R must be run before this script

# Packages
require(ggplot2)
require(rgdal)
require(rgeos)
require(maptools)
require(ggmap)
require(sp)
require(spdep)
require(ifultools)

# Download historical county map data
setwd(data.directory)
url <- "http://publications.newberry.org/ahcbp/downloads/gis/GA_AtlasHCB.zip"
map.data <- basename(url)
download.file(url,map.data)
unzip(map.data)

# Load map data
setwd(paste0(data.directory,"GA_AtlasHCB/GA_Historical_Counties")) # set directory to map files
county.map <- readOGR(dsn = ".", "GA_Historical_Counties")

# Subset to counties as of 12/31/1807
county.map <- county.map[(county.map$START_N<=18071231 & county.map$END_N >= 18071231),]

# Convert to df
county.f <- fortify(county.map,region="NAME")

# Merge back info
county.f <- merge(county.f,county.map@data, by.x = "id", by.y = "NAME")

# Make county names proper
county.f$id <- properCase(as.character(county.f$id))

# Create dummy for new counties
county.f$newcounty <- ifelse((county.f$id=="Wilkinson" | county.f$id=="Wayne" | county.f$id=="Baldwin"),1,0)

# Subset 1807 counties + 3 new counties
counties1807 <- county.f[county.f$id=="Baldwin" | county.f$id== "Bryan"| county.f$id=="Bulloch"| county.f$id=="Burke"| county.f$id=="Camden"| county.f$id=="Chatham"| county.f$id=="Clarke"| county.f$id=="Columbia"| county.f$id=="Effingham"| county.f$id=="Elbert"| county.f$id=="Franklin"| county.f$id=="Glynn"| county.f$id=="Greene"| county.f$id=="Hancock"| county.f$id=="Jackson"| county.f$id=="Jefferson"| county.f$id=="Liberty"| county.f$id=="Lincoln"| county.f$id=="Mcintosh"| county.f$id=="Montgomery"| county.f$id=="Oglethorpe"| county.f$id=="Richmond"| county.f$id=="Screven"| county.f$id=="Tattnall"| county.f$id=="Warren"| county.f$id=="Washington"| county.f$id=="Wayne" | county.f$id=="Wilkes" | county.f$id=="Wilkinson",]

## 1800 Census 

#merge 1800 census data 
county.00 <- merge(counties1807,counties.00, by.x="id", by.y = "name",all.x=TRUE) # only counties in 1807

# Make labels
cnames <- aggregate(cbind(counties1807$long, counties1807$lat),list(counties1807$id), 
                    FUN=function(x)mean(range(x)))
colnames(cnames) <- c("group","long","lat")
cnames$lat[cnames$group=="Camden"] <- 31 # move up Camden
cnames$lat[cnames$group=="Bryan"] <- 32.01 # move up Bryan
cnames$long[cnames$group=="Chatham"] <- -81.2 # move left Chatham
cnames$group[cnames$group=="Mcintosh"] <- "McIntosh"

if(patient.descriptive){ 
# Produce counties map with 1807 boundaries
Map <- ggplot(counties1807, aes(long, lat, group = group, fill = newcounty)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "white", high = "gray") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
  geom_map(aes(map_id = id, colour = 'black'), map = counties1807) + 
  scale_colour_manual(values=c('black')) + theme(legend.position="none") 

ggsave(paste0(data.directory,"plots/county-map.png"), Map + with(cnames, annotate(geom="text", x = long, y=lat, label = group, size = 3)), width=11, height=8.5)

# Counties map with 1800 slave share
Map.slave.pop <- ggplot(county.00, aes(long, lat, group = group, fill = slave.poppc)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "white", high = "gray") + labs(fill="Slave pop. (% in 1800)") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
  geom_map(aes(map_id = id, colour = 'black'), map = counties1807) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") 

ggsave(paste0(data.directory,"plots/county-map-slave-pop-1800.png"), Map.slave.pop + with(cnames, annotate(geom="text", x = long, y=lat, label = group, size = 3)), width=11, height=8.5)
}

## 1820 Census

# Merge 1820 census data 
county.20 <- merge(counties1807,counties.1820, by.x="id", by.y = "name",all.x=TRUE) # only counties in 1807

if(patient.descriptive){ 
  # Produce slavery population map with 1807 boundaries
  Map.slave.pop.20 <- ggplot(county.20, aes(long, lat, group = group, fill = slave.poppc)) + geom_polygon() + 
    coord_equal()  + scale_fill_gradient(low = "white", high = "gray") + labs(fill="Slave pop. (% in 1820)") + 
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
          axis.text.y = element_blank(),axis.title.x = element_blank(),
          axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                  panel.grid.major=element_blank()) +
    geom_map(aes(map_id = id, colour = 'black'), map = counties1807) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") 
  
  ggsave(paste0(data.directory,"plots/county-map-slave-pop-1820.png"), Map.slave.pop.20 + with(cnames, annotate(geom="text", x = long, y=lat, label = group, size = 3)), width=11, height=8.5)
  
  # Produce wealth gini map with 1807 boundaries
  Map.wealth.gini.20 <- ggplot(county.20, aes(long, lat, group = group, fill = gini)) + geom_polygon() + 
    coord_equal()  + scale_fill_gradient(low = "white", high = "gray") + labs(fill="Slave wealth Gini (1820$)") + 
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
          axis.text.y = element_blank(),axis.title.x = element_blank(),
          axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                  panel.grid.major=element_blank()) +
    geom_map(aes(map_id = id, colour = 'black'), map = counties1807) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") 
  
  ggsave(paste0(data.directory,"plots/county-map-wealth-gini-1820.png"), Map.wealth.gini.20 + with(cnames, annotate(geom="text", x = long, y=lat, label = group, size = 3)), width=11, height=8.5)
  
}

## 1850 Census

# Merge 1850 census data 
county.50 <- merge(counties1807,counties, by.x="id", by.y = "name",all.x=TRUE) # only counties in 1807

if(patient.descriptive){ 
# Produce slavery population map with 1807 boundaries
Map.slave.pop.50 <- ggplot(county.50, aes(long, lat, group = group, fill = slave.poppc)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "white", high = "gray") + labs(fill="Slave pop. (% in 1850)") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
  geom_map(aes(map_id = id, colour = 'black'), map = counties1807) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") 

ggsave(paste0(data.directory,"plots/county-map-slave-pop-1850.png"), Map.slave.pop.50 + with(cnames, annotate(geom="text", x = long, y=lat, label = group, size = 3)), width=11, height=8.5)


# Produce wealth gini map with 1807 boundaries
Map.wealth.gini.50 <- ggplot(county.50, aes(long, lat, group = group, fill = gini)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "white", high = "gray") + labs(fill="Real estate wealth Gini (1850$)") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
  geom_map(aes(map_id = id, colour = 'black'), map = counties1807) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") 

ggsave(paste0(data.directory,"plots/county-map-wealth-gini-1850.png"), Map.wealth.gini.50 + with(cnames, annotate(geom="text", x = long, y=lat, label = group, size = 3)), width=11, height=8.5)
}

## 1870 Census

# Merge 1870 census data 
county.70 <- merge(counties1807,counties.1870, by.x="id", by.y = "county",all.x=TRUE) # only counties in 1807

if(patient.descriptive){ 
  # Produce per capita taxes map with 1807 boundaries
  Map.taxes.pop.70 <- ggplot(county.70, aes(long, lat, group = group, fill = tax.pc)) + geom_polygon() + 
    coord_equal()  + scale_fill_gradient(low = "white", high = "gray") + labs(fill="Per-capita county taxation (1870)") + 
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
          axis.text.y = element_blank(),axis.title.x = element_blank(),
          axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                  panel.grid.major=element_blank()) +
    geom_map(aes(map_id = id, colour = 'black'), map = counties1807) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") 
}

ggsave(paste0(data.directory,"plots/county-map-taxes-pop-1870.png"), Map.taxes.pop.70 + with(cnames, annotate(geom="text", x = long, y=lat, label = group, size = 3)), width=11, height=8.5)