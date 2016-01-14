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

# Make labels
cnames <- aggregate(cbind(counties1807$long, counties1807$lat),list(counties1807$id), 
                    FUN=function(x)mean(range(x)))
colnames(cnames) <- c("group","long","lat")
cnames$lat[cnames$group=="Camden"] <- 31 # move up Camden
cnames$lat[cnames$group=="Bryan"] <- 32.01 # move up Bryan
cnames$long[cnames$group=="Chatham"] <- -81.2 # move left Chatham

# Produce counties map with 1807 boundaries
Map <- ggplot(counties1807, aes(long, lat, group = group, fill = newcounty)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "white", high = "gray") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
  geom_map(aes(map_id = id, colour = 'black'), map = counties1807) + 
  scale_colour_manual(values=c('black')) + theme(legend.position="none") 

pdf(paste0(data.directory,"county-map.pdf"), width=11, height=8.5)
Map + with(cnames, annotate(geom="text", x = long, y=lat, label = group, size = 3))
dev.off() 

#merge 1850 census data 
county.50 <- merge(counties1807,counties, by.x="id", by.y = "name",all.x=TRUE) # only counties in 1807

# Produce slavery population map with 1807 boundaries
Map.slave.pop.50 <- ggplot(county.50, aes(long, lat, group = group, fill = slave.poppc)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "white", high = "gray") + labs(fill="Slave pop. (% in 1850)") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
  geom_map(aes(map_id = id, colour = 'black'), map = counties1807) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") 

pdf(paste0(data.directory,"county-map-slave-pop-1850.pdf"), width=11, height=8.5)
Map.slave.pop.50 + with(cnames, annotate(geom="text", x = long, y=lat, label = group, size = 3))
dev.off() 