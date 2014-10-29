library(plyr)
library(ggplot2)
library(RColorBrewer))
library(gdata)
library(gtools) 
library(RgoogleMaps)
library(ggmap)  

# ==================================
# = Import file for Global Maps    =
# ==================================   
setwd("/Users/emilydarling/Dropbox/1-On the go/IN REVIEW/FoodSecurity/Data")          
d <- read.csv("Study Sites GPS.csv", header = TRUE, stringsAsFactors = TRUE) 
head(d)
nrow(d)
names(d)      

geocode("Kenya")  

map <- get_map(location=c(lon = 39.35, lat = -4), zoom=9, maptype="satellite", source="google", crop = FALSE)         
p <- ggmap(map)
p       

#Add Kenya Marine National Park shape files		
setwd("/Users/emilydarling/Dropbox/1-On the go/IN REVIEW/FoodSecurity/R_FoodSecurity/ShapeFiles_Kenya MNPs")  		     	
library(rgdal)              
MNP <- readOGR(".","esdarling-search-kenya-marine-national-1413907655365")
MNP <- spTransform(MNP, CRS("+proj=longlat +datum=WGS84"))   
MNP <- fortify(MNP) 
head(MNP)
str(MNP)  
nrow(MNP)

MNP_check <- ddply(MNP, c("id"), function(x) data.frame(min_lat = min(x$lat))) 
MNP_check[rev(order(MNP_check$min_lat)),]     

#Use a subset of MNPs that relate to study
#Removed Diani MNP which is a paper park if that
MNP2 <- subset(MNP, id != 4 & id != 2 & id != 5)
nrow(MNP2)

p + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2, color='green', data=MNP2, alpha=0)  

#Add points for study sites
#levels(d$type)
last_plot() + geom_point(aes(x = longitude, y = latitude, colour = type), alpha = 0.65, size = 6, data = d) + 
	scale_colour_manual(values = c("yellow", "red", "orange")) 
	
last_plot() + theme_classic(base_size = 14) + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank(), 
		plot.margin = unit(c(0.5,0.5,0.5,0.5),"in"))     

p2 <- last_plot()
		
#This code provides the base Kenya study sites map which I then further edit in Keynote (I know, I know..)		
#Save plot to drag into Keynote
setwd("/Users/emilydarling/Dropbox/1-On the go/IN REVIEW/FoodSecurity/Figures") 
pdf("Kenya map from R.pdf", width = 9, height = 9)
p2
dev.off()  		

##This code provides the larger Africa map with Kenya highlighted
# ========================
# = Kenya map for inset  =
# ========================
geocode("Kenya")  

#Load Country polygons
setwd("/Users/emilydarling/Dropbox/1-On the go/IN REVIEW/FoodSecurity/R_FoodSecurity/ShapeFiles_countries")  		     	
library(rgdal)              

  world = readOGR(dsn=".", layer="ne_110m_admin_0_countries")
  world@data$id = rownames(world@data)
  world.points = fortify(world)
  world.df = join(world.points, world@data, by="id")               
head(world.df)  
names(world.df)  
str(world.df) 
levels(world.df$name)  
Kenya.df <- subset(world.df, name == "Kenya")

map2 <- get_map(location=c(lon = 35, lat = -10), zoom=4, maptype="satellite", source="google", crop = FALSE)         
q <- ggmap(map2)

kenya2 <- q + geom_polygon(aes(x = long, y = lat, group = group), data = kenya.df, 
	fill = "grey", colour = "white", alpha = .6, size = .3) 
kenya2  
   	
#Save plot to drag into Keynote
setwd("/Users/emilydarling/Dropbox/1-On the go/IN REVIEW/FoodSecurity/Figures") 
pdf("EAfrica map from R.pdf", width = 5, height = 5)
kenya2
dev.off()  	  
               
              
