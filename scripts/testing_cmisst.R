# Testing

library(reshape2)
library(sf)
library(ggplot2)
library(RColorBrewer)

source('scripts/get_index.R')

# get the land for plotting (wrap across antimeridian)
land<-rnaturalearth::ne_countries(type='countries', scale = "large", returnclass = "sf")
amer <- land[land$region_un=='Americas',]
# shift the Americas to a more Pacific centric worldview
pacified_amer <- st_shift_longitude(amer)
rest_of_world <- land[!land$region_un=='Americas',]
land2 <- rbind(pacified_amer, rest_of_world)

# set parameters
dataSet='ERSST'
min.lon=158
max.lon=246
min.lat=10
max.lat=62
years=seq(1981, 2020, 1)
months=seq(1,12,1)
removeBering=TRUE
returnDataType='anom'
returnObjectType='array'

response <- data.frame(year=years, val=runif(length(years)))

cmisst <- get_CMISST_index(response = response, dataSet = dataSet)

index <- cmisst[[1]]
plot(index$year, index$spr.cov, type='b', xlab="", ylab="CMISST Index")

# What to plot
covMap<-cmisst[[2]] # wint
covMap<-cmisst[[3]] # spr
covMap<-cmisst[[4]] # sum
covMap<-cmisst[[5]] # aut

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

ggplot() +
  geom_raster(data = melt(covMap),
              aes(x = Var1, y = Var2, fill=value)) +
  geom_sf(data=land2, color="black", fill="grey", linewidth=0.25) +
  xlim(min.lon, max.lon) + ylim(min.lat, max.lat) +
  #scale_fill_gradientn(colours = myPalette(100),limits=c(-0.6,0.5),name="Covariance", na.value = "white") +
  scale_fill_gradientn(colours = myPalette(100), name="Covariance", na.value = "white") +
  theme_classic() + theme(panel.border = element_rect(colour = "grey", fill=NA)) +
  labs(x = "Longitude", y = "Latitude")

