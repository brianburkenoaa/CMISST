# Testing

library(reshape2)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ncdf4)

source('scripts/get_index.R')
source('scripts/dam_counts/run_fetch_FPC_counts_single_species.R')

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

# Full globe
min.lon=2
max.lon=358
min.lat=-88
max.lat=88

# Regional
# min.lon=220
# max.lon=246
# min.lat=30
# max.lat=55

# Get the lat and long of the 
ncfname <- "data/../../data/SST/ersst.v5.202001.nc"
ncin <- nc_open(ncfname)
lons <- ncvar_get(ncin,"lon")
lats <- ncvar_get(ncin,"lat",verbose=F)
nc_close(ncin)


#  NOT IMPLEMENTED IN THE REST OF THE CODE
# Code to extract the full globe data and save it to a file
dataSet='ERSST'
# Full globe
min.lon=0
max.lon=360
min.lat=-90
max.lat=90
years=seq(1950, 2022, 1)
months=seq(1,12,1)
removeBering=FALSE
returnDataType='anom'
returnObjectType='array'

oceanData<-getOceanData(dataSet=dataSet,
                        returnDataType=returnDataType, returnObjectType=returnObjectType,
                        min.lon=min.lon, max.lon=max.lon,
                        min.lat=min.lat, max.lat=max.lat,
                        years = years, months = months,
                        removeBering = removeBering)
save(x = "oceanData", file = 'data/oceanSSTData.RData')
load('data/oceanSSTData.RData')


#  Code to extract dam counts and save to a file
# response <- fetch_FPC_counts_single_species(my_species="spCK", my_age="adult")
# response$val <- response$BON[[1]]
# response <- response[,c("Year","val")]
# colnames(response) <- c("year","val")
# # lag response
# response$year <- response$year - 2
# response <- response[response$year %in% years, ]
# save(x = "response", file = 'data/responseData.RData')

load('data/responseData.RData')
response <- response[response$year %in% years, ]
# Scale (and log?)
response$val.scl <- scale(response$val)

cmisst <- get_CMISST_index(response = response[,c("year","val.scl")], dataSet = dataSet,
                           min.lon = min.lon, max.lon = max.lon,
                           min.lat = min.lat, max.lat = max.lat,
                           years = years, months = months,
                           returnDataType = returnDataType,
                           removeBering = removeBering)

index <- cmisst[[1]]
plot(index$year, index$spr.cov, type='b', xlab="", ylab="CMISST Index")
#  Used to plot lines from multiple extents - you have to get the index multiple times
# points(index$year, index$spr.cov, type='b', col="blue")
# points(index$year, index$spr.cov, type='b', col="red")

# What to plot
#covMap<-cmisst[[2]] # wint
covMap<-cmisst[[3]] # spr
#covMap<-cmisst[[4]] # sum
#covMap<-cmisst[[5]] # aut

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
lmt<-max(abs(covMap), na.rm=TRUE)
limits<-c(-lmt, lmt)

ggplot() +
  geom_raster(data = melt(covMap),
              aes(x = Var1, y = Var2, fill=value)) +
  geom_sf(data=land2, color="black", fill="grey", linewidth=0.25) +
  xlim(min.lon, max.lon) + ylim(min.lat, max.lat) +
  scale_fill_gradientn(colours = myPalette(100),limits=limits,name="Covariance", na.value = "white") +
  #scale_fill_gradientn(colours = myPalette(100), name="Covariance", na.value = "white") +
  theme_classic() + theme(panel.border = element_rect(colour = "grey", fill=NA)) +
  labs(x = "Longitude", y = "Latitude")

