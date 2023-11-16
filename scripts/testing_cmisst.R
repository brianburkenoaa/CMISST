# Testing

library(reshape2)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ncdf4)

source('scripts/get_index.R')
source('scripts/dam_counts/run_fetch_FPC_counts_single_species.R')

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
# min.lon=2
# max.lon=358
# min.lat=-88
# max.lat=88

# Regional
# min.lon=220
# max.lon=246
# min.lat=30
# max.lat=55

# Get the lat and long of the 
# ncfname <- "data/../../data/SST/ersst.v5.202001.nc"
# ncin <- nc_open(ncfname)
# lons <- ncvar_get(ncin,"lon")
# lats <- ncvar_get(ncin,"lat",verbose=F)
# nc_close(ncin)


#  NOT IMPLEMENTED IN THE REST OF THE CODE
# THIS WAS TO CREATE THE GLOBAL SST DATA (RDATA FILE) FOR SHINY
# Code to extract the full globe data and save it to a file
# dataSet='ERSST'
# # Full globe
# min.lon=0
# max.lon=360
# min.lat=-90
# max.lat=90
# years=seq(1950, 2022, 1)
# months=seq(1,12,1)
# removeBering=FALSE
# returnDataType='anom'
# returnObjectType='array'
# 
# oceanData<-getOceanData(dataSet=dataSet,
#                         returnDataType=returnDataType, returnObjectType=returnObjectType,
#                         min.lon=min.lon, max.lon=max.lon,
#                         min.lat=min.lat, max.lat=max.lat,
#                         years = years, months = months,
#                         removeBering = removeBering)
# save(x = "oceanData", file = 'data/oceanSSTData.RData')
# load('data/oceanSSTData.RData')


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
load('CMISSTapp/data/land.Rdata')
response <- response[response$year %in% years, ]
# Scale (and log?)
response$val <-log(response$spCK)
response$val <- scale(response$val)

cmisst <- get_CMISST_index(response = response[,c("year","val")], oceanData = oceanData_ERSST,
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
  geom_sf(data=land, color="black", fill="grey", linewidth=0.25) +
  xlim(min.lon, max.lon) + ylim(min.lat, max.lat) +
  scale_fill_gradientn(colours = myPalette(100),limits=limits,name="Covariance", na.value = "white") +
  #scale_fill_gradientn(colours = myPalette(100), name="Covariance", na.value = "white") +
  theme_classic() + theme(panel.border = element_rect(colour = "grey", fill=NA)) +
  labs(x = "Longitude", y = "Latitude")


# LOO CV
source('CMISSTapp/crossValidation.R')

# This first part would be done in the shiny app
year_mo<-data.frame(year=rep(years, each=length(months)), month=rep(months, length(years)),
                    label=paste(rep(years, each=length(months)), rep(months, length(years)), sep = "_"))
load('CMISSTapp/data/oceanSSTData.RData')
oceanData<-oceanData_ERSST
lons <- as.numeric(dimnames(oceanData)[[1]])
lats <- as.numeric(dimnames(oceanData)[[2]])
yr_mo <- dimnames(oceanData)[[3]]
lon.index<-which(lons >= min.lon & lons <= max.lon) 
lat.index<-which(lats >= min.lat & lats <= max.lat)
yr_mo.index<-which(yr_mo %in% year_mo$label)
oceanData <- oceanData[lon.index, lat.index, yr_mo.index]


start_time <- Sys.time()
loocv <- LOO_CV(response = response[,c("year","val")],
                oceanData = oceanData,
                years = years, months = months)
end_time <- Sys.time()
end_time - start_time

