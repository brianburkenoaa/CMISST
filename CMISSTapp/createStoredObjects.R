# Testing

library(reshape2)
library(sf)
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
save("land2", file = "CMISSTapp/data/land.Rdata")

# THIS WAS TO CREATE THE GLOBAL SST DATA (RDATA FILE) FOR SHINY
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
spCK <- fetch_FPC_counts_single_species(my_species="spCK", my_age="adult")
spCK$val <- spCK$BON[[1]]
spCK <- spCK[,c("Year","val")]
colnames(spCK) <- c("year","spCK")
faCK <- fetch_FPC_counts_single_species(my_species="faCK", my_age="adult")
faCK$val <- faCK$BON[[1]]
faCK <- faCK[,c("Year","val")]
colnames(faCK) <- c("year","faCK")
steel <- fetch_FPC_counts_single_species(my_species="steel", my_age="adult")
steel$val <- steel$BON[[1]]
steel <- steel[,c("Year","val")]
colnames(steel) <- c("year","steel")
response <- merge(merge(spCK, faCK), steel)

# lag response
response$year <- response$year - 2
response <- response[response$year %in% c(1981:2020), ]
save(x = "response", file = 'data/responseData.RData')


