# This application requires several datasets
#  This code creates those datasets and saves them as RData files
#  that can be loaded by the app
#  But it is not part of the app

library(reshape2)
library(sf)
library(ncdf4)

source('scripts/get_index.R') # This is not the same script as the app version
source('scripts/dam_counts/run_fetch_FPC_counts_single_species.R')

# get the land for plotting (wrap across antimeridian)
land<-rnaturalearth::ne_countries(type='countries', scale = "large", returnclass = "sf")
amer <- land[land$region_un=='Americas',]
# shift the Americas to a more Pacific centric worldview
pacified_amer <- st_shift_longitude(amer)
rest_of_world <- land[!land$region_un=='Americas',]
land <- rbind(pacified_amer, rest_of_world)
save("land", file = "CMISSTapp/data/land.Rdata")

# THIS WAS TO CREATE THE GLOBAL SST DATA (RDATA FILE) FOR SHINY
dataSet='ERSST'
# Full globe
min.lon=0
max.lon=360
min.lat=-90
max.lat=90
years=seq(1967, 2022, 1)
months=seq(1,12,1)
removeBering=FALSE
returnDataType='anom'
returnObjectType='array'

oceanData_ERSST<-getOceanData(dataSet=dataSet,
                        returnDataType=returnDataType, returnObjectType=returnObjectType,
                        min.lon=min.lon, max.lon=max.lon,
                        min.lat=min.lat, max.lat=max.lat,
                        years = years, months = months,
                        removeBering = removeBering)
save(x = "oceanData_ERSST", file = 'CMISSTapp/data/oceanSSTData.RData')
load('CMISSTapp/data/oceanSSTData.RData')

# SSH
dataSet='SSH'
# Full globe
# SSH longitude goes from 0.5 to 359.5, by 1
min.lon=0
max.lon=360
# SSH latitude goes from -74 to 65, by 1/3?
min.lat=-90
max.lat=90
years=seq(1980, 2022, 1)
months=seq(1,12,1)
removeBering=FALSE
returnDataType='anom'
returnObjectType='array'

oceanData_SSH<-getOceanData(dataSet=dataSet,
                        returnDataType=returnDataType, returnObjectType=returnObjectType,
                        min.lon=min.lon, max.lon=max.lon,
                        min.lat=min.lat, max.lat=max.lat,
                        years = years, months = months,
                        removeBering = removeBering)
save(x = "oceanData_SSH", file = 'CMISSTapp/data/oceanSSHData.RData')
load('CMISSTapp/data/oceanSSHData.RData')


#  Code to extract dam counts and save to a file
spCK <- fetch_FPC_counts_single_species(my_species="spCK", my_age="adult", Year_start = 1970, Year_end = 2023)
spCK$val <- spCK$BON[[1]]
spCK <- spCK[,c("Year","val")]
colnames(spCK) <- c("year","spCK")
faCK <- fetch_FPC_counts_single_species(my_species="faCK", my_age="adult", Year_start = 1970, Year_end = 2023)
faCK$val <- faCK$BON[[1]]
faCK <- faCK[,c("Year","val")]
colnames(faCK) <- c("year","faCK")
steel <- fetch_FPC_counts_single_species(my_species="steel", my_age="adult", Year_start = 1970, Year_end = 2023)
steel$val <- steel$BON[[1]]
steel <- steel[,c("Year","val")]
colnames(steel) <- c("year","steel")
response <- merge(merge(spCK, faCK), steel)

# lag response (this is now done in the app)
#response$year <- response$year - 2
response <- response[response$year %in% c(1970:2023), ]
save(x = "response", file = 'CMISSTapp/data/responseData.RData')
load(file = 'CMISSTapp/data/responseData.RData')

# Use MCN data as a test for user-defined files
spCK <- fetch_FPC_counts_single_species(my_species="spCK", my_age="adult")
spCK$val <- spCK$MCN[[1]]
response <- spCK[,c("Year","val")]
colnames(response) <- c("year","val")
# lag response
#response$year <- response$year - 2
response <- response[response$year %in% c(1981:2023), ]
write.csv(x = response, file = 'CMISSTapp/data/user_response.csv', row.names = FALSE)

