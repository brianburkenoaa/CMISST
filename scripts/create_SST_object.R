# Extract data from global data sets
# User sets extent and return object type

# SST data are from ERSST (https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/)
# ERSST is a 2x2 degree gloabal dataset

# references: Huang et al, 2017: Extended Reconstructed Sea Surface Temperatures Version 5 (ERSSTv5): Upgrades, Validations, and Intercomparisons. Journal of Climate, https://doi.org/10.1175/JCLI-D-16-0836.1
# climatology: Climatology is based on 1971-2000 SST, Xue, Y., T. M. Smith, and R. W. Reynolds, 2003: Interdecadal changes of 30-yr SST normals during 1871.2000. Journal of Climate, 16, 1601-1612.
# acknowledgment: The NOAA Extended Reconstructed Sea Surface Temperature (ERSST) data are provided by the NOAA National Centers for Environmental Information(NCEI)

library(ncdf4)
# Do we need the following (move them up once it's confirmed they're required)
# library(RColorBrewer)
# library(sp)
# library(reshape2)

# For testing the function - delete when finished
returnDataType='anom'
returnObjectType='array'
lon.subset=seq(158, 246, 2)
lat.subset=seq(10, 62, 2)
years=seq(1980, 2020, 1)
months=seq(1,12,1)
year_mo<-data.frame(year=rep(years, each=12), month=rep(months, length(years)),
                    label=paste(rep(years, each=12), rep(months, length(years)), sep = "_"))
removeBering=TRUE

#***************************************************************
# Create the function
#***************************************************************
getOceanData<-function(dataSet='ERSST',
                 returnDataType='anom', returnObjectType='array',
                 min.lon=158, max.lon=246,
                 min.lat=10, max.lat=62,
                 years=seq(1980, 2020, 1), months=seq(1,12,1),
                 removeBering=TRUE) {

  # Check conditions
  if (!returnDataType %in% c('anom','raw')) return (cat("returnDataType must be either 'anom' or 'raw'"))
  if (!returnObjectType %in% c('array')) return (cat("returnObjectType must be 'array'"))
  if (min.lon < 0 | min.lon > 360 | max.lon < 0 | max.lon > 360) return (cat("Longitude is in degrees east (Tokyo is 139.8, Seattle is 237.6)"))
  if (min.lat < -90 | min.lat > 90 | max.lat < -90 | max.lat > 90) return (cat("Latitude is in degrees from the equator (should not exceed 90)"))

  year_mo<-data.frame(year=rep(years, each=12), month=rep(months, length(years)),
                      label=paste(rep(years, each=12), rep(months, length(years)), sep = "_"))
  
  #***************************************************************
  # Extract the location data
  #***************************************************************

  # open a (any) netCDF file to extract lats and longs
  if (dataSet == 'ERSST') {
    ncfname <- "data/SST/ersst.v5.202001.nc"
    ncin <- nc_open(ncfname)
    #print(ncin) # metadata
    lons <- ncvar_get(ncin,"lon")
    lats <- ncvar_get(ncin,"lat",verbose=F)
    nc_close(ncin)
  }
  if (dataSet == 'SSH') {
    ncfname <- "data/SSH/sshg.mon.ltm.1991-2020.nc"
    ncin <- nc_open(ncfname) # open it
    #print(ncin) # metadata
    lons <- ncvar_get(ncin,"lon")
    lats <- ncvar_get(ncin,"lat",verbose=F)
    ssh.ltm <- ncvar_get(ncin,"sshg")
    nc_close(ncin)
  }
  
  #***************************************************************
  # Define the grid of interest
  #***************************************************************
  
  # Index the locations in the file
  lon.index<-which(lons > min.lon & lons < max.lon) 
  lat.index<-which(lats > min.lat & lats < max.lat)
  lon.subset <- lons[lon.index]
  lat.subset <- lats[lat.index]

  # Loop over files, extract the anomaly data, and store them in a single array
  returnData<-array(rep(x = NaN, nrow(year_mo) * length(lon.subset) * length(lat.subset)),
                  dim = c(length(lon.subset), length(lat.subset), nrow(year_mo)),
                  dimnames = list(lon.subset, lat.subset, year_mo$label))
  
  if (dataSet == 'ERSST') {
    # Loop over files and get sst data
    for (ym in 1:nrow(year_mo)) {
      ncfname <- paste0("data/SST/ersst.v5.",year_mo$year[ym],sprintf("%02d",year_mo$month[ym]),".nc")
      ncin <- nc_open(ncfname)
      if (returnDataType=="anom") sst <- ncvar_get(ncin,"ssta")
      if (returnDataType=="raw") sst <- ncvar_get(ncin,"sst")
      # Extract and add to the array
      returnData[,,ym]<-sst[lon.index, lat.index]
      nc_close(ncin)
    }
  }
  if (dataSet == 'SSH') {
    # Loop over files and get ssh data
    # Don't open the file 12 times for each year!
    for (ym in 1:nrow(year_mo)) {
      if (year_mo$month[ym]==1) {
        ncfname <- paste0("data/SSH/sshg.",year_mo$year[ym],".nc")
        ncin <- nc_open(ncfname)
        ssh.temp <- ncvar_get(ncin,"sshg")
      }
      # get anomaly
      if (returnDataType == 'anom') {
        ssh.temp[,, year_mo$month[ym]]<-ssh.temp[,, year_mo$month[ym]] - ssh.ltm[,, year_mo$month[ym]]
      }
      # Extract and add to the array
      ssh.anom[,,ym]<-ssh.temp[lon.index, lat.index, year_mo$month[ym]]
      if (year_mo$month[ym]==12) nc_close(ncin)
    }
  }
    
  if (removeBering) {
    # We extracted SST data for the full grid, but we don't want some portions of it
    #  Remove the Bering Sea
    returnData[lon.subset < 206, lat.subset > 56,] <- NA
    returnData[lon.subset < 202, lat.subset > 54,] <- NA
    returnData[lon.subset < 196, lat.subset > 52,] <- NA
    returnData[lon.subset < 158, lat.subset > 50,] <- NA
    returnData[lon.subset < 156, lat.subset > 48,] <- NA
    returnData[lon.subset < 154, lat.subset > 46,] <- NA
    returnData[lon.subset < 152, lat.subset > 44,] <- NA
    returnData[lon.subset < 148, lat.subset > 42,] <- NA
    returnData[lon.subset < 146, lat.subset > 40,] <- NA
  }
  
  return(returnData)
}






#***************************************************************
# Create the function for SSH
# Data from: https://psl.noaa.gov/data/gridded/data.godas.html
#***************************************************************
getSSH<-function(returnDataType='anom', returnObjectType='array',
                 min.lon=158, max.lon=246,
                 min.lat=10, max.lat=62,
                 years=seq(1980, 2020, 1), months=seq(1,12,1),
                 removeBering=TRUE) {
  
  # Check conditions
  if (!returnDataType %in% c('anom','raw')) return (cat("returnDataType must be either 'anom' or 'raw'"))
  if (!returnObjectType %in% c('array')) return (cat("returnObjectType must be 'array'"))
  if (min.lon < 0 | min.lon > 360 | max.lon < 0 | max.lon > 360) return (cat("Longitude is in degrees east (Tokyo is 139.8, Seattle is 237.6)"))
  if (min.lat < -90 | min.lat > 90 | max.lat < -90 | max.lat > 90) return (cat("Latitude is in degrees from the equator (should not exceed 90)"))
  
  year_mo<-data.frame(year=rep(years, each=12), month=rep(months, length(years)),
                      label=paste(rep(years, each=12), rep(months, length(years)), sep = "_"))
  
  #***************************************************************
  # Extract the data
  #***************************************************************
  
  # open a (any) netCDF file to extract lats and longs
  ncfname <- "data/indicators/covMaps/ssh/sshg.mon.ltm.1991-2020.nc"
  ncin <- nc_open(ncfname) # open it
  #print(ncin) # metadata
  lons <- ncvar_get(ncin,"lon")
  lats <- ncvar_get(ncin,"lat",verbose=F)
  ssh.ltm <- ncvar_get(ncin,"sshg")
  nc_close(ncin)
  
  #***************************************************************
  # Define the grid of interest
  #***************************************************************

  lon.index<-which(lons > min.lon & lons < max.lon) 
  lat.index<-which(lats > min.lat & lats < max.lat)
  lon.subset <- lons[lon.index]
  lat.subset <- lats[lat.index]
  
  # Loop over files, extract the anomaly data, and store them in a single array
  ssh.anom<-array(rep(x = NaN, nrow(year_mo) * length(lon.index) * length(lat.index)),
                  dim = c(length(lon.index), length(lat.index), nrow(year_mo)),
                  dimnames = list(lon.subset, lat.subset, year_mo$label))
  # Loop over files and get ssh data
  # Don't open the file 12 times for each year!
  for (ym in 1:nrow(year_mo)) {
    if (year_mo$month[ym]==1) {
      ncfname <- paste0("data/SSH/sshg.",year_mo$year[ym],".nc")
      ncin <- nc_open(ncfname)
      ssh.temp <- ncvar_get(ncin,"sshg")
    }
    # get anomaly
    if (returnDataType == 'anom') {
      ssh.temp[,, year_mo$month[ym]]<-ssh.temp[,, year_mo$month[ym]] - ssh.ltm[,, year_mo$month[ym]]
    }
    # Extract and add to the array
    ssh.anom[,,ym]<-ssh.temp[lon.index, lat.index, year_mo$month[ym]]
    if (year_mo$month[ym]==12) nc_close(ncin)
  }
  
  if (removeBering) {
    # We extracted SST data for the full grid, but we don't want some portions of it
    #  Remove the Bering Sea
    ssh.anom[lon.subset < 206, lat.subset > 56,] <- NA
    ssh.anom[lon.subset < 202, lat.subset > 54,] <- NA
    ssh.anom[lon.subset < 196, lat.subset > 52,] <- NA
    ssh.anom[lon.subset < 158, lat.subset > 50,] <- NA
    ssh.anom[lon.subset < 156, lat.subset > 48,] <- NA
    ssh.anom[lon.subset < 154, lat.subset > 46,] <- NA
    ssh.anom[lon.subset < 152, lat.subset > 44,] <- NA
    ssh.anom[lon.subset < 148, lat.subset > 42,] <- NA
    ssh.anom[lon.subset < 146, lat.subset > 40,] <- NA
  }
  
  return(ssh.anom)
}