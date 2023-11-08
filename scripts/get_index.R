# This will create the CMISST index, given a response variable time series
#  and a user-specified spatial ocean variable

source("scripts/create_OceanData_Object.R")

get_CMISST_index <- function(response, dataSet='ERSST',
                          lon.subset=seq(158, 246, 2),
                          lat.subset=seq(10, 62, 2),
                          returnDataType='anom',
                          returnObjectType='array',
                          removeBering=TRUE) {
  # Verify that the response is what we expect
  if (ncol(response)!=2) { print("incorrect data - requires a 2-column data frame with year and the response"); return(NA) }
  colnames(response)<-c("year","val")
  
  years=response$year
  months=1:12
  year_mo<-data.frame(year=rep(years, each=12), month=rep(months, length(years)),
                      label=paste(rep(years, each=12), rep(months, length(years)), sep = "_"))
  
  #***************************************************************
  # Extract and scale the SST data
  #***************************************************************
  
  source("scripts/create_OceanData_Object.R")
  oceanData<-getOceanData(dataSet=dataSet,
                         returnDataType=returnDataType, returnObjectType=returnObjectType,
                         min.lon=158, max.lon=246,
                         min.lat=10, max.lat=62,
                         years = years, months = months,
                         removeBering = removeBering)
 
  createSeasonalData<-function(oceanData,
                               years = years, months = months, year_mo=year_mo, season=1) {
    seasonal<-array(NA, c(dim(oceanData)[1], dim(oceanData)[2], length(years)), dimnames = list(dimnames(oceanData)[[1]], dimnames(oceanData)[[2]], years))
    for (yy in 1:length(years)) {
      if (season==1) seasonal[,,yy]<-(oceanData[,,year_mo$month == 1 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 2 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 3 & year_mo$year==years[yy]])/3
      if (season==2) seasonal[,,yy]<-(oceanData[,,year_mo$month == 4 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 5 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 6 & year_mo$year==years[yy]])/3
      if (season==3) seasonal[,,yy]<-(oceanData[,,year_mo$month == 7 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 8 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 9 & year_mo$year==years[yy]])/3
      if (season==4) seasonal[,,yy]<-(oceanData[,,year_mo$month == 10 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 11 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 11 & year_mo$year==years[yy]])/3
    } 
    # This scales (Z-score) the data cell-wise
    # The aperm is needed because for some reason the apply function returns the third dimension (time) as the first dimension
    oceanData.scl <- aperm(apply(seasonal, 1:2, scale), c(2,3,1))
    dimnames(oceanData.scl)[[3]]<-years
    return(oceanData.scl)
  }
  
  # Create the data by calling our function (returns scaled sst array and full dataset with fish)
  oceanData.s1.scl <- createSeasonalData(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 1)
  oceanData.s2.scl <- createSeasonalData(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 2)
  oceanData.s3.scl <- createSeasonalData(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 3)
  oceanData.s4.scl <- createSeasonalData(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 4)

  # Get covariance between each cell's temperature and survival
  covs1<-apply(oceanData.s1.scl, 1:2, function(x) cov(x, response$val, use="pairwise.complete.obs"))
  covs2<-apply(oceanData.s2.scl, 1:2, function(x) cov(x, response$val, use="pairwise.complete.obs"))
  covs3<-apply(oceanData.s3.scl, 1:2, function(x) cov(x, response$val, use="pairwise.complete.obs"))
  covs4<-apply(oceanData.s4.scl, 1:2, function(x) cov(x, response$val, use="pairwise.complete.obs"))

  #********************************************************************
  # Create the index (how similar is each year to the covariance map)
  # For the PDO, I think they regress each year onto the pdo pattern
  # But are there other methods of comparing similarity of matrices that we should think about?
  #********************************************************************
  coefs_cov<-NULL
  options(na.action="na.omit")
  for (tt in 1:dim(oceanData.s1.scl)[3])
    coefs_cov<-rbind(coefs_cov, c(lm(as.vector(oceanData.s1.scl[,,tt]) ~ -1 + as.vector(covs1))$coef,
                                  lm(as.vector(oceanData.s1.scl[,,tt]) ~ -1 + as.vector(covs2))$coef,
                                  lm(as.vector(oceanData.s1.scl[,,tt]) ~ -1 + as.vector(covs3))$coef,
                                  lm(as.vector(oceanData.s1.scl[,,tt]) ~ -1 + as.vector(covs4))$coef))
  coefs_cov<-data.frame(coefs_cov)
  coefs_cov$year<-years
  index_cov<-cbind(coefs_cov,response$val)
  colnames(index_cov)<-c("win.cov","spr.cov","sum.cov","aut.cov","year","val")

  return(list(index_cov, covs1, covs2, covs3, covs4))
}
