# This is not the shiny app.  It's a sort of copy that can be
#  used to debug

library(reshape2)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(doBy)

source('CMISSTapp/R/get_index.R')
source('CMISSTapp/R/crossValidation.R')

# The loadData.R script uses a relative path, which is inside the app folder
#  I'm just going to load the data individually here
#source('CMISSTapp/R/loadData.R')
load("CMISSTapp/data/land.Rdata")
load('CMISSTapp/data/responseData.RData')
load('CMISSTapp/data/oceanSSTData.RData')
load('CMISSTapp/data/oceanSSHData.RData')


# set parameters
dataSet='ERSST'
months=seq(1,12,1)
removeBering=TRUE
returnDataType='anom'
returnObjectType='array'
loocvYears=5 # the most recent X years to include in the LOO CV


#************************************
#       Define UI ----
#************************************
input.spatialData = "ERSST"
#input.spatialData = "SSH"

# Input: Choose a stock ----
input.stock = "spCK"
#input.stock = "faCK"
#input.stock = "steel"

# Input: Choose a file ----
input.datafile=NULL

# Input: log response? ----
input.log = TRUE

# Input: lag response? ----
input.lag= 2

# Input: Slider for the Latitude range ----
input.lat = c(10, 62)

# Input: Slider for the Latitude range ----
input.long= c(158, 246)

# Input: Slider for Ocean Years ----
input.years= c(1980, 2021)
input.years.pred=c(2021,2022)
input.years.pred=NA

# Input: MAE LOO CV? ----
input.loocv= FALSE
#input.loocv= TRUE

# Input: What map to plot
input.season = "spr"

# Not in the app, this is for loocv for the manuscript
includePDO = FALSE
#includePDO = TRUE

#************************************
# Define server logic ----
#************************************
# This runs once when things are changed, then is available for plots to use
updateCMISST <- function() {
  min.lon = input.long[1]
  max.lon = input.long[2]
  min.lat = input.lat[1]
  max.lat = input.lat[2]
  years = seq(input.years[1], input.years[2], 1)
  years = sort(unique(c(years, input.years.pred)))
  
  response.tmp <- response
  response.tmp$year <- response.tmp$year - as.numeric(input.lag)
  response.tmp <- response.tmp[response.tmp$year %in% years, c('year', input.stock)]

  # We changed the response years, so make sure years doesn't go beyond the new range
  #years <- years[years %in% response.tmp$year]

  colnames(response.tmp) <- c('year','val')
  if(input.log) response.tmp$val <- log(response.tmp$val)
  response.tmp$val.scl <- scale(response.tmp$val)

  if (input.spatialData == "ERSST") oceanData <- oceanData_ERSST
  if (input.spatialData == "SSH") oceanData <- oceanData_SSH

  cmisst <- get_CMISST_index(response = response.tmp[,c("year","val.scl")],
                             oceanData = oceanData, years.pred = input.years.pred,
                             min.lon = min.lon, max.lon = max.lon,
                             min.lat = min.lat, max.lat = max.lat,
                             years = years, months = months,
                             returnDataType = returnDataType,
                             removeBering = removeBering)

  if (input.loocv) {
    loocv <- LOO_CV(response = response.tmp[,c("year","val.scl")],
                                   oceanData = oceanData, loocvYears = loocvYears,
                                   min.lon = min.lon, max.lon = max.lon,
                                   min.lat = min.lat, max.lat = max.lat,
                                   years = years, months = months)
    return(append(cmisst, loocv))
  } else return(cmisst)
  # Returns index (6 columns, but one list item: 4 seasonal indices, year, val),
  #         cov values (for maps, 4 seasons, each a list item), so the list is length 5 now
  #         lat, long min and max, 1 list item, that's 6 so far
  #         loocv, if requested, that's 7
}

# Call the function - this would happen in the app any time a parameter is changed
cmisst <- updateCMISST()

# Covariance Map
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
season <- switch(input.season,
               win = 2,
               spr = 3,
               sum = 4,
               aut = 5)
myTitle <- switch(input.season,
                 win = "Winter",
                 spr = "Spring",
                 sum = "Summer",
                 aut = "Autumn")
covMap<-cmisst[[season]]
lmt<-max(abs(covMap), na.rm=TRUE)
limits<-c(-lmt, lmt)
extent <- cmisst[[6]] # min, max of lat, long

gg <- ggplot() + ggtitle(myTitle)
  # geom_raster is faster, but wants all cells to be the same size, which it seems tehy are not for the SSH data??
  if (input.spatialData == "SSH") gg <- gg + geom_tile(data = melt(covMap),
            aes(x = Var1, y = Var2, fill=value), lwd=0, lty=0)
  if (input.spatialData == "ERSST") gg <- gg + geom_raster(data = melt(covMap),
            aes(x = Var1, y = Var2, fill=value))
gg <- gg +
  geom_sf(data=land, color="black", fill="grey", linewidth=0.25) +
  xlim(extent[3], extent[4]) + ylim(extent[1], extent[2]) +
  scale_fill_gradientn(colours = myPalette(100),limits=limits,name="Covariance", na.value = "white") +
  #scale_fill_gradientn(colours = myPalette(100), name="Covariance", na.value = "white") +
  theme_classic() + theme(panel.border = element_rect(colour = "grey", fill=NA)) +
  labs(x = "Longitude", y = "Latitude")
gg


# Biplot with response
index <- cmisst[[1]]
season <- switch(input.season,
                 win = 1,
                 spr = 2,
                 sum = 3,
                 aut = 4)
index$ind <- index[,season]
myTitle <- switch(input.season,
                  win = "Winter",
                  spr = "Spring",
                  sum = "Summer",
                  aut = "Autumn")
plot(index$ind, index$val, pch=20, cex=2, xlab=paste(myTitle, "CMISST Index"),
     ylab="Scaled (Z-score) Response", main=myTitle)
lm1 <- lm(index$val~index$ind)
abline(lm1)
text(bquote(~ R^2 == .(round(summary(lm1)$adj.r.squared, 2))),
     x = par("usr")[1]*0.8, y=par("usr")[4]*0.80, cex=1.6, col="blue")
if (input.loocv) {
  mae <- cmisst[[7]]
  text(paste("MAE =", round(mae[season,3], 2)),
     x = par("usr")[1]*0.75, y=par("usr")[4]*0.60, cex=1.6, col="blue")
}
# cmisst[[7]][cmisst[[7]]$season=="spr",]

# Output: Index time series
index <- cmisst[[1]]
plot(index$year, index$win.cov, type='b', pch=20, col="red4",
     xlab="", ylab="CMISST Index",
     ylim=c(min(index[,c("win.cov","spr.cov","sum.cov","aut.cov")], na.rm=TRUE),
            max(index[,c("win.cov","spr.cov","sum.cov","aut.cov")], na.rm=TRUE)))
points(index$year, index$spr.cov, type='b', pch=20, col="blue")
points(index$year, index$sum.cov, type='b', pch=20, col="green3")
points(index$year, index$aut.cov, type='b', pch=20, col="purple")
legend("topleft", legend = c("Win","Spr","Sum","Aut"), bty='n',
       col = c("red4","blue","green3","purple"), pch = 20, lty=1)

  
# Output: Table
out<-cmisst[[1]]
out$year <- as.integer(out$year)
#out <- out[,c(5,6,1:4)]
colnames(out)[colnames(out)=="val"] <- "response"
cbind(out)
