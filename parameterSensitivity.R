#  This will loop over some parameter values, calculating MAE for each set
#   so we can compare which set works best.

#  The MAE is just calculated on the most recent 5 years, so we know
#    that the length of the predictions is not causing the impact on MAE

library(gridExtra)

# this sets up all the parameters and functions
#   and gets the index for the default parameters
#   (but does not run LOO CV)
#  It's also great for resetting all variables
source('CMISSTapp.R')

#************************************
# Set up parameters to loop over
#************************************

input.loocv <- TRUE

# Store original values, jic
input.years.orig<-input.years
input.lat.orig<-input.lat
input.long.orig<-input.long
response.orig <- response

input.stock<-"spCK"
input.stock<-"faCK"
input.stock<-"steel"
response<-response.orig[,c("year",input.stock)]

loocvYears=5 # the most recent X years to include in the LOO CV

#************************************
# Evaluate starting year
#************************************

startYears <- input.years[1]:(input.years[2]-9)
maeResults <- data.frame(stock=as.character(), year=as.numeric(), season=as.character(),
                         MAE=as.numeric(), SE=as.numeric(), stringsAsFactors = FALSE)

for (startYear in startYears) {
  input.years[1] <- startYear
  cmisst <- updateCMISST()
  mae<-cmisst[[7]]
  maeResults <- rbind(maeResults, data.frame(stock= input.stock, year=rep(startYear,4), season=mae$season,
                                             MAE=mae$mae.mean, SE=mae$mae.se, stringsAsFactors = FALSE))
}
input.years <- input.years.orig # reset

ggplot() + ggtitle(input.stock) +
  geom_ribbon(data=maeResults, aes(x=year, ymin=MAE-SE, ymax=MAE+SE, fill=season), alpha=0.1) +
  geom_line(data=maeResults, aes(x=year, y=MAE, col=season)) +
  ylab("Mean Absolute Error for the last 5 years") + xlab("Start Year") +
  theme_classic()


#************************************
# Evaluate starting lat
#************************************

startLats<- seq(-40, 50, 10)
maeResults <- data.frame(stock=as.character(), lat=as.numeric(), season=as.character(),
                         MAE=as.numeric(), SE=as.numeric(), stringsAsFactors = FALSE)

for (startLat in startLats) {
  input.lat[1] <- startLat
  cmisst <- updateCMISST()
  mae<-cmisst[[7]]
  maeResults <- rbind(maeResults, data.frame(stock= input.stock, lat=rep(startLat,4), season=mae$season,
                                             MAE=mae$mae.mean, SE=mae$mae.se, stringsAsFactors = FALSE))
}
input.lat <- input.lat.orig # reset

ggplot() + ggtitle(input.stock) +
  geom_ribbon(data=maeResults, aes(x=lat, ymin=MAE-SE, ymax=MAE+SE, fill=season), alpha=0.1) +
  geom_line(data=maeResults, aes(x=lat, y=MAE, col=season)) +
  ylab("Mean Absolute Error for the last 5 years") + xlab("Start Latitude") +
  theme_classic()


#************************************
# Evaluate starting long
#************************************

startLongs<- seq(100, 220, 10)
maeResults <- data.frame(stock=as.character(), long=as.numeric(), season=as.character(),
                         MAE=as.numeric(), SE=as.numeric(), stringsAsFactors = FALSE)

for (startLong in startLongs) {
  input.long[1] <- startLong
  cmisst <- updateCMISST()
  mae<-cmisst[[7]]
  maeResults <- rbind(maeResults, data.frame(stock= input.stock, long=rep(startLong,4), season=mae$season,
                                             MAE=mae$mae.mean, SE=mae$mae.se, stringsAsFactors = FALSE))
}
input.long <- input.long.orig # reset

ggplot() + ggtitle(input.stock) +
  geom_ribbon(data=maeResults, aes(x=long, ymin=MAE-SE, ymax=MAE+SE, fill=season), alpha=0.1) +
  geom_line(data=maeResults, aes(x=long, y=MAE, col=season)) +
  ylab("Mean Absolute Error for the last 5 years") + xlab("Start Longitude") +
  theme_classic()





#************************************
# Covariance Map
#************************************
startYears <- input.years[1]:(input.years[2]-9)
for (startYear in startYears) {
  input.years[1] <- startYear
  cmisst <- updateCMISST()
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
  # Optionally:
  # if (input.stock=="spCK") lmt<-0.9; limits<-c(-lmt, lmt)
  # if (input.stock=="steel") lmt<-1.0; limits<-c(-lmt, lmt)
  lmt<-1.0; limits<-c(-lmt, lmt)
  extent <- cmisst[[6]] # min, max of lat, long
  
  gg <- ggplot() +
    #ggtitle(paste(myTitle, startYear))
    ggtitle(startYear)
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
    theme(legend.position = "none") +
    #labs(x = "Longitude", y = "Latitude")
    labs(x = "", y = "")
    assign(paste('ggPlot', startYear, sep=''), gg)
}
input.years <- input.years.orig # reset

margin = theme(plot.margin = unit(c(0,0,0,0), "cm"))
grid.arrange(grobs = lapply(list(ggPlot1970, ggPlot1973, ggPlot1976, ggPlot1979,
                                 ggPlot1982, ggPlot1985, ggPlot1988, ggPlot1991,
                                 ggPlot1994, ggPlot1997, ggPlot2000, ggPlot2003,
                                 ggPlot2006, ggPlot2009, ggPlot2012), "+", margin), nrow = 4)

