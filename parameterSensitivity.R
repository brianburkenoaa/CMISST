#  This will loop over some parameter values, calculating MAE for each set
#   so we can compare which set works best.

#  The MAE is just calculated on the most recent 5 years, so we know
#    that the length of the predictions is not causing the impact on MAE

library(gridExtra)
library(patchwork)

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
#  Basic Covariance Map
#  Figure 1
#************************************

for (input.stock in c("spCK","faCK","steel")) {
  response<-response.orig[,c("year",input.stock)]
  cmisst <- updateCMISST()
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  season <- switch(input.season, win = 2, spr = 3, sum = 4, aut = 5)
  myTitle <- switch(input.season, win = "Winter", spr = "Spring", sum = "Summer", aut = "Autumn")
  covMap<-cmisst[[season]]
  lmt <- 0.8; limits<-c(-lmt, lmt)
  extent <- cmisst[[6]] # min, max of lat, long
  
  gg <- ggplot() +
    ggtitle(switch(input.stock, spCK="Spring Chinook", faCK="Fall Chinook", steel="Steelhead")) +
    geom_raster(data = melt(covMap), aes(x = Var1, y = Var2, fill=value)) +
    geom_sf(data=land, color="black", fill="grey", linewidth=0.25) +
    xlim(extent[3], extent[4]) + ylim(extent[1], extent[2]) +
    scale_fill_gradientn(colours = myPalette(100),limits=limits,name="Covariance", na.value = "white") +
    theme_classic() + theme(panel.border = element_rect(colour = "grey", fill=NA)) +
    #theme(legend.position = "none") +
    labs(x = "Longitude", y = "Latitude")
  assign(paste('ggCM_', input.stock, sep=''), gg)
}

ggCM <- ggCM_spCK + ggCM_faCK + ggCM_steel + guide_area() +
  plot_layout(ncol = 2, guides = "collect")

ggsave(filename = "manuscriptFigures/covMaps.pdf",
       plot = ggCM, width = 169, units = "mm", dpi = 300)

#************************************
# Covariance Map Time Series
#  NOT USED IN MANUSCRIPT
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

#***************************************************
#*   Data time series and relationship to CMISST
#*    (Figure 2)
#***************************************************
source('CMISSTapp.R')
response_long <- melt(response, id.vars = "year", variable.name = "stock")
response_long$stock <- factor(response_long$stock, levels = c("spCK","faCK","steel"))

g1 <- ggplot(data = response_long[response_long$year>=input.years[1],]) +
  geom_line(aes(x = year, y = value/1000, col=stock), size=1.1) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.2, 0.8)) + ylab("Adult Returns (x1000)") +
  scale_color_discrete(name = "", labels = c("Spring Chinook","Fall Chinook","Steelhead"))

# Make the correlation plots
for (input.stock in c("spCK","faCK","steel")) {
  response<-response.orig[,c("year",input.stock)]
  cmisst <- updateCMISST()
  index<-cmisst[[1]]
  gg <- ggplot(data = index) +
    geom_point(aes(x = spr.cov, y = val), size=1.5, col="blue2") +
    theme_classic() +
    theme(plot.margin = margin(0.5, 0.01, 0.01, 0.01)) +
    xlab(paste(switch(input.stock, spCK="Spring Ch.", faCK="Fall Ch.", steel="Steelhead"), "CMISST")) +
    ylab("Adult Returns\n(Scaled)")
  if (input.stock == "faCK" | input.stock == "steel") gg <- gg +
    theme(axis.title.y = element_text(color = "white"))
  assign(paste("gg_",input.stock,"_cor", sep = ""), gg)
}  

ggFig2 <- g1 + gg_spCK_cor + gg_faCK_cor + gg_steel_cor + 
  plot_layout(design = "AAA\n###\nBCD", heights=c(6,0.5,4))
ggFig2

ggsave(filename = "manuscriptFigures/salmonData.pdf", plot = ggFig2,
       width = 169, height = 100, units = "mm", dpi = 300)

#***************************************************
#*   Lags (Figure 3)
#***************************************************
library(gridExtra)
library(grid)
library(gtable)
library(RColorBrewer)

plot.list<-list()
lags<-0:4
l_n<-length(lags)
# mean absolute covariance of the map
mac <- data.frame(season=as.character(), lag=as.numeric(), mac=as.numeric(), stringsAsFactors = FALSE)
for (input.lag in lags) {
  for (input.season in c("win","spr","sum","aut")) {
    cmisst <- updateCMISST()
    
    # Covariance Map
    myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
    season <- switch(input.season,
                     win = 2,
                     spr = 3,
                     sum = 4,
                     aut = 5)
    covMap<-cmisst[[season]]
    extent <- cmisst[[6]] # min, max of lat, long
    
    gg <- ggplot() #+ ggtitle(myTitle)
    if (input.spatialData == "SSH") gg <- gg + geom_tile(data = melt(covMap),
                                                         aes(x = Var1, y = Var2, fill=value), lwd=0, lty=0)
    if (input.spatialData == "ERSST") gg <- gg + geom_raster(data = melt(covMap),
                                                             aes(x = Var1, y = Var2, fill=value))
    gg <- gg +
      geom_sf(data=land, color="black", fill="grey", linewidth=0.25) +
      xlim(extent[3], extent[4]) + ylim(extent[1], extent[2]) +
      scale_fill_gradientn(colours = myPalette(100), limits=c(-0.75,0.75), name="Covariance", na.value = "white") +
      theme_classic() +
      theme(panel.border = element_rect(colour = "grey", fill=NA),
            axis.text.x=element_blank(), axis.text.y=element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            legend.position = "none",
            plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "inches"))
    plot.list[[length(plot.list)+1]] <- gg
    mac[nrow(mac)+1,] <- data.frame(season=input.season, lag=input.lag, mac=mean(abs(covMap), na.rm = TRUE), stringsAsFactors = FALSE)
  }
}

# grid.arrange(grobs=plot.list, nrow=l_n)
# Complicated code to get row and column headers in grid.arrange
cg <- arrangeGrob(
  textGrob('Winter', gp = gpar(fontsize = 12)),
  textGrob('Spring', gp = gpar(fontsize = 12)),
  textGrob('Summer', gp = gpar(fontsize = 12)),
  textGrob('Autumn', gp = gpar(fontsize = 12)), nrow=1)
rg <- arrangeGrob(textGrob(paste("Lag",lags[1]), gp = gpar(fontsize = 12), rot=90))
for (ll in 2:l_n) { # Add a row in the gtable, then add the Grob
  rg <- gtable_add_rows(rg, unit(1, "null"), pos = -1) # -1 is the bottom
  rg <- gtable_add_grob(rg, textGrob(paste("Lag",lags[ll]), gp = gpar(fontsize = 13, fontface = 'bold'), rot=90), t=ll, l=1)
}
# The headers take up the first row and column in the layout
pl<-c(list(cg), plot.list, list(rg))
lay <- matrix(c(rep(1, 4), seq(2,(l_n*4+1))), ncol=4, byrow=TRUE)
lay<-cbind(matrix(c(NA, rep(max(lay)+1, l_n))), lay)
# grid.arrange(grobs = pl, layout_matrix = lay,
#              widths=c(1,rep(8, 4)), heights=c(1,rep(4, l_n)))

mac$season_fac <- mac$season
for (ii in 1:nrow(mac)) mac$season_fac[ii] <- switch(mac$season[ii], win = "Winter", spr = "Spring", sum = "Summer", aut = "Autumn")
mac$season_fac <- factor(mac$season_fac, levels = c("Winter","Spring","Summer","Autumn"))
lagHist<-ggplot() +
  geom_col(data = mac, aes(x = season_fac, y = mac, fill=factor(lag)), position = "dodge") +
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  scale_fill_brewer() +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  xlab("") + ylab("Mean Absolute Covariance") +
  guides(fill=guide_legend(title="Lag (years)"))
lagHist

pl<-c(list(cg), plot.list, list(rg), list(lagHist))
lay <- matrix(c(rep(1, 4), seq(2,(l_n*4+1))), ncol=4, byrow=TRUE)
lay<-cbind(matrix(c(NA, rep(max(lay)+1, l_n))), lay)
lay<-rbind(lay, rep((max(lay, na.rm = TRUE)+1), 5))
# grid.arrange(grobs = pl, layout_matrix = lay,
#              widths=c(1,rep(8, 4)), heights=c(1,rep(5, l_n),20))

ggsave(filename = "manuscriptFigures/lags.pdf", plot = grid.arrange(grobs = pl, layout_matrix = lay,
                                                                    widths=c(2,rep(8, 4)), heights=c(2,rep(6, l_n), 20)),
       width = 169, units = "mm", dpi = 300)


#************************************
# Evaluate starting year
#  Figure 4
#************************************

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

for (input.stock in c("spCK","faCK","steel")) {
  #startYears <- input.years[1]:(input.years[2]-9)
  startYears <- 1970:(input.years[2]-9)
  maeResults <- data.frame(stock=as.character(), year=as.numeric(), season=as.character(),
                         MAE=as.numeric(), SE=as.numeric(), stringsAsFactors = FALSE)
  response<-response.orig[,c("year",input.stock)]
  
  for (startYear in startYears) {
    input.years[1] <- startYear
    cmisst <- updateCMISST()
    mae<-cmisst[[7]] # LOO results
    covMap <- cmisst[[3]] # spring covariances
    maeResults <- rbind(maeResults, data.frame(stock= input.stock, year=rep(startYear,4), season=mae$season,
                                               MAE=mae$mae.mean, SE=mae$mae.se, stringsAsFactors = FALSE))
    if (startYear %in% c(1970, 1990, 2010)) {
      gg <- ggplot() +
        geom_raster(data = melt(covMap), aes(x = Var1, y = Var2, fill=value)) +
        geom_sf(data=land, color="black", fill="grey", linewidth=0.25) +
        xlim(extent[3], extent[4]) + ylim(extent[1], extent[2]) +
        scale_fill_gradientn(colours = myPalette(100), limits=c(-1,1), name="Covariance", na.value = "white") +
        theme_classic() +
        theme(panel.border = element_rect(colour = "grey", fill=NA),
              axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x = element_blank(), axis.title.y = element_blank(),
              axis.ticks = element_blank(), legend.position = "none",
              plot.margin = unit(c(0,0,0,0), "cm"))
      assign(paste("gg", startYear, sep = ""), gg)
    }
  }
  input.years <- input.years.orig # reset
  
  gg <- ggplot() + ggtitle(switch(input.stock, spCK="Spring Chinook", faCK="Fall Chinook", steel="Steelhead")) +
    geom_ribbon(data=maeResults, aes(x=year, ymin=MAE-SE, ymax=MAE+SE, fill=season), alpha=0.1) +
    geom_line(data=maeResults, aes(x=year, y=MAE, col=season)) +
    ylab("Mean Absolute Error") + xlab("Start Year") +
    theme_classic() +
    scale_fill_discrete(name = "", labels = c("Winter","Spring","Summer","Autumn")) +
    scale_color_discrete(name = "", labels = c("Winter","Spring","Summer","Autumn"))
  if (input.stock == "spCK") gg <- gg +
    theme(axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white"))
  if (input.stock == "faCK") gg <- gg +
    theme(axis.title.x = element_text(color = "white"))
  if (input.stock == "steel") gg <- gg +
    theme(axis.title.y = element_text(color = "white"))
  #gg
  
  gg_inset <- gg +
    inset_element(gg1970, left = 0.05, bottom = 0.65, right = .3, top = 1) +
    inset_element(gg1990, left = 0.375, bottom = 0.65, right = .625, top = 1) +
    inset_element(gg2010, left = 0.7, bottom = 0.65, right = 0.95, top = 1)
  #gg_inset
  
  # Run the above code, then assign the results to one of these 3 before moving on
  if (input.stock=="spCK") gg_inset_sp <- gg_inset
  if (input.stock=="faCK") gg_inset_fa <- gg_inset
  if (input.stock=="steel") gg_inset_st <- gg_inset
  
}

design <- "1\n1\n1\n2\n2\n2\n3\n3\n3\n4"
gg_final <- gg_inset_sp + gg_inset_fa + gg_inset_st + guide_area() +
  plot_layout(ncol = 1, guides = "collect", design = design) &
  theme(legend.direction = 'horizontal')

#legend.position = "bottom", legend.direction = 'vertical')
#+ plot_annotation(tag_levels = "a")

ggsave(filename = "manuscriptFigures/mae.pdf",
       plot = gg_final, width = 105, height = 180, units = "mm", dpi = 300)



#***************************************************
#*   Spatial Extent (Figure 5)
#***************************************************
#************************************
# Evaluate starting lat
#************************************

for (input.stock in c("spCK","faCK","steel")) {
  response<-response.orig[,c("year",input.stock)]
  
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
  
  gg <- ggplot() + theme_classic() +
    ggtitle(input.stock) + theme(plot.title = element_text(color = "white")) +
    geom_ribbon(data=maeResults, aes(x=lat, ymin=MAE-SE, ymax=MAE+SE, fill=season), alpha=0.1) +
    geom_line(data=maeResults, aes(x=lat, y=MAE, col=season)) +
    ylab("Mean Absolute Error") + xlab("Start Latitude")  +
    scale_fill_discrete(name = "Season", labels = c("Winter","Spring","Summer","Autumn")) +
    scale_color_discrete(name = "Season", labels = c("Winter","Spring","Summer","Autumn"))
  if (input.stock == "spCK") gg <- gg +
    theme(axis.title.x = element_text(color = "white"))
  if (input.stock == "faCK") gg <- gg +
    theme(axis.title.y = element_text(color = "white"))
  if (input.stock == "steel") gg <- gg +
    theme(axis.title.x = element_text(color = "white")) +
    theme(axis.title.y = element_text(color = "white"))
  
  assign(paste("gg_",input.stock,"_lat", sep = ""), gg)
}

#************************************
# Evaluate starting long
#************************************

for (input.stock in c("spCK","faCK","steel")) {
  response<-response.orig[,c("year",input.stock)]
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
  
  gg <- ggplot() + theme_classic() +
    ggtitle(input.stock) + theme(plot.title = element_text(color = "white")) +
    geom_ribbon(data=maeResults, aes(x=long, ymin=MAE-SE, ymax=MAE+SE, fill=season), alpha=0.1) +
    geom_line(data=maeResults, aes(x=long, y=MAE, col=season)) +
    ylab("Mean Absolute Error") + xlab("Start Longitude") +
    scale_fill_discrete(name = "Season", labels = c("Winter","Spring","Summer","Autumn")) +
    scale_color_discrete(name = "Season", labels = c("Winter","Spring","Summer","Autumn"))
  if (input.stock == "spCK") gg <- gg +
    theme(axis.title.x = element_text(color = "white"))
  if (input.stock == "faCK") gg <- gg +
    theme(axis.title.y = element_text(color = "white"))
  if (input.stock == "steel") gg <- gg +
    theme(axis.title.x = element_text(color = "white")) +
    theme(axis.title.y = element_text(color = "white"))
  
  assign(paste("gg_",input.stock,"_long", sep = ""), gg)
}

# Run all the lat and long code first
gg_final <- gg_spCK_lat + gg_faCK_lat + gg_steel_lat +
  gg_spCK_long + gg_faCK_long + gg_steel_long +
  plot_layout(ncol = 3, guides = "collect") #+ plot_annotation(tag_levels = "a")

ggsave(filename = "manuscriptFigures/latlong.pdf",
       plot = gg_final, width = 169, height = 85, units = "mm", dpi = 300)




