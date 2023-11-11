library(shiny)

library(reshape2)
library(sf)
library(ggplot2)
library(RColorBrewer)

source('get_index.R')
source('run_fetch_FPC_counts_single_species.R')

# get the land for plotting (wrap across antimeridian)
land<-rnaturalearth::ne_countries(type='countries', scale = "large", returnclass = "sf")
amer <- land[land$region_un=='Americas',]
# shift the Americas to a more Pacific centric worldview
pacified_amer <- st_shift_longitude(amer)
rest_of_world <- land[!land$region_un=='Americas',]
land2 <- rbind(pacified_amer, rest_of_world)

# set parameters
dataSet='ERSST'
min.lon = 158
max.lon = 246
min.lat = 10
max.lat = 62
years = seq(1981, 2020, 1)
months=seq(1,12,1)
removeBering=TRUE
returnDataType='anom'
returnObjectType='array'

load('data/responseData.RData')

# Define UI ----
ui <- fluidPage(
  titlePanel("CMISST"),
  
  sidebarLayout(
    sidebarPanel(h1("Controls"),
                 
                 # Input: Slider for the Latitude range ----
                 sliderInput(inputId = "lat",
                             label = "Latitude Range:",
                             min = -90,
                             max = 90,
                             value = c(10, 62)),
                 
                 # Input: Slider for the Latitude range ----
                 sliderInput(inputId = "long",
                             label = "Longitude Range (0 and 260 are the Prime Meridian):",
                             min = 2,
                             max = 360,
                             step = 2,
                             value = c(158, 246)),
                 
                 # Input: Slider for Years ----
                 sliderInput(inputId = "years",
                             label = "Years:",
                             min = 1960,
                             max = 2020,
                             sep="",
                             value = c(1981, 2020))
                 
                 
    ),
    mainPanel(h3("Covariance Map of Sea surface Temperature"),
              h5("(or other spatial data)"),
              
              # Output: Covariance Map ----
              plotOutput(outputId = "covMap"),
              
              # Output: Covariance Map ----
              plotOutput(outputId = "index")
              )
  )
)

# Define server logic ----
server <- function(input, output) {

  # This runs once when things are changed, then is available for plots to use
  updateCMISST <- reactive({
    min.lon = input$long[1]
    max.lon = input$long[2]
    min.lat = input$lat[1]
    max.lat = input$lat[2]
    years = seq(input$years[1], input$years[2], 1)
    
    response.tmp <- response[response$year %in% years, ]
    response.tmp$val.scl <- scale(response.tmp$val)

    cmisst <- get_CMISST_index(response = response.tmp[,c("year","val.scl")], dataSet = dataSet,
                               min.lon = min.lon, max.lon = max.lon,
                               min.lat = min.lat, max.lat = max.lat,
                               years = years, months = months,
                               returnDataType = returnDataType,
                               removeBering = removeBering)
    return(cmisst)
  })
  
  # Covariance Map
  output$covMap <- renderPlot({
    cmisst <- updateCMISST()
    covMap<-cmisst[[3]] # spr
    myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
    lmt<-max(abs(covMap), na.rm=TRUE)
    limits<-c(-lmt, lmt)
    
    ggplot() +
      geom_raster(data = melt(covMap),
                  aes(x = Var1, y = Var2, fill=value)) +
      geom_sf(data=land2, color="black", fill="grey", linewidth=0.25) +
      xlim(cmisst[[8]], cmisst[[9]]) + ylim(cmisst[[6]], cmisst[[7]]) +
      scale_fill_gradientn(colours = myPalette(100),limits=limits,name="Covariance", na.value = "white") +
      #scale_fill_gradientn(colours = myPalette(100), name="Covariance", na.value = "white") +
      theme_classic() + theme(panel.border = element_rect(colour = "grey", fill=NA)) +
      labs(x = "Longitude", y = "Latitude")
  })
  
  # Index
  output$index <- renderPlot({
    # These have to be in here (depend on them) in order for this to refresh
    # min.lon = input$long[1]
    # max.lon = input$long[2]
    # min.lat = input$lat[1]
    # max.lat = input$lat[2]
    # years = seq(input$years[1], input$years[2], 1)
    # 
    # # cmisst <- updateCMISST()
    # 
    # response.tmp <- response[response$year %in% years, ]
    # # Scale (and log?)
    # response.tmp$val.scl <- scale(response.tmp$val)
    # 
    # # cmisst <- updateCMISST()
    # cmisst <- get_CMISST_index(response = response.tmp[,c("year","val.scl")], dataSet = dataSet,
    #                            min.lon = min.lon, max.lon = max.lon,
    #                            min.lat = min.lat, max.lat = max.lat,
    #                            years = years, months = months,
    #                            returnDataType = returnDataType,
    #                            removeBering = removeBering)
    cmisst <- updateCMISST()
    index <- cmisst[[1]]
    plot(index$year, index$spr.cov, type='b', xlab="", ylab="CMISST Index")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)