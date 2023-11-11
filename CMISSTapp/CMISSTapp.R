library(shiny)

library(reshape2)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(rnaturalearth)

source('get_index.R')
source('run_fetch_FPC_counts_single_species.R')

# get the land for plotting (wrap across antimeridian)
land<-ne_countries(type='countries', scale = "large", returnclass = "sf")
amer <- land[land$region_un=='Americas',]
# shift the Americas to a more Pacific centric worldview
pacified_amer <- st_shift_longitude(amer)
rest_of_world <- land[!land$region_un=='Americas',]
land2 <- rbind(pacified_amer, rest_of_world)

# set parameters
dataSet='ERSST'
# min.lon = 158
# max.lon = 246
# min.lat = 10
# max.lat = 62
# years = seq(1981, 2020, 1)
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
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 #HTML('<hr style="color: purple;">'),
                 
                 # Input: log response? ----
                 checkboxInput(inputId = "log",
                             label = "Log response?",
                             value = TRUE),

                 hr(style = "border-top: 1px solid #000000;"),
                 # Input: Slider for the Latitude range ----
                 sliderInput(inputId = "lat",
                             label = "Latitude Range:",
                             min = -88,
                             max = 88,
                             value = c(10, 62)),
                 
                 # Input: Slider for the Latitude range ----
                 sliderInput(inputId = "long",
                             label = "Longitude Range (0 and 360 are the Prime Meridian):",
                             min = 0,
                             max = 358,
                             step = 2,
                             value = c(158, 246)),
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 # Input: Slider for Years ----
                 sliderInput(inputId = "years",
                             label = "Years:",
                             min = 1960,
                             max = 2020,
                             sep="",
                             value = c(1981, 2020)),
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 # Input: What map to plot
                 radioButtons("season", "Which season to plot:",
                              c("Winter" = "win",
                                "Spring" = "spr",
                                "Summer" = "sum",
                                "Autumn" = "aut"),
                              selected = "spr"),
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 # Input: Reset button ----
                 actionButton(inputId = "reset",
                              label = "Reset all"),
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 # Download the results to a file
                 downloadButton('download',"Download the Results")

    ),
    mainPanel(h3("Covariance Map of Sea surface Temperature"),
              h5("(or other spatial data)"),
              
              # Output: Covariance Map ----
              plotOutput(outputId = "covMap"),
              
              # Output: Time series plot ----
              plotOutput(outputId = "index"),
              
              # Output: biplot of index and response ----
              plotOutput(outputId = "biplot"),
              
              # Output: table ----
              tableOutput(outputId = "table")
    
              )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  # This runs once when things are changed, then is available for plots to use
  updateCMISST <- reactive({
    min.lon = input$long[1]
    max.lon = input$long[2]
    min.lat = input$lat[1]
    max.lat = input$lat[2]
    years = seq(input$years[1], input$years[2], 1)
    
    response.tmp <- response[response$year %in% years, ]
    if(input$log) response.tmp$val <- log(response.tmp$val)
    response.tmp$val.scl <- scale(response.tmp$val)

    cmisst <- get_CMISST_index(response = response.tmp[,c("year","val.scl")], dataSet = dataSet,
                               min.lon = min.lon, max.lon = max.lon,
                               min.lat = min.lat, max.lat = max.lat,
                               years = years, months = months,
                               returnDataType = returnDataType,
                               removeBering = removeBering)
    return(cmisst)
  })
  
  # Reset Button
  observeEvent(input$reset,{
    updateSliderInput(session,'lat',value = c(10, 62))
    updateSliderInput(session,'long',value = c(158, 246))
    updateSliderInput(session,'years',value = c(1981, 2020))
  })

  # Covariance Map
  output$covMap <- renderPlot({
    cmisst <- updateCMISST()
    myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

    season <- switch(input$season,
                   win = 2,
                   spr = 3,
                   sum = 4,
                   aut = 5)
    
    myTitle <- switch(input$season,
                     win = "Winter",
                     spr = "Spring",
                     sum = "Summer",
                     aut = "Autumn")
    
    covMap<-cmisst[[season]]
    lmt<-max(abs(covMap), na.rm=TRUE)
    limits<-c(-lmt, lmt)
    
    ggplot() + ggtitle(myTitle) +
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
    cmisst <- updateCMISST()
    index <- cmisst[[1]]
    plot(index$year, index$win.cov, type='b', pch=20, col="red4",
         xlab="", ylab="CMISST Index",
         ylim=c(min(index[,1:4], na.rm=TRUE), max(index[,1:4], na.rm=TRUE)))
    points(index$year, index$spr.cov, type='b', pch=20, col="blue")
    points(index$year, index$sum.cov, type='b', pch=20, col="green3")
    points(index$year, index$aut.cov, type='b', pch=20, col="purple")
    legend("topleft", legend = c("Win","Spr","Sum","Aut"), bty='n',
           col = c("red4","blue","green3","purple"), pch = 20, lty=1)
    
  })
  
  # Biplot with response
  output$biplot <- renderPlot({
    cmisst <- updateCMISST()
    index <- cmisst[[1]]
    season <- switch(input$season,
                     win = 1,
                     spr = 2,
                     sum = 3,
                     aut = 4)
    index$ind <- index[,season]
    myTitle <- switch(input$season,
                      win = "Winter",
                      spr = "Spring",
                      sum = "Summer",
                      aut = "Autumn")
    plot(index$ind, index$val, pch=20, cex=2, xlab="CMISST Index", ylab="Response", main=myTitle)
    abline(lm(index$val~index$ind))

  })

  output$table <- renderTable({
    cmisst <- updateCMISST()
    out<-cmisst[[1]]
    out$year <- as.integer(out$year)
    out <- out[,c(5,6,1:4)]
    colnames(out)[2] <- "response"
    cbind(out)
  })
  
  output$download <- downloadHandler(
    filename = function(){"cmisst.csv"}, 
    content = function(fname){
      write.csv(updateCMISST()[[1]], fname, row.names = FALSE)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)