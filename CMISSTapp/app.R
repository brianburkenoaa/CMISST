library(shiny)
library(reshape2)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(doBy)


# set parameters
dataSet='ERSST'
months=seq(1,12,1)
removeBering=TRUE
returnDataType='anom'
returnObjectType='array'
loocvYears=5 # the most recent X years to include in the LOO CV


# Define UI ----
ui <- fluidPage(
  titlePanel("CMISST"),
  
  tags$head(
    tags$style(HTML(".help-block a {font-size: 75%;}"))
  ),
  
  sidebarLayout(
    sidebarPanel(h1("Controls"),
                 
                 fluidRow(
                   hr(style = "border-top: 1px solid #000000;"),

                   # Input: Choose a dataset ----
                   radioButtons(inputId = "spatialData",
                                label = "Spatial Data",
                                choices = c("SST" = "ERSST",
                                            "SSH (higher res, will take longer)" = "SSH"),
                                selected = "ERSST"),

                   # Input: Choose a stock ----
                   selectInput(inputId = "stock",
                               label = "Select Response",
                               choices = c("Spring Chinook" = "spCK",
                                           "Fall Chinook" = "faCK",
                                           "Steelhead" = "steel"),
                               selected = "sprCh"),
                   
                   # Input: Choose a file ----
                   fileInput('datafile', 'Or choose your own CSV file',
                             accept=c('csv', 'comma-separated-values','.csv')),
                   div(style = "margin-top: -20px"),
                   helpText(a('CSV file should have 2-columns (year and response) with headers.',
                            'To reset the file to null, refresh the webpage')),
                   
                   hr(style = "border-top: 1px solid #000000;"),
                   # Input: log response? ----
                   checkboxInput(inputId = "log",
                                 label = "Log response?",
                                 value = TRUE),

                   hr(style = "border-top: 1px solid #000000;"),
                   # Input: Slider for Ocean Years ----
                   sliderInput(inputId = "years",
                               label = "Years:",
                               min = 1970,
                               max = 2023,
                               sep="",
                               value = c(1980, 2021)),
                   
                   # Input: lag response? ----
                   selectInput(inputId = "lag",
                               label = "Lag Response:",
                               choices = c("None" = 0,
                                           "1 year" = 1,
                                           "2 years" = 2,
                                           "3 years" = 3),
                               selected = 2),
                   
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
                   # Input: MAE LOO CV? ----
                   checkboxInput(inputId = "loocv",
                                 label = "Perform Cross-Validation?",
                                 value = FALSE),
                   div(style = "margin-top: -20px"),
                   helpText(a('Caution: performing LOO CV will increase',
                            'the amount of time each refresh takes.  The only',
                            'output is the Mean Absolute Error of a Leave',
                            'One Out Cross Validation in the biplot')),
                   
                   hr(style = "border-top: 1px solid #000000;"),
                   # Input: What map to plot
                   radioButtons("season", "Which season to plot:",
                                c("Winter (Jan-Mar)" = "win",
                                  "Spring (Apr-Jun)" = "spr",
                                  "Summer (Jul-Sep)" = "sum",
                                  "Autumn (Oct-Dec)" = "aut"),
                                selected = "spr"),
                   
                   hr(style = "border-top: 1px solid #000000;"),
                   # Input: Reset button ----
                   actionButton(inputId = "reset",
                                label = "Reset all"),
                   
                   hr(style = "border-top: 1px solid #000000;"),
                   # Download the results to a file
                   downloadButton('download',"Download the Results")
                 )
    ),
    mainPanel(h3("Covariance Map of Sea Surface Temperature"),
              h5("(or other spatial data)"),
              
              # Output: Covariance Map ----
              plotOutput(outputId = "covMap"),
              
              # Output: biplot of index and response ----
              plotOutput(outputId = "biplot"),
              
              # Output: Time series plot ----
              plotOutput(outputId = "index"),
              
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
    
    # User input file, if provided
    inFile <- input$datafile
    if (!is.null(inFile)) {
      response.tmp <- read.csv(inFile$datapath, header=TRUE)
      response.tmp$year <- response.tmp$year - as.numeric(input$lag)
      response.tmp <- response.tmp[response.tmp$year %in% years,]
    } else{
      response.tmp <- response
      response.tmp$year <- response.tmp$year - as.numeric(input$lag)
      response.tmp <- response.tmp[response.tmp$year %in% years, c('year', input$stock)]
    }
    # We changed the response years, so make sure years doesn't go beyond the new range
    years <- years[years %in% response.tmp$year]

    colnames(response.tmp) <- c('year','val')
    if(input$log) response.tmp$val <- log(response.tmp$val)
    response.tmp$val.scl <- scale(response.tmp$val)

    if (input$spatialData == "ERSST") oceanData <- oceanData_ERSST
    if (input$spatialData == "SSH") oceanData <- oceanData_SSH

    cmisst <- get_CMISST_index(response = response.tmp[,c("year","val.scl")],
                               oceanData = oceanData,
                               min.lon = min.lon, max.lon = max.lon,
                               min.lat = min.lat, max.lat = max.lat,
                               years = years, months = months,
                               returnDataType = returnDataType,
                               removeBering = removeBering)
    
    if (input$loocv) {
      loocv <- LOO_CV(response = response.tmp[,c("year","val.scl")],
                                     oceanData = oceanData, loocvYears = loocvYears,
                                     min.lon = min.lon, max.lon = max.lon,
                                     min.lat = min.lat, max.lat = max.lat,
                                     years = years, months = months)
      return(append(cmisst, loocv))
    } else return(cmisst)
  })

  # Reset Button
  observeEvent(input$reset,{
    updateSliderInput(session,'lat',value = c(10, 62))
    updateSliderInput(session,'long',value = c(158, 246))
    updateSliderInput(session,'years',value = c(1981, 2023))
    updateRadioButtons(session = session, inputId = 'season', selected = 'spr')
    #reset('datafile') # this would require the shinyjs package
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
    extent <- cmisst[[6]] # min, max of lat, long
    
    gg <- ggplot() + ggtitle(myTitle)
      # geom_raster is faster, but wants all cells to be the same size, which it seems tehy are not for the SSH data??
      if (input$spatialData == "SSH") gg <- gg + geom_tile(data = melt(covMap),
                aes(x = Var1, y = Var2, fill=value), lwd=0, lty=0)
      if (input$spatialData == "ERSST") gg <- gg + geom_raster(data = melt(covMap),
                aes(x = Var1, y = Var2, fill=value))
    gg <- gg +
      geom_sf(data=land, color="black", fill="grey", linewidth=0.25) +
      xlim(extent[3], extent[4]) + ylim(extent[1], extent[2]) +
      scale_fill_gradientn(colours = myPalette(100),limits=limits,name="Covariance", na.value = "white") +
      #scale_fill_gradientn(colours = myPalette(100), name="Covariance", na.value = "white") +
      theme_classic() + theme(panel.border = element_rect(colour = "grey", fill=NA)) +
      labs(x = "Longitude", y = "Latitude")
    gg
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
    plot(index$ind, index$val, pch=20, cex=2, xlab=paste(myTitle, "CMISST Index"),
         ylab="Scaled (Z-score) Response", main=myTitle)
    lm1 <- lm(index$val~index$ind)
    abline(lm1)
    text(bquote(~ R^2 == .(round(summary(lm1)$adj.r.squared, 2))),
         x = par("usr")[1]*0.8, y=par("usr")[4]*0.80, cex=1.6, col="blue")
    if (input$loocv) {
      mae <- cmisst[[7]]
      text(paste("MAE =", round(mae[season,3], 2)),
         x = par("usr")[1]*0.75, y=par("usr")[4]*0.60, cex=1.6, col="blue")
    }
  })

  # Output: Index time series
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
  
  # Output: Table
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