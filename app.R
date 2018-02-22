## app.R ##
library(rgdal)
library(dplyr)
library(shiny)
library(raster)
library(shinydashboard)
library(shinycssloaders)
library(htmlwidgets)
library(leaflet)
library(scales)
library(ggplot2)
#library(plotly)
library(lubridate)
library(httr)
library(jsonlite)
library(XML)

today <- Sys.Date()
today_1year <- today - years(1)
this_month <- paste0(months(today),"  ")

ui <- dashboardPage(
  dashboardHeader(title = "EMMA"),
  dashboardSidebar(
        sidebarMenu(id="tabs",
        menuItem("Info", tabName = "Info", icon = icon("info-circle")) ,          
        menuItem("Overview", tabName = "Overview", icon = icon("map")),
        menuItem("Diagnosis", tabName = "Diagnosis", icon = icon("map-o"))
      )
  ),
  dashboardBody( 
        tabItems(
          # 1 tab content
          tabItem(tabName = "Info",
                  fluidRow(
                    box(h2("Ecosystem monitoring and management algorithm (EMMA) version 0.1.2"),width=12)),
                  fluidRow(box(
                                h4("Welcome to EMMA, a tool to support the monitoring and management of vegetaion change in non-forest ecosystems.
                                  This beta version is intented for demonstrative purposes only and accompanies a submission to the UN Global Pulse
                                  Data for Climate Action challange"),
                                h4("The Overview tab shows the entire region being monitored, and displays a map of the current vegetation health as detected by
                                  our algorithms.The two layers displayed show the number of detections of vegetation activity above model expectations, and the number of detections 
                                   of vegetation activity below model expectations. Clicking on a pixel in this map will result in the trajectory of vegetation productivity being plotted 
                                   and compared to model predictions. The displayed data is available for download through the 'Download data' button"),
                                h4("The Diagnosis tab shows high-resolution true-colour satellite imagery from Planet Labs for a pixel selected in the Overview. Recent imagery
                                   can be compared with an image from 1 year ago. This view can provide insights into the spatial nature of vegetation change that may not be apparent from 
                                   the medium resolution imagery on which models are trained. In future versions of EMMA additional data will be included to aid diagnosis,
                                   such as weather and fire history"),width=12)),
                  fluidRow(box(img(src="logos2.png"),width=12))
                    #imageOutput("preImage",height="auto"))
                  
          ),
          # 2 tab content
        tabItem(tabName = "Overview",
                fluidRow(
                  box(h4("Click a pixel to view time series data. Move to the diagnosis tab to view high resolution satellite imagery"),width=12)),
                fluidRow(
                  box(withSpinner(leafletOutput("map1",height=550)),width=7),
                  box(withSpinner(plotOutput("tsplot1")),
                      downloadButton("downloadData", "Download data"),width=5)
                )
          ),
        # 3 tab content
        tabItem(tabName = "Diagnosis",
                fluidRow(
                  box(h4("After selecting a pixel in the overview tab, move the slider to compare the most recent image on top to the image from last year below"),
                      sliderInput("transp", "",  
                              min = 2016, max = 2017, value = 2017,ticks=FALSE,step=1,width='30%',sep="",pre=this_month),width=12)),
                fluidRow(
                  box(withSpinner(leafletOutput("MapPlot1",height=500)),width=12)
                )
        )
      )
  )
)

server <- function(input, output) {
  
# server setup ------------------------------------------------------------

  #setwd("~/science/D4CA/ModelOutput")
  source("get_planet.R")
 # source("weather_sentry.R")
  load("peninsula_data_oct2017_short.RData")
  cvplot4$date <- as.Date(cvplot4$date)
#  pras <- raster("jkopprecip.grd")
  iras <- raster("exceedbelow_int.grd")
  iras2 <- raster("exceedabove_int.grd")
  iras <- projectRasterForLeaflet(iras)
  iras2 <- projectRasterForLeaflet(iras2)
  isum <- summary(iras)
  isum2 <- summary(iras2)
  extRas <- extent(iras)
  extRas2 <- extent(iras2)
  bins <- c(0,1,2,5,10,20,round(isum[[5]],0))
  bins2 <- c(0,1,2,5,10,20,round(isum2[[5]],0))
  plotcols <- colorBin("YlOrRd",domain=c(round(isum[[1]],0),round(isum[[5]],0)),bins=bins,reverse=FALSE,na.color=NA)
  plotcols2 <- colorBin("YlGnBu",domain=c(round(isum2[[1]],0),round(isum2[[5]],0)),bins=bins2,reverse=FALSE,na.color=NA)
  clng = 18.4484
  clat = -33.9789
  planet_init <- 0
  planet_string0=""
  planet_string1=""
  cvplot5="no data selected"


# oberserves and reactives ------------------------------------------------
  #data download
  output$downloadData <- downloadHandler(
    filename = "emma_data.csv",
    content = function(file) {
      write.csv(cvplot5, file, row.names = FALSE)
    }
  )

  #organization logos
  # output$preImage <- renderImage({
  # 
  #   filename <- normalizePath(file.path('./images',
  #                                       paste('logos.png', sep='')))
  #   # Return a list containing the filename
  #   list(src = filename, 
  #        width = 700)
  #   
  # }, deleteFile = FALSE)
  
  #overview map
  output$map1 <- renderLeaflet({
    leaflet() %>%
     # fitBounds(extRas@ymin,extRas@xmin,extRas@ymax,extRas@xmax) %>% 
      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Map data @2017 Google') %>%
      addRasterImage(iras, colors = plotcols,group="Exceedance below",project=FALSE,opacity = 0.7) %>%
      addRasterImage(iras2, colors = plotcols2, group="Exceedance above",project=FALSE,opacity = 0.7) %>%
      addLayersControl(baseGroups =c("Exceedance below","Exceedance above"))
  })

  #high res map
  output$MapPlot1 <- renderLeaflet({
   leaflet() %>%
    setView(lng = 18.4484, lat = -33.9789, zoom = 14)
  })
  outputOptions(output,"MapPlot1",suspendWhenHidden=FALSE)
      
  #legends hide
  observeEvent(input$map1_groups,{
    map1 <- leafletProxy("map1") %>% clearControls()
    if (input$map1_groups == 'Exceedance below')
    {map1 <- map1 %>% addLegend("bottomright", 
                                title = "Exceedances below",
                                pal = plotcols, 
                                values = round(bins,0),
                                opacity = 0.6,
                                labels=as.character(bins),
                                labelFormat(digits = 0))}
                                  else if (input$map1_groups == 'Exceedance above')
                                  {map1 <- map1 %>% addLegend("bottomright", 
                                                              title = "Exceedances above",
                                                              pal = plotcols2, 
                                                              values = round(bins2,0),
                                                              opacity = 0.6,
                                                              labels=as.character(bins2),
                                                              labelFormat(digits = 0))}
                                  })
  
  #slider
  observe({

     leafletProxy('MapPlot1') %>% # use the proxy to save computation
        removeTiles("tr0") %>%
        addTiles(planet_string0,
                layerId ="tr0",
                group="TR",
                options = providerTileOptions(opacity =  (input$transp-2016)))
  })
  
  #diagnosis tab 
  observe({
    if (input$tabs == "Diagnosis") {
      #add planet data to map
      if(exists("rect_ext")){
          leafletProxy('MapPlot1') %>%
            clearShapes() %>%
            setView(lng = clng, lat = clat, zoom = 15) %>%
            addTiles(planet_string1,
                     layerId ="tr1",
                     attribution = 'Map data @2017 Planet') %>%
            addTiles(planet_string0,
                     layerId ="tr0",
                     group="TR",
                     attribution = 'Map data @2017 Planet',
                     options = providerTileOptions(opacity = 1)) %>%
            addRectangles(
              lng1=rect_ext@xmin, lat1=rect_ext@ymax,
              lng2=rect_ext@xmax, lat2=rect_ext@ymin,
              fillColor = "transparent"
            )}
    }
  })
  
  # Observe mouse clicks
  observeEvent(input$map1_click, {
    withProgress(message = "", value = 0, {
      

# get click ---------------------------------------------------------------
        click <- isolate(input$map1_click)
        clat <<- click$lat
        clng <<- click$lng
        cll <-  cbind(clng,clat)
        cllsp <- SpatialPoints(cll,CRS("+proj=longlat +datum=WGS84 +no_defs"))
        cllsp_t <- spTransform(cllsp,CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

         cellnum <- cellFromXY(iras, cllsp_t)

# if click is valid -------------------------------------------------------
         setProgress(1/4, detail = paste("25%"))
         if(!(is.na(cellnum))){
           # rectangle ---------------------------------------------------------------
             cell <- rasterFromCells(iras, cellnum)
             cell <- extent(cell)
        
             rectp <- as(cell, 'SpatialPolygons')
             proj4string(rectp) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
             rectp_t <- spTransform(rectp,CRS("+proj=longlat +datum=WGS84 +no_defs"))
             rect_ext <<- extent(rectp_t)
     
             #add rectangle
             leafletProxy('map1') %>%
               clearShapes() %>%
               addRectangles(
                 lng1=rect_ext@xmin, lat1=rect_ext@ymax,
                 lng2=rect_ext@xmax, lat2=rect_ext@ymin,
                 fillColor = "transparent"
               )
   
             setProgress(2/4, detail = paste("50%"))
            # ts plot -----------------------------------------------------------------
            cvplot5 <<- dplyr::filter(cvplot4,cellID==cellnum) %>% dplyr::arrange(date)

             output$tsplot1 <- renderPlot({
               shiny::validate(
                 shiny::need((nrow(cvplot5) > 0), 'No data for selected region'))
                 
             ggplot(data=cvplot5, aes(x=date)) +
                 #geom_point() +
                 geom_ribbon(aes(ymin=`lower limit 95%`,ymax=`upper limit 95%`),fill="grey",alpha=0.8)+
                 geom_ribbon(aes(ymin=`lower limit 50%`,ymax=`upper limit 50%`),fill="black",alpha=0.3)+
                 geom_line(aes(y=`observed NDVI`),color="blue") +
                # scale_x_date(date_breaks = "1 year",
                 #             labels=date_format("%Y"),
                 #             limits = as.Date(c('2013-01-01','2017-06-10'))) +
                 scale_y_continuous(limits=c(0,1)) +
                 #facet_wrap(~siteID) +
                 xlab("Date") +
                 ylab("Vegetation Index (MODIS NDVI)") +
                 theme_bw() +
                 theme(legend.position="none")
              
            
             })
            
            # Planet data -------------------------------------------------------------
            
            planet_init <- 1
            #get planet data

            planet_id0 <- get_planet(rect_ext,today)
  
            setProgress(3/4, detail = paste("75%"))
            
            planet_id1 <- get_planet(rect_ext,today_1year)
    
            planet_string0 <<- paste0("https://tiles.planet.com/data/v1/PSOrthoTile/",
                                     planet_id0,
                                     "/{z}/{x}/{y}.png?api_key=27d323a68bd24f97852dfd0416128a35")

            planet_string1 <<- paste0("https://tiles.planet.com/data/v1/PSOrthoTile/",
                                     planet_id1,
                                     "/{z}/{x}/{y}.png?api_key=27d323a68bd24f97852dfd0416128a35")
            

            # weather data -------------------------------------------------------------
          
          #  wsentry <- getWeather(clng,clat,today)
          #  clim <- extract(cll,clat,pras)
            
           # setProgress(3/3, detail = paste("100%"))
            
             }
         setProgress(4/4, detail = paste("100%"))

    })
  })
  
}





shinyApp(ui, server)
