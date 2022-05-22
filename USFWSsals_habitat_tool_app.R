### Author: Christopher Field
### 3/1/2020

### This app allows the user to plot key variables related to marsh management alongside Saltmarsh Sparrow (Ammospiza caudacuta) 
### abundance and view the location of selectable marsh patches. Saltmarsh Sparrow abundance is always shown on the x-axis.
### Each marsh patch is shown as a clickable dot. Once clicked, the location of a marsh patch will be displayed on the map.
### The shapefile "SHARP_patches_100m_USFWSsals.shp", the workspace "SHARP_patches_wforest.RData", and the document, 
### "SSMT_doc.pdf" must be available in the main directory.

## app requires 'leaflet' (for the interactive map) and 'rdgal' (to create an object from the shapefile)

library(shiny)
library(leaflet)
library(rgdal)
library(shinydashboard)

header <- dashboardHeader(
  disable = FALSE, titleWidth = 830, 
  title = "Saltmarsh Sparrow management tool: a data-driven aid for identifying suitable marshes") #400

# optional code to use banner instead of text for title
#title = tags$a(tags$img(src='banner.png', height=80)), 
#titleWidth = "95%"

sidebar <- dashboardSidebar(
  width = 125,
  div(style = "height:100px; width:124.5px; padding-top: 0px; padding-bottom: 0px;", imageOutput("myImageHeader")),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard"),
    menuItem("Setup", tabName = "setup"),
    menuItem("Background", tabName = "background")
  )
)

body <- dashboardBody(
  tags$head(tags$style(HTML(
    '.shiny-output-error {visibility: hidden;}
  .shiny-output-error:before {visibility: hidden;}
  .skin-black .main-header .navbar {
  background-color: #2E2E2E;
  }
  .skin-black .main-header .logo {
  background-color: #2E2E2E;
  color: #fff;
  border: #2E2E2E;
  }
  .skin-black .main-header .logo:hover {
  background-color: #2E2E2E;
  color: #fff;
  border: #2E2E2E;
  }
  .skin-black .main-sidebar {
  background-color: #fff;
  }
  .skin-black .main-header .navbar .sidebar-toggle:hover{
  background-color: #454545;
  color: #fff;
  border: #2E2E2E;
  }
  .skin-black .main-header .navbar .sidebar-toggle{
  background-color: #363636;
  color: #fff;
  border: #2E2E2E;
  }
  /* active selected tab in the sidebarmenu */
  .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
  background-color: #2E2E2E;
  color: #fff;
  }
  /* other links in the sidebarmenu */
  .skin-black .main-sidebar .sidebar .sidebar-menu a{
  background-color: #fff;
  color: #000000;
  }
  /* other links in the sidebarmenu when hovered */
  .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
  background-color: #2E2E2E;
  color: #fff;
  }
  .main-header .logo {
  font-family: "Avenir", "Helvetica", sans-serif;
  font-size: 20px;
  }
  .content-wrapper {
  background-color: #fff;
  }'
  ))),
  
  tabItems(
    tabItem(tabName = "setup",
            column(4, div(style = "padding: 15px",
                          selectInput(inputId = "ytest", 
                                      label = "Y-axis", 
                                      choices = c("Forest loss adjacent to marsh" = "1", 
                                                  "Proportion of marsh behind tidal restriction" = "2", 
                                                  "Sea-level rise rate" = "3"), 
                                      selected = "Forest loss"), 
                          br(),
                          sliderInput("slider1", label = "Threshold", min = 0, 
                                      max = 1, value = 0.20), 
                          br(),
                          radioButtons("basemap", "Map background",
                                       c("Street map" = "2",
                                         "Load satellite" = "1"))
            )),
            column(2, div(style = "padding: 15px", radioButtons("state", "State",
                                                                c("ALL" = "1",
                                                                  "Virginia" = "2",
                                                                  "Maryland" = "3",
                                                                  "Delaware" = "4",
                                                                  "New Jersey" = "5",
                                                                  "New York" = "6",
                                                                  "Connecticut" = "7",
                                                                  "Rhode Island" = "8",
                                                                  "Massachusetts" = "9",
                                                                  "New Hampshire" = "10",
                                                                  "Maine" = "11"))
            )), 
            br(),
            column(4, div(style = "font-size:16px; color: #6A6A6A", "Use the drop-down menu to choose the management variable to show on the y-axis of the dashboard plot. Use the slider to choose a threshold value that 
                               sets, for the x and y variables, the top X % of the locations to highlight on the plot. The map background can be set to show the OpenStreetMap 
                               layer or satellite imagery from the U.S. Geological Survey. If desired, use the radio button to filter the points by state, which reduces the number of 
                               plotted points, making it easier to interpret the plot and click the correct patch. For more information, see the Background tab."))
    ),
    tabItem(tabName = "dashboard",
            fluidRow(
              column(6, div(style = "height:680px; width:550px; background-color: rgba(187, 187, 187, 0); overflow-y: scroll;padding: 0px; margin-left: 15px", 
                            div(style = "font-size:15px; color: #6A6A6A", "Please allow the map 20 seconds for the initial load. Click on a point to see its location in the map window. Attributes for the mapped location are shown below the plot."), br(),
                            plotOutput("plot2", click = "plot2_click", height = "475px"), br(),
                            tableOutput('table')
              )),
              column(6, div(style = "width:550px; padding: 0px; margin-right: 25px;",
                            div(style = "border-style: solid; border-width: 1px; border-color: rgba(0, 0, 0, 0.5)",
                                leafletOutput("mymap", height = "620px")) #570
              ),br())
            ),
            fluidRow(
              column(12, div(style="display: inline-block; margin-left: 25px", 
                             tags$a(rel="license", href="http://creativecommons.org/licenses/by-nc/4.0/", img(alt="Creative Commons License", style="border-width:0", src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png")),
                             HTML("&nbsp", "This work is licensed under a"),
                             tags$a(rel="license", href="http://creativecommons.org/licenses/by-nc/4.0/", "Creative Commons Attribution-NonCommercial 4.0 International License."), br(), br()
              ))
            )
    ),
    tabItem(tabName = "background", 
            fluidRow(
              column(9, div(style = "padding: 15px;", 
                            div(style = "font-size:17px; color: #6A6A6A", HTML(paste0("The goal of this application is to synthesize datasets relevant to the management of Saltmarsh Sparrows (", em("Ammospiza caudacuta"), ") that are difficult to access or would otherwise require specialized software to 
                                process. This app currently includes data that span the Saltmarsh Sparrow breeding range, including remote sensing data from Google Earth Engine and University of Massachusetts, tide gauge data from NOAA, and a spatial layer from the Saltmarsh Habitat and Avian 
                                Research Program (SHARP) that contains tidal marsh extent and projections of Saltmarsh Sparrow abundance. The interactive graph and map allow the user to visualize key variables related to marsh management alongside Saltmarsh Sparrow abundance to find areas where 
                                high management suitability and sparrow abundance overlap. These variables currently related to three management actions: encouraging marsh migration, attenuating high tides using tide gates, and using thin layer deposition to allow marshes to keep pace with 
                                sea-level rise. The specific variables that indicate the suitability of these actions at marshes across the Saltmarsh Sparrow range are, respectively, recent loss of coastal forest within 100 m of marsh (“Forest loss adjacent to marsh”), the proportion of the 
                                marsh patch that is behind tidal restrictions (“Proportion of marsh behind tidal restriction”), and the rate of sea-level rise that the marsh is experiencing (“Sea-level rise rate”)."))), 
                            br(),
                            div(style = "font-size:17px; color: #6A6A6A", HTML("Each marsh patch is shown as a clickable dot on the main plot. Once clicked, the location of a marsh patch will be displayed on the map and the attributes for the patch will be printed to a table below the plot. To begin, 
                                choose from the drop-down menu which management variable to show on the y-axis. Next, choose the threshold value to aid in visualizing the overlap between areas of high saltmarsh sparrow abundance and management potential. The threshold slider highlights, for the 
                                x and y variables, the top or bottom X % of the locations. The plot shows the top X % for saltmarsh sparrow abundance, forest loss, and tidal restriction. Note that the plot shows the bottom X % for rates of sea-level rise, since it is likely that marshes with 
                                lower rates would require less intensive thin layer management. The highlighted green points show locations that exceed the specified threshold for both the x and y variables – i.e. marshes that might be suitable for management in areas that would benefit Saltmarsh Sparrows.")), 
                            br(),
                            div(style = "font-size:17px;color: #6A6A6A", HTML("Saltmarsh Sparrow abundance estimates are from Wiest et al. (2018). Marsh patch boundaries follow Wiest et al. (2016), who used a 50-m buffering approach to group marshes into biologically relevant units using the U.S. Fish and Wildlife 
                                Service’s National Wetlands Inventory. The variable name “SHARP Patch ID” in the printed table refers to the identification field of this spatial layer (Wiest et al. 2016, see below for a link to access the data). Forest loss is the area 
                                within 100 m of tidal marsh that experienced loss between 2000-2018 (as a proportion of the forest extent at the beginning of the time period) from the Global Forest Loss dataset (Hansen et al. 2018), which I obtained for each marsh patch 
                                via analyses in Google Earth Engine (Field 2021). The area of marsh (ha) that is behind a tidal restriction is from an analysis by the University of Massachusetts Landscape Ecology Lab’s Designing Sustainable Landscapes. Sea-level rise rate 
                                (mm/year) is from the NOAA tide gauge that is closest to the marsh patch. For more information, see the downloadable documentation below.")), 
                            br(),
                            div(style = "font-size:17px; color: #6A6A6A", HTML("This app and its features, including all new analyses, were developed by Christopher R Field and funded in part by the U.S. Fish and Wildlife Service, Atlantic Coast Joint Venture (ACJV). To access the spatial files used in this app, see the"), 
                                tags$a(href="https://acjv.org/tools-and-data/", "ACJV data page.")), 
                            br(), br(),
                            downloadButton("downloadData", "Download documentation"), 
                            br(), br(), br(),
                            div(style="display: inline-block; margin-left: 0px", 
                                tags$a(rel="license", href="http://creativecommons.org/licenses/by-nc/4.0/", img(alt="Creative Commons License", style="border-width:0", src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png")),
                                HTML("&nbsp", "This work is licensed under a"),
                                tags$a(rel="license", href="http://creativecommons.org/licenses/by-nc/4.0/", "Creative Commons Attribution-NonCommercial 4.0 International License.")
                            ), 
                            br()
              )
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black", title="Saltmarsh sparrow management")

# wide header, which has been replaced with a square logo in the version that uses 'shinydashboard'
#fluidRow(column(12, div(style = "height:120px;padding-top: 5px; padding-bottom: 0px", imageOutput("myImageHeader")))),
#)

server <- function(input, output, session) {
  
  # create a function to round columns for table output while allowing different sig. figs./decimal places for each column
  round_df <- function(df) {
    df[, 2] <- round(df[, 2], digits = 1)
    df[, 3] <- round(df[, 3], digits = 2)
    df[, 4] <- round(df[, 4], digits = 2)
    df[, 5] <- round(df[, 5], digits = 1)
    df[, 1] <- as.character(df[,1])
    df[, 2] <- as.character(df[,2])
    df[, 3] <- as.character(df[,3])
    df[, 4] <- as.character(df[,4])
    df[, 5] <- as.character(df[,5])
    (df)
  }
  
  output$table <- renderTable({if(identical(click_info2(), numeric(0))){message}else{
    round_df(nearPoints(SHARP_patches_4plot[as.logical(bystate[,as.numeric(input$state)]), ], xvar="Sparrow abundance", yvar=varnames[as.numeric(input$ytest)], 
                        input$plot2_click, addDist = TRUE, threshold = 10)[1 ,c("SHARP Patch ID", "Sparrow abundance", "Forest loss", "Prop. restricted", "SLR")])
  }}, spacing = "m", align = "l", digits = 2)
  
  output$myImageHeader <- renderImage({
    list(src = "header_bw.png",
         contentType = 'image/png',
         width = 124.5,
         height = 100,
         alt = " ")
  }, deleteFile = FALSE)
  
  SHARP_polys <- readOGR("SHARP_patches_100m_USFWSsals.shp")
  SHARP_polys <- spTransform(SHARP_polys,"+init=epsg:4326")
  
  load("SHARP_patches_wforest.RData")
  # make all zeros in SHARP_patches_4plot = NA to avoid warnings
  SHARP_patches_4plot[SHARP_patches_4plot == 0] <- NA
  colnames(SHARP_patches_4plot)[1] <- "SHARP Patch ID"
  colnames(SHARP_patches_4plot)[10] <- "Prop. restricted"
  # 0 is more appropriate than NA for displaying prop. restricted
  SHARP_patches_4plot[is.na(SHARP_patches_4plot[ ,"Prop. restricted"]), "Prop. restricted"] <- 0
  colnames(SHARP_patches_4plot)[4] <- "Forest loss"
  colnames(SHARP_patches_4plot)[5] <- "Slope"
  colnames(SHARP_patches_4plot)[6] <- "Sparrow abundance"
  
  click_info2 <- reactiveVal()
  click_info2(368)
  click_info2 <- reactive(
    nearPoints(SHARP_patches_4plot[as.logical(bystate[,as.numeric(input$state)]), ], xvar="Sparrow abundance", yvar=varnames[as.numeric(input$ytest)], 
               input$plot2_click, addDist = TRUE, threshold = 10)[,"SHARP Patch ID"]
  )
  
  message <- data.frame("No point selected")
  colnames(message) <- "  "
  output$click_info <- renderPrint({if(identical(click_info2(), numeric(0))){message}else{
    round(nearPoints(SHARP_patches_4plot[as.logical(bystate[,as.numeric(input$state)]), ], xvar="Sparrow abundance", yvar=varnames[as.numeric(input$ytest)], 
                     input$plot2_click, addDist = TRUE, threshold = 10)[,c("SHARP Patch ID", "Forest loss", "Sparrow abundance", "Prop. restricted", "SLR")], 2)
  }})
  
  SHARP_polys2 <- reactiveVal()
  SHARP_polys2 <- reactive(if(identical(click_info2(), 368)){NULL}else{subset(SHARP_polys, SHARP_polys$PatchID==click_info2())})
  bounds <- reactive(bbox(SHARP_polys2()))
  
  bystate <- mat.or.vec(length(SHARP_patches_4plot$States), 11)
  bystate[,1] <- rep(1, length(SHARP_patches_4plot$States))
  bystate[,2] <- SHARP_patches_4plot$States == "VA"
  bystate[,3] <- SHARP_patches_4plot$States == "MD"
  bystate[,4] <- SHARP_patches_4plot$States == "DE"
  bystate[,5] <- SHARP_patches_4plot$States == "NJ"
  bystate[,6] <- SHARP_patches_4plot$States == "NY"
  bystate[,7] <- SHARP_patches_4plot$States == "CT"
  bystate[,8] <- SHARP_patches_4plot$States == "RI"
  bystate[,9] <- SHARP_patches_4plot$States == "MA"
  bystate[,10] <- SHARP_patches_4plot$States == "NH"
  bystate[,11] <- SHARP_patches_4plot$States == "ME"
  
  varnames <- c("Forest loss", "Prop. restricted", "SLR")
  axisbounds <- list(c(0.0001, 0.001, 0.01, 0.1, 1), c(0, 0.25, 0.5, 0.75, 1), c(1, 2, 3, 4, 5, 6))
  axislabels <- list(c('0.0001', '0.001', '0.01', '0.1', '1'), c('0', '0.25', '0.5', '0.75', '1'), c("1", "2", "3", "4", "5", "6"))
  ylabs <- c("Forest loss (poportion of patch)", "Tidal restriction (proportion of patch)", "Sea-level rise rate (mm/year)")
  loglogical <- c("xy", "x", "x")
  plotlims <- list(c(0.0001, 1), c(0.00001, 1), c(1, 6))
  
  output$plot2 <- renderPlot({
    par(mar=c(5, 4.3, 0, 3))
    plot(SHARP_patches_4plot[as.logical(bystate[,as.numeric(input$state)]), "Sparrow abundance"], 
         SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])], log = loglogical[as.numeric(input$ytest)], xaxt = "n", yaxt = "n", 
         xlab="Sparrow abundance", ylab = " ", bty = "n", pch = 16, col = rgb(0, 0, 0, 0.25), cex=1.5, cex.lab=1.25, ylim = plotlims[[as.numeric(input$ytest)]], xlim = c(0.0001, 10000))
    mtext(side = 2, line = 3.2, ylabs[as.numeric(input$ytest)], cex = 1.25)
    axis(side=1, at=c(0.0001, 0.01, 1, 100, 10000), 
         labels=c('0.0001', '0.01', '1', '100', '10000'), cex.axis=1.25)
    axis(side=2, at=axisbounds[[as.numeric(input$ytest)]], 
         labels=axislabels[[as.numeric(input$ytest)]], cex.axis=1.25)
    xx <- SHARP_patches_4plot[as.logical(bystate[,as.numeric(input$state)]), "Sparrow abundance"] >= quantile(SHARP_patches_4plot[as.logical(bystate[,as.numeric(input$state)]), "Sparrow abundance"], c(1 - input$slider1), na.rm=TRUE)
    #yy <- SHARP_patches_4plot[ ,varnames[as.numeric(input$ytest)]][as.logical(bystate[ ,as.numeric(input$state)])] >= quantile(SHARP_patches_4plot[ ,varnames[as.numeric(input$ytest)]][as.logical(bystate[ ,as.numeric(input$state)])], c(1 - input$slider1), na.rm=TRUE)
    slog <- SHARP_patches_4plot[ ,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])] >= quantile(SHARP_patches_4plot[ ,varnames[as.numeric(input$ytest)]][as.logical(bystate[ ,as.numeric(input$state)])], c(1 - input$slider1), na.rm=TRUE)
    xlog <- SHARP_patches_4plot[ ,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])] <= quantile(SHARP_patches_4plot[ ,varnames[as.numeric(input$ytest)]][as.logical(bystate[ ,as.numeric(input$state)])], c(input$slider1), na.rm=TRUE)
    sliderdir <- cbind(slog, slog, xlog)
    yy <- sliderdir[ ,as.numeric(input$ytest)]
    points(SHARP_patches_4plot[as.logical(bystate[ ,as.numeric(input$state)]), "Sparrow abundance"][yy], SHARP_patches_4plot[ ,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])][yy], pch=16, col=rgb(.243, .22, .635, 0.5), cex=1.55)
    points(SHARP_patches_4plot[as.logical(bystate[ ,as.numeric(input$state)]), "Sparrow abundance"][xx], SHARP_patches_4plot[ ,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])][xx], pch=16, col=rgb(.149, .486, .565, 0.5), cex=1.55)
    points(SHARP_patches_4plot[as.logical(bystate[ ,as.numeric(input$state)]), "Sparrow abundance"][as.logical(xx*yy)], SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[ ,as.numeric(input$state)])][as.logical(xx*yy)], pch=16, col=rgb(.384, .788, .188, 0.5), cex=1.55)
  })
  
  output$mymap <- renderLeaflet({
    if(as.numeric(input$basemap)==1){
      leaflet(SHARP_polys2()) %>%
        fitBounds(lng1 = bounds()[1, 1], lat1 = bounds()[2, 1], lng2 = bounds()[1, 2], lat2 = bounds()[2, 2], options = list(maxZoom = 12)) %>%
        addPolygons(color = "white", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5, group = "SHARP polys") %>%
        #addPolylines() %>%
        addTiles() %>%
        addWMSTiles(
          "http://basemap.nationalmap.gov/ArcGIS/services/USGSImageryTopo/MapServer/WMSServer",
          layers = "0",
          options = WMSTileOptions(format = "image/png", transparent = TRUE),
          attribution = "USGS"
        )
      #addMarkers(data = latlong()) #%>%
      #fitBounds(bounds()[1, 1], bounds()[2, 1], bounds()[1, 2], bounds()[2, 2])
      #clearBounds()
    }else{
      leaflet(SHARP_polys2()) %>%
        fitBounds(lng1 = bounds()[1, 1], lat1 = bounds()[2, 1], lng2 = bounds()[1, 2], lat2 = bounds()[2, 2], options = list(maxZoom = 12)) %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5) %>%
        #addPolylines() %>%
        addTiles()
        #addMarkers(data = latlong()) #%>%
        #clearBounds()
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = "SSMT_doc.pdf",
    content = function(file) {
      file.copy("SSMT_doc.pdf", file)
    }
  )
}

shinyApp(ui, server)

