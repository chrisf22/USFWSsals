### Author: Christopher Field
### 3/1/2020

### This app allows the user to plot key variables related to marsh management alongside Saltmarsh Sparrow (Ammospiza caudacuta) 
### abundance and view the location of selectable marsh patches. Saltmarsh Sparrow abundance is always shown on the x-axis.
### Each marsh patch is shown as a clickable dot. Once clicked, the location of a marsh patch will be displayed on the map.
### The shapefile "SHARP_patches_100m_USFWSsals.shp", the workspace "SHARP_patches_wforest.RData", and the document, 
### "Shiny_metadata.pdf" must be available in the main directory.

## app requires 'leaflet' (for the interactive map) and 'rdgal' (to create an object from the shapefile)

library(shiny)
library(leaflet)
library(rgdal)

ui <- fluidPage(title="Saltmarsh sparrow management",
                # hide errors from shiny app so plots don't immediately load with errors
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                fluidRow(column(12, div(style = "height:95px;padding-top: 5px; padding-bottom: 0px", imageOutput("myImageHeader")))),
                fluidRow(
                  column(2, div(style = "padding: 15px",
                                selectInput(inputId = "ytest", 
                                            label = "Y-axis", 
                                            choices = c("Forest loss" = "1", 
                                                        "Proportion restricted" = "2", 
                                                        "Sea-level rise rate" = "3"), 
                                            selected = "Forest_loss"),
                                sliderInput("slider1", label = "Threshold", min = 0, 
                                            max = 1, value = 0.20),
                                radioButtons("state", "State",
                                             c("ALL" = "1",
                                               "VA" = "2",
                                               "MD" = "3",
                                               "DE" = "4",
                                               "NJ" = "5",
                                               "NY" = "6",
                                               "CT" = "7",
                                               "RI" = "8",
                                               "MA" = "9",
                                               "NH" = "10",
                                               "ME" = "11")
                                ), br(), br(), 
                                downloadButton("downloadData", "Download documentation"))),
                  column(5,div(style = "height:720px; width:600px; background-color: rgba(187, 187, 187, 0); overflow-y: scroll;padding: 50px", 
                               plotOutput("plot2", click = "plot2_click", height = "475px"),
                               helpText("Click on a point to see its location in the map window. Data for the mapped location is shown below the window. 
                                        The threshold slider highights, for the x and y variables, the top or bottom X% of the locations (top X% for saltmarsh sparrow abundance, forest loss, and tidal restriction; bottom X% for sea-level rise). 
                                        The highlighted green points show locations that exceed the specified threshold for both the x and y variables.  "),
                  )
                  ),
                  column(5, div(style = "width:500px; padding: 15px",
                                div(style = "border-style: solid; border-width: 1px; border-color: rgba(0, 0, 0, 0.5)",
                                    leafletOutput("mymap", height = "570px")),
                                verbatimTextOutput("click_info")), br())
                ))


server <- function(input, output, session) {
  
  output$myImageHeader <- renderImage({
    list(src = "header_bw.png",
         contentType = 'image/png',
         width = 500,
         height = 90,
         alt = " ")
  }, deleteFile = FALSE)
  
  SHARP_polys <- readOGR("SHARP_patches_100m_USFWSsals.shp")
  SHARP_polys <- spTransform(SHARP_polys,"+init=epsg:4326")
  
  load("SHARP_patches_wforest.RData")
  # make all zeros in SHARP_patches_4plot = NA to avoid warnings
  SHARP_patches_4plot[SHARP_patches_4plot == 0] <- NA
  colnames(SHARP_patches_4plot)[1] <- "SHARP_Patch_ID"
  colnames(SHARP_patches_4plot)[4] <- "Forest_loss"
  colnames(SHARP_patches_4plot)[5] <- "Slope"
  colnames(SHARP_patches_4plot)[6] <- "Saltmarsh_Sparrow_abundance"
  
  click_info2 <- reactiveVal()
  click_info2(368)
  click_info2 <- reactive(
    nearPoints(SHARP_patches_4plot, xvar="Saltmarsh_Sparrow_abundance", yvar=varnames[as.numeric(input$ytest)], input$plot2_click, addDist = TRUE)[,"SHARP_Patch_ID"])
  
  message <- "No point selected"
  output$click_info <- renderPrint({if(identical(click_info2(), numeric(0))){message}else{
    round(nearPoints(SHARP_patches_4plot, xvar="Saltmarsh_Sparrow_abundance", yvar=varnames[as.numeric(input$ytest)], input$plot2_click, addDist = TRUE)[,c("SHARP_Patch_ID", "Forest_loss", "Saltmarsh_Sparrow_abundance", "Prop_restricted", "SLR")], 2)
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
  
  varnames <- c("Forest_loss", "Prop_restricted", "SLR")
  axisbounds <- list(c(0.0001, 0.001, 0.01, 0.1, 1), c(0, 0.25, 0.5, 0.75, 1), c(1, 2, 3, 4, 5, 6))
  axislabels <- list(c('0.0001', '0.001', '0.01', '0.1', '1'), c('0', '0.25', '0.5', '0.75', '1'), c("1", "2", "3", "4", "5", "6"))
  ylabs <- c("Forest loss (poportion of patch)", "Tidal restriction (proportion of patch)", "Sea-level rise rate (mm/year)")
  loglogical <- c("xy", "x", "x")
  plotlims <- list(c(0.0001, 1), c(0.00001, 1), c(1, 6))
  
  output$plot2 <- renderPlot({
    par(mar=c(5, 4, 0, 3))
    plot(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])], SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])], log=loglogical[as.numeric(input$ytest)], xaxt="n", yaxt="n", 
         xlab="Saltmarsh sparrow abundance", ylab=ylabs[as.numeric(input$ytest)], bty="n", pch=16, col=rgb(0, 0, 0, 0.25), cex=1.5, cex.lab=1.25, ylim=plotlims[[as.numeric(input$ytest)]], xlim=c(0.0001, 10000))
    axis(side=1, at=c(0.0001, 0.01, 1, 100, 10000), 
         labels=c('0.0001', '0.01', '1', '100', '10000'), cex.axis=1.25)
    #axis(side=2, at=c(0.0001, 0.001, 0.01, 0.1, 1), 
    #labels=c('0.0001', '0.001', '0.01', '0.1', '1'), cex.axis=1.25)
    axis(side=2, at=axisbounds[[as.numeric(input$ytest)]], 
         labels=axislabels[[as.numeric(input$ytest)]], cex.axis=1.25)
    xx <- SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])] >= quantile(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])], c(1-input$slider1), na.rm=TRUE)
    #yy <- SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])] >= quantile(SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])], c(1-input$slider1), na.rm=TRUE)
    slog <- SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])] >= quantile(SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])], c(1-input$slider1), na.rm=TRUE)
    xlog <- SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])] <= quantile(SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])], c(input$slider1), na.rm=TRUE)
    sliderdir <- cbind(slog, slog, xlog)
    yy <- sliderdir[,as.numeric(input$ytest)]
    points(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])][yy], SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])][yy], pch=16, col=rgb(.243, .22, .635, 0.5), cex=1.55)
    points(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])][xx], SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])][xx], pch=16, col=rgb(.149, .486, .565, 0.5), cex=1.55)
    points(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])][as.logical(xx*yy)], SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])][as.logical(xx*yy)], pch=16, col=rgb(.384, .788, .188, 0.5), cex=1.55)
  })
  
  output$mymap <- renderLeaflet({
    leaflet(SHARP_polys2()) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5) %>%
      #addPolylines() %>%
      addTiles()
    #addMarkers(data = latlong()) #%>%
    #fitBounds(bounds()[1, 1], bounds()[2, 1], bounds()[1, 2], bounds()[2, 2])
    #clearBounds()
  })
  
  output$downloadData <- downloadHandler(
    filename = "Shiny_metadata.pdf",
    content = function(file) {
      file.copy("Shiny_metadata.pdf", file)
    }
  )
}

shinyApp(ui, server)
