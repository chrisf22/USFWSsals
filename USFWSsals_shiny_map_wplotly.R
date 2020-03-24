library(shiny)
library(leaflet)

ui <- fluidPage(
  # hide errors from shiny app so plots don't immediately load with errors
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  titlePanel("Explore where forest retreat overlaps with high Saltmarsh Sparrow abundance"),
  
  fluidRow(
    column(1,
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
           )),
    column(5,
           plotOutput("plot2", click = "plot2_click", height = "600px"),
           helpText("Click on a point to see its location in the map window. Data for the mapped location is shown below the window. The threshold slider highights, for the x and y variables, the greatest or lowest X% of the locations. ")
    ),
    column(6,
           leafletOutput("mymap", height = "600px"),
           verbatimTextOutput("click_info"))
  ))


server <- function(input, output, session) {
  
  load("SHARP_patches_wforest.RData")
  # to avoid warnings, make all zeros in SHARP_patches_4plot = NA
  SHARP_patches_4plot[SHARP_patches_4plot == 0] <- NA
  # 'SHARP_patches_4plot' must be in workspace
  colnames(SHARP_patches_4plot)[1] <- "SHARP_Patch_ID"
  colnames(SHARP_patches_4plot)[4] <- "Forest_loss"
  colnames(SHARP_patches_4plot)[5] <- "Slope"
  colnames(SHARP_patches_4plot)[6] <- "Saltmarsh_Sparrow_abundance"
    
  output$click_info <- renderPrint({
    nearPoints(SHARP_patches_4plot, xvar="Saltmarsh_Sparrow_abundance", yvar=varnames[as.numeric(input$ytest)], input$plot2_click, addDist = TRUE)[,c("SHARP_Patch_ID", "Forest_loss", "Saltmarsh_Sparrow_abundance")]
  })
  
  latlong <-
    reactive(nearPoints(SHARP_patches_4plot, xvar="Saltmarsh_Sparrow_abundance", yvar=varnames[as.numeric(input$ytest)], input$plot2_click, addDist = FALSE)[,c("long", "lat")])
  
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
 
  # where does this go?
  output$plot2 <- renderPlot({
    plot(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])], SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])], log=loglogical[as.numeric(input$ytest)], xaxt="n", yaxt="n", 
         xlab="Saltmarsh sparrow abundance", ylab=ylabs[as.numeric(input$ytest)], bty="n", pch=16, col=rgb(0, 0, 0, 0.5), cex=1.5, cex.lab=1.25)
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
    points(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])][yy], SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])][yy], pch=16, col="blue")
    points(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])][xx], SHARP_patches_4plot[,varnames[as.numeric(input$ytest)]][as.logical(bystate[,as.numeric(input$state)])][xx], pch=16, col=rgb(1, 0, 0, 0.5))
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = latlong())
  })
}

shinyApp(ui, server)
