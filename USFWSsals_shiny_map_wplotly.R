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
           helpText("Click on a point to see its location in the map window. Data for the mapped location is shown below the window. ")
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
  colnames(SHARP_patches_4plot)[6] <- "Saltmarsh_Sparrow_abundance"
    
  output$click_info <- renderPrint({
    nearPoints(SHARP_patches_4plot, xvar="Saltmarsh_Sparrow_abundance", yvar="Forest_loss", input$plot2_click, addDist = TRUE)[,c("SHARP_Patch_ID", "Forest_loss", "Saltmarsh_Sparrow_abundance")]
  })
  
  latlong <-
    reactive(nearPoints(SHARP_patches_4plot, xvar="Saltmarsh_Sparrow_abundance", yvar="Forest_loss", input$plot2_click, addDist = FALSE)[,c("long", "lat")])
  
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
  
  # where does this go?
  output$plot2 <- renderPlot({
    plot(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance[as.logical(bystate[,as.numeric(input$state)])], SHARP_patches_4plot$Forest_loss[as.logical(bystate[,as.numeric(input$state)])], log="xy", xaxt="n", yaxt="n", 
         xlab="Saltmarsh sparrow abundance", ylab="Forest loss (poportion of patch)", bty="n", pch=16, col=rgb(0, 0, 0, 0.5), cex=1.5, cex.lab=1.25)
    axis(side=1, at=c(0.0001, 0.01, 1, 100, 10000), 
         labels=c('0.0001', '0.01', '1', '100', '10000'), cex.axis=1.25)
    axis(side=2, at=c(0.0001, 0.001, 0.01, 0.1, 1), 
         labels=c('0.0001', '0.001', '0.01', '0.1', '1'), cex.axis=1.25)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = latlong())
  })
}

shinyApp(ui, server)
