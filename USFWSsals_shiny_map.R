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
    column(6,
           plotOutput("plot2", click = "plot2_click", height = "600px"),
           helpText("Click on a point to see its location on the map")
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
  
  output$plot2 <- renderPlot({
    plot(SHARP_patches_4plot$Saltmarsh_Sparrow_abundance, SHARP_patches_4plot$Forest_loss, log="xy", xaxt="n", yaxt="n", 
         xlab="Saltmarsh sparrow abundance", ylab="Forest loss (poportion)", bty="n", pch=16, col=rgb(0, 0, 0, 0.5), cex=1.5, cex.lab=1.25)
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
