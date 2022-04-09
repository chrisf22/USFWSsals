### Author: Christopher Field
### 3/14/2020

### This app allows the user to explore the correlations between Saltmarsh Sparrow (Ammospiza caudacuta) abundance, 
### and indicators of landward marsh migration: recent forest dieback and slope at the forest-to-marsh boundary. 
### The data on Saltmarsh sparrows are from range-wide surveys conducted by the Saltmarsh Habitat and Avian Research Program 
### (SHARP; www.tidalmarshbirds.org). Marsh migration indicators are from an analysis of the Global Forest Change (Hansen et al. 2020) 
### and U.S. Geological Survey National Elevation datasets using Google Earth Engine. The workspace "SALS_forest_workspace.RData" 
### contains objects for sparrow abundance and marsh migration indicators. The file "haunted_habitat_documentation.pdf" must be in the
### main working directory to be able to download the documentation.

## app requires 'plotly' as the figures use the Plotly JavaScript library

library(shiny)
library(plotly)


ui <- fluidPage(title="Saltmarsh sparrow management",
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                titlePanel(tags$h6(" ")),
                fluidRow(
                  column(3, 
                         img(src="tree2.gif", align = "left",height='230px',width='310px'),
                         helpText("This dashboard uses data to elucidate the current picture of the potential for tidal marsh migration across the range of the globally endangered Saltmarsh Sparrow. 
                         One of the most readily observable aspects of this process is the creation of “ghost forests”, which are stands of grey snags left behind when saltwater kills trees at the marsh boundary. 
                         Here, the extent of forest loss near tidal marshes is shown alongside data on the abundance of Saltmarsh Sparrows from range-wide surveys conducted by the Saltmarsh Habitat and Avian Research Program (SHARP). 
                         Areas that have greatest overlap between forest loss and sparrow abundance likely hold the greatest potential as future habitat for sparrows and other tidal marsh specialists. 
                        Download the documentation below for more information on the motivation, datasets, and analyses underlying these visualizations."), br(),
                         downloadButton("downloadData", "Download documentation")
                  ),
                  column(3, div(style = "height:750px; width:320px; background-color: rgba(187, 187, 187, 0); overflow-y: scroll;padding: 10px; border-style: solid; border-width: 1px; border-color: rgba(0, 0, 0, 0.2)",
                                plotlyOutput('plot2')
                  )
                  ),
                  column(6, div(style = "height:800px; width:530px; background-color: rgba(187, 187, 187, 0); overflow-y: scroll;padding: 10px",
                                plotlyOutput('plot'), br(), br()
                  ))
                )
)

server <- function(input, output) {
  
  output$downloadData <- downloadHandler(
    filename = "haunted_habitat_documentation.pdf",
    content = function(file) {
      file.copy("haunted_habitat_documentation.pdf", file)
    }
  )
  
  load('SALS_forest_workspace.RData')
  
  # updatemenus component
  # create buttons that toggle on/off the points for each state; states are specified in alphabetical order by default in 'plotly'
  updatemenus <- list(
    list(
      active = -1,
      type= 'buttons',
      x = -0.4,
      buttons = list(
        list(
          label = "VA",
          method = "update",
          args = list(list(visible = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)))),
        
        list(
          label = "MD",
          method = "update",
          args = list(list(visible = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)))), 
        
        list(
          label = "DE",
          method = "update",
          args = list(list(visible = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)))), 
        
        list(
          label = "NJ",
          method = "update",
          args = list(list(visible = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)))), 
        
        list(
          label = "NY",
          method = "update",
          args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)))), 
        
        list(
          label = "CT",
          method = "update",
          args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)))), 
        
        list(
          label = "RI",
          method = "update",
          args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)))), 
        
        list(
          label = "MA",
          method = "update",
          args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)))), 
        
        list(
          label = "NH",
          method = "update",
          args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)))), 
        
        list(
          label = "ME",
          method = "update",
          args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)))), 
        
        list(
          label = "All",
          method = "update",
          args = list(list(visible = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)))))
    )
  )
  
  # create 3 plots to be combined into one subplot; legend is turned off for all but one; these lines primarily use piping to set plot parameters
  p1 <- SHARP_patches_4plot%>%group_by(States)%>%plot_ly(x = round(SHARP_patches_4plot$SALS, 2), y = round(SHARP_patches_4plot$observed, 4), text = ~paste("SHARP patch ID = ", PatchID), 
                                                         color = ~States, type="scatter", colors = "Paired", legendgroup= ~States, 
                                                         showlegend=TRUE, mode="markers", height=700, width=520, opacity = 0.8) %>% #height=700, width=600
    layout(title = " ",
           xaxis=list(title=" ", type = "log", range = c(-3.8, 4)),
           yaxis=list(title=list(text = "Recent forest loss (proportion of patch)", standoff = 11), type = "log", tickformat="f"),
           updatemenus=updatemenus)
  
  p3 <- SHARP_patches_4plot%>%group_by(States)%>%plot_ly(x = round(SHARP_patches_4plot$SALS, 2), y = round(SHARP_patches_4plot$slope, 2), text = ~paste("SHARP patch ID = ", PatchID), 
                                                         color = ~States, type="scatter", colors = "Paired", legendgroup= ~States, 
                                                         showlegend=FALSE, mode="markers", height=700, width=520, opacity = 0.8) %>% #height=700, width=600
    layout(title = " ",
           xaxis=list(title="Saltmarsh sparrow abundance", type = "log", range = c(-3.8, 4)),
           yaxis=list(title=list(text="Slope (degrees)", standoff=11), type = "log", tickformat="f"),
           updatemenus=updatemenus, xaxis = list(type = "log"))
  
  p4 <- subplot(p1, p3, nrows=2, titleY = TRUE, titleX = TRUE, margin = 0.03)
  
  p4 <- p4 %>% config(displayModeBar = FALSE)
  p4$sizingPolicy$padding <- 40
  
  updatemenus2 <- list(
    list(
      active = 0,
      direction = "up",
      xanchor = 'center',
      yanchor = "bottom",
      pad = list('r'= 0, 't'= 10, 'b' = 10),
      x = 0.5,
      y = -0.1,
      buttons = list(
        list(
          label = "Forest loss vs. sparrow abundance",
          method = "update",
          args = list(list(visible = c(TRUE, TRUE, FALSE)))), 
        list(
          label = "Slope vs. sparrow abundance",
          method = "update",
          args = list(list(visible = c(TRUE, FALSE, TRUE)))
        )
      )
    ))
  
  p5 <- plot_ly(type = 'violin', box = list(visible = F), points = F, 
                showlegend=TRUE, hoverinfo="none", x = group, y = lat_SALS, 
                color = I(rgb(229/255, 133/255, 50/255, 1)), side = 'positive', name = "Sparrow abundance", height = 725, width = 300) 
  
  p5 <- p5 %>%
    add_trace(
      x = group2, y = lat_loss,  side = 'negative', color = I(rgb(85/255, 157/255, 63/255, 1)), name = "Forest loss") 
  
  p5 <- p5 %>%
    add_trace(
      x = group3, y = lat_slope,  side = 'negative', visible=FALSE, color = I(rgb(58/255, 119/255, 175/255, 1)), name = "Slope") 
  
  p5 <- p5 %>%
    layout(title = " ",
           xaxis=list(title=" ", tickfont = list(color='white', size=1)),
           yaxis=list(title=list(text="Latitude", standoff=25), tickformat="f"), updatemenus = updatemenus2, legend = list(x = 0, y = 100),
           margin=list(
             l=0,
             r=10,
             b=0,
             t=0,
             pad=0
           ))
  
  p5 <- p5 %>% config(displayModeBar = FALSE)
  p5$sizingPolicy$padding <- 40
  
  output$plot <- renderPlotly(p4)
  output$plot2 <- renderPlotly(p5)
}


shinyApp(ui,server)
