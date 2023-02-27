###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$table <- renderTable(DisplayDF)
  
  #map
  
  output$mymap <- renderLeaflet({
    leaflet(points) %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        color = ifelse(points$pointcolor > 1 ,"red",(ifelse(points$pointcolor==1,"yellow","navy"))) ,
        stroke = FALSE, fillOpacity = 0.6,
        label = points$pointlegend
      ) %>%
      addLegend(values = c("red", "yellow", "navy"), 
                colors = c("red", "yellow", "navy"), 
                labels = c("Does Not Meet Partial Body Contact Standard (>575 MPN)",
                           "Does Not Meet Full Body Contact Standard (>235 MPN)",
                           "Meets Arizona Body Contact Standards <235 MPN"), 
                position = 'bottomright')
    
    
    
  })
  output$currentTime <- renderText({
    format(sysDate1) 
  })
}
