###################
# ui.R
# 
# UI controller. 
# Used to define the graphical aspects of the app.
###################


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("E. coli Prediction: Upper Santa Cruz River"),
  
  sidebarPanel(markdown("LAURA FIGURE OUT HOW TO FORMAT THIS
  
  This tool is intended to predict Escherichia coli (E. coli) levels
               in the Upper Santa Cruz River using a model trained on sampling data
               collected after the 2009 upgrade of the Nogales International Wastewater Treatment Plant.
               
               Variables were as follows:
               - PreviousTMin - Minimum temperature from 2 days prior from KA7WSB-1 weather station upload on ClimateAnalyzer.org
               Discharge_CFS - Current (within last 5 minute) discharge reading from the USGS gage 09481740,
               Stage - Categorical classification of current trend from USGS gage 09481740 (Low Flow, Base flow, High and Rising Flow, High and Falling Flow)),
               NinXTS - Current (or most recent) Oceanic Nino Index from NOAA
               TOD - Categorical quartile classification of time of day,
               DistFromSonoita - Categorical distance classification from the Rio Sonoita Inputs"
  )),
  mainPanel(
    
    fluidPage(
      h3("Current Time in Santa Cruz County:"),
      
      h4(textOutput("currentTime", container = span)
      ),
      h3("Current Conditions: "
      ),
      fluidRow(
        column(12,
               tableOutput('table')
        )),
      
      h3("Current Predictions: "
      ),
      leafletOutput("mymap")
    )
  )
)



