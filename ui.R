###################
# ui.R
# 
# UI controller. 
# Used to define the graphical aspects of the app.
###################


# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "litera"), #https://bootswatch.com/
  # Application title
  div(h3(em("E. coli"), "prediction", style="margin: 0;"), h6('Upper Santa Cruz River', style="margin: 0;")),
  tabPanel("Welcome",
           br(),
           h4("Welcome!", align = "center"),
           br(),
           h6("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Est pellentesque elit ullamcorper dignissim cras. Mauris pellentesque pulvinar pellentesque habitant morbi tristique senectus. Dui nunc mattis enim ut. In iaculis nunc sed augue lacus viverra vitae congue eu. Morbi tristique senectus et netus et. Ultrices eros in cursus turpis massa tincidunt dui ut. Turpis nunc eget lorem dolor sed viverra ipsum nunc. Facilisis sed odio morbi quis. Amet massa vitae tortor condimentum lacinia quis vel eros donec. Non quam lacus suspendisse faucibus interdum posuere. Tempus iaculis urna id volutpat lacus laoreet non curabitur. Pharetra massa massa ultricies mi quis hendrerit dolor magna. Faucibus et molestie ac feugiat sed lectus vestibulum mattis. Elementum nisi quis eleifend quam adipiscing. Accumsan tortor posuere ac ut. A lacus vestibulum sed arcu non odio euismod lacinia.", align = "center"),
  ),
  tabPanel("Map",
          "This tool is intended to predict Escherichia coli (E. coli) levels
               in the Upper Santa Cruz River using a model trained on sampling data
               collected after the 2009 upgrade of the Nogales International Wastewater Treatment Plant.",
          br(), br(),
          "Variables were as follows:",
          tags$div(
            tags$ul(
              tags$li("PreviousTMin - Minimum temperature from 2 days prior from KA7WSB-1 weather station upload on ClimateAnalyzer.org"),
              tags$li("Discharge_CFS - Current (within last 5 minute) discharge reading from the USGS gage 09481740,"),
              tags$li("Stage - Categorical classification of current trend from USGS gage 09481740 (Low Flow, Base flow, High and Rising Flow, High and Falling Flow))"),
              tags$li("NinXTS - Current (or most recent) Oceanic Nino Index from NOAA"),
              tags$li("TOD - Categorical quartile classification of time of day,"),
              tags$li("DistFromSonoita - Categorical distance classification from the Rio Sonoita Inputs"),
              )
          ),
      h3("Current Conditions:", align = "center"
      ),
      fluidRow(
        column(12, align="center",
               tableOutput('table')
        )),
      
      h3("Current predictions (", textOutput("currentTime", container = span), "):", align = "center"
      ),
      leaflet::leafletOutput("mymap")
),
tabPanel("About us",
         br(),
         h4("About us", align = "center"),
         br(),
         h6("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Laoreet suspendisse interdum consectetur libero. Sollicitudin aliquam ultrices sagittis orci a scelerisque purus semper. Integer feugiat scelerisque varius morbi enim nunc faucibus a pellentesque. Ultrices sagittis orci a scelerisque. Aliquam etiam erat velit scelerisque in dictum non consectetur. Vestibulum lectus mauris ultrices eros in cursus turpis massa tincidunt. Pharetra magna ac placerat vestibulum lectus mauris ultrices eros. Varius quam quisque id diam vel quam elementum. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor aliquam. Varius quam quisque id diam vel quam elementum pulvinar. Fermentum posuere urna nec tincidunt.", align = "center"),
)
)



