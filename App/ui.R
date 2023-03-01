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
  tabPanel("Map",
           h4("THIS APPLICATION IS STILL IN DEVELOPMENT", align = "center", style="color:red"),
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
tabPanel("Acknowledgements",
         br(),
         h4("Acknowledgements", align = "center"),
         br(),
          h6("Thanks to Christian Roman Palacios (School of Information, University of Arizona) 
            and Maliaca Oxnam (Data Science Institute, Univeristy of Arizona) for mentorship and assistance with 
            data analysis and application creation.", align = "center"), 
            
          h6("Thanks to Jennifer Duan (University of Arizona) and Erfan Tousi (NextGen Engineering Inc) for sharing their comparable
            study of irrigation water quality.", align = "center"),
            
          h6("Thanks to Salek Shafiqullah (National Park Service), Cheryl McIntyre (National Park Service), 
            Kara Raymond (National Park Service), Meghan Smart (Arizona Department of Environmental Quality), 
            Nicholas Paretti (United States Geological Survey) and Connie Williams (Friends of the Santa Cruz River)
            for their expertise on water quality and sharing collected coliform data.", align = "center"),
            
          h6("This study would not be possible from the data collection performed by: 
            National Park Service Sonoran Desert Network, Tumacácori National Historical Park, 
            U.S. Geological Survey, Friends of the Santa Cruz, and Arizona Department of Environmental Quality.", align = "center")
)
)



