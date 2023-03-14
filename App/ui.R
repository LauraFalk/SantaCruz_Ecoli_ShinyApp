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
               in the Upper Santa Cruz River. Predictions are based off of public data sources
          gathered from 2009-2022.",
          br(),br(),
          "While it may serve as a first warning of high bacterial loads likelihood; water quality
          must still be verified using coliform testing procedures to ensure public safety.",
          br(),
      h3("Current Conditions:", align = "center"
      ),
      fluidRow(
        column(12, align="center",
               tableOutput('table')
        )),
      "For a more in-depth description of variables, please see the variables tab.",
      
      h3("Current predictions (", textOutput("currentTime", container = span), "):", align = "center"
      ),
      leaflet::leafletOutput("mymap"),
      "Predictions are measured in MPN (Most Probable Number) and based on the EPA SM9223 method of E. coli measurement.", align = "center"
      
),
tabPanel("Variables",
         h4("The variables used for model training and prediction are as follows:"),
         br(),
         h6("Previous_Minimum_Air_Temperature_Celsius"),
         "Minimum temperature from two days prior to current time from KA7WSB-1 
                     weather station upload on ClimateAnalyzer.org. The two day lag is due to
                     a two day publishing delay online. Model was traineed accordingly to 
                     account for the lag time in the current predictions.",
         h6("River_Discharge_CFS"),
         "Current (within last 15 minute) discharge 
                     reading in cubic feet per second from the USGS gage 09481740.",
         h6("River_Stage_Category"),
         "Categorical classification of current trend from 
                     USGS gage 09481740 (Low Flow, Base flow, High and Rising Flow, 
                     High and Falling Flow)). Categories are created using quantiles
                     of the month's data (determines low, base or high) as well as difference
                     from previous value in the cases of high flow (determines rise or fall).",
         h6("El_Nino_Score"),
         "Current (or most recent, due to monthly updates) of
                     the Oceanic Nino Index from NOAA. See website for further information on
                     this calculated value.",
         h6("Time_Of_Day"),
         "Categorical quartile classification of time of day (Morning, Afternoon, Evening, Night).",
         h6("Distance_From_Sonoita"),
         "Distance classification (roughly rounded to the nearest 1000m) from the Rio Sonoita Inputs, used for mapping."
         ),

tabPanel("Acknowledgements",
         br(),
         h4("Acknowledgements", align = "center"),
         br(),
         "This study would not be possible from the data collection performed by: 
            National Park Service Sonoran Desert Network, Tumacacori National Historical Park, 
            U.S. Geological Survey, Friends of the Santa Cruz, and Arizona Department of Environmental Quality.",
         br(),br(),
         "Thanks to Christian Roman Palacios (School of Information, University of Arizona) 
            and Maliaca Oxnam (Data Science Institute, Univeristy of Arizona) for continuous mentorship, assistance and support with 
            data analysis and application creation.",
         br(),br(),
         "Thanks to Jennifer Duan (University of Arizona) and Erfan Tousi (NextGen Engineering Inc) for sharing their completed work,",
         tags$a(href="https://www.sciencedirect.com/science/article/abs/pii/S004896972104359X", "Evaluation of E. coli in sediment for assessing 
                irrigation water quality using machine learning,"),
         " from which the methods for this study were developed.",
         br(),br(),
         "Thanks to Salek Shafiqullah (National Park Service), Cheryl McIntyre (National Park Service), 
            Kara Raymond (National Park Service), Meghan Smart (Arizona Department of Environmental Quality), 
            Nicholas Paretti (United States Geological Survey) and Connie Williams (Friends of the Santa Cruz River)
            for their expertise on water quality and sharing collected coliform data."
         
))



