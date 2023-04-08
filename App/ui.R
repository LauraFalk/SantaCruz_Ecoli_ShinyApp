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
  div(h6(em("E. coli"), "prediction", style="margin: 0;"), h6('Upper Santa Cruz River', style="margin: 0;")),
  tabPanel("Map",
           h4("Project Description", align = "center"
           ),
          "This tool predicts", em("Escherichia coli (E. coli)"), "levels
               in the Upper Santa Cruz River using a model trained on public data from 2009-2022.",
          br(),
          "It is intended as a warning of possible high bacterial loads; water quality
          must still be verified using coliform testing procedures to ensure public safety.",
          br(),
          br(),
      h4("Current Conditions:", align = "center"
      ),
      fluidRow(
        column(12, align="center",
               tableOutput('table')
        )),
      em("For a more in-depth description of variables, please see the variables tab."),
      br(),
      br(),
      h4("Current predictions (", textOutput("currentTime", container = span), "):", align = "center"
      ),

      leaflet::leafletOutput("mymap"),
      "Predictions are measured in MPN (Most Probable Number).", align = "center",
      br(),
      br(),
      br(),
      "Please report any bugs to the", tags$a("GitHub Issue Tracker", href="https://github.com/LauraFalk/SantaCruz_Ecoli_ShinyApp/issues")
      
),
tabPanel("Variables",
         h4("The variables used for model training and prediction are as follows:"),
         br(),
         h6("E. coli levels"),
         "Data were gathered from public data sources (National Park Service and Storet water quality portal). All data follows",
         tags$a("EPA Standard Method 9223B", href="https://www.nemi.gov/methods/method_summary/5583/"),
         "testing procedures.",
         h6("Previous_Minimum_Air_Temperature_Celsius"),
         "Minimum temperature from two days prior to current time from KA7WSB-1 
                     weather station upload on ",
                     tags$a("ClimateAnalyzer.org", href="http://www.climateanalyzer.us/raws/KA7WSB-1"),
                     ". The two day lag is due to
                     a two day publishing delay online. Model was traineed accordingly to 
                     account for the lag time in the current predictions.",
         h6("River_Discharge_CFS"),
         "Current (within last 15 minute) discharge 
                     reading in cubic feet per second from the",
         tags$a("USGS Gage 09481740",
                href="https://waterdata.usgs.gov/monitoring-location/09481740/#parameterCode=00060&period=P7D"),".",
         h6("River_Stage_Category"),
         "Categorical classification of current trend from", 
         tags$a("USGS Gage 09481740",
                href="https://waterdata.usgs.gov/monitoring-location/09481740/#parameterCode=00060&period=P7D"),
                " (Low Flow, Base flow, High and Rising Flow, 
                     High and Falling Flow)). Categories are based on quantile calculation from the training dataset. (determines low, base or high) as well as difference
                     from previous value in the cases of high flow (determines rise or fall).",
         h6("El_Nino_Score"),
         "Current (or most recent, due to monthly updates) of
                     the ",
         tags$a("Oceanic Nino Index from NOAA",
                href="https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php"),
         ". See website for further information on
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
         "This study would not be possible from the data collection performed by:",
         tags$a("National Park Service - Sonoran Desert Network",
                href="https://www.nps.gov/im/sodn/index.htm"),
         ",",
         tags$a("Tumacaori National Historic Park",
                href="https://www.nps.gov/tuma/index.htm"),
         ",",
         tags$a("United States Geological Survey",
                href="https://www.usgs.gov/"),
         ",",
         tags$a("Friends of the Santa Cruz River",
                href="https://friendsofsantacruzriver.org/"),
         ", and ",
         tags$a("Arizona Department of Environmental Quality",
                href="https://azdeq.gov/"),
            ".",
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



