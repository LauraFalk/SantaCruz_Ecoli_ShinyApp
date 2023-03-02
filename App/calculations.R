###################
# calculations.R
# 
# calculations controller. 
# Used to define the back-end aspects of the app.
###################

##Dist from Sonoita is within the mapping layer
library(dplyr)

spatiallocs <- read_xls("Data/SantaCruzLocs.xls")
spatiallocs <- spatiallocs %>%
  arrange(DistCatego)

##Retrieve all variables using the functions
predictionDF <- as.data.frame(spatiallocs)
predictionDF$PreviousTmin <- c(Var_TMin)
predictionDF$Discharge_CFS	<- c(Var_Discharge_CFS)
predictionDF$Stage	<- c(Var_Stage)
predictionDF$NinXTS	<- c(Var_NinXTS)
predictionDF$TOD <- c(Var_TOD)

predictionDF <- predictionDF %>%
  rename(DistFromSonoita = DistCatego) %>%
  select(PreviousTmin, Discharge_CFS, Stage, NinXTS, TOD, DistFromSonoita)


DisplayDF <- predictionDF %>%
  select(-DistFromSonoita,-TOD)%>%
  distinct() %>%
  rename(Previous_Air_Temperature_Celsius = PreviousTmin,
         River_Discharge_CFS = Discharge_CFS,
         River_Stage_Category = Stage,
         El_Nino_Score = NinXTS) %>%
  mutate(River_Stage_Category = ifelse(River_Stage_Category == 1, "Low Flow",
                (ifelse(River_Stage_Category ==2,"Base Flow", 
                 ifelse(River_Stage_Category == 3, "High Flow and Increasing",
                        "High Flow and Decreasing")))))

## Run the model for 235
XGBModel <- xgb.load('Data/XGBmodel235')
predictionDM <- data.matrix(predictionDF)
pred <- predict(XGBModel,predictionDM)
pred <-  as.numeric(pred > 0.45)
spatiallocs$pred235 <- c(pred)
#spatiallocs$pred235 <- ifelse(spatiallocs$pred235 > 0, "Bacteria Level >235  Likely", "High Bacteria levels > 235 not predicted")


## Run the model for 575
XGBModel <- xgb.load('Data/XGBmodel575')
pred <- predict(XGBModel,predictionDM)
pred <-  as.numeric(pred > 0.45)
spatiallocs$pred575 <- c(pred)
#spatiallocs$pred575 <- ifelse(spatiallocs$pred575 > 0, "Bacteria Level >575 Likely", "High Bacteria levels > 575 not predicted")

## For map
points <- spatiallocs %>%
  select(Lat,Long,DistCatego,pred235, pred575) %>%
  mutate(pointcolor = (pred235+pred575*3)) %>%
  mutate(pointlegend = ifelse(pointcolor > 1 ,">575 MPN likely",(ifelse(pointcolor==1,">235 MPN likely","<235 MPN likely")))) 
