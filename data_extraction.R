# Extract water quality data from eagle.IO
#   By: Cameron Roberts

# The WQI package provides API wrappers and look-up tables to better facilitate data extraction from an Eagle.IO account housing the desired datasets (key required) 
#devtools::install_github("https://github.com/UncleCamsWaterPlans/WQI")
library(WQI)
library(tidyverse)


#Constants
Start <- "2023-01-01T00:00:00Z"
End <- "2024-01-01T00:00:00Z"
APIKEY <- keyring::key_get("EIO_KEY")

# QC codes::
GOOD <- c(1:100,1000:1999, 2012)
UNCERTAIN <- c(101:200,2000:2999)
BAD <- c(201:301,3000:3999)

# Herbert site names:
vec <- c("Herbert River at Nash's Crossing",
         "Broadwater Creek at Day Use",
         "Elphinstone Creek at Copley Road",
         "Stone River at Running Creek",
         "Stone River at Venables Crossing",
         "Lannercost Creek at Lannercost Ext Road",
         "Ripple Creek at Gangemis Road",
         "Ripple Creek at Seymour Creek Gates",
         "Trebonne Creek at Bruce Highway",
         "Francis Creek at Weir",
         "Catherina Creek at Catherina Creek Road",
         "Lagoon Creek at Five Mile Road",
         "Palm Creek at Cemetery Road",
         "Palm Creek at Bosworths Road",
         "Waterview Creek at Jourama Road",
         "Waterview Creek at Mammarellas Road",
         "Herbert River at John Row Bridge"
)

# build a list of the desired sites to extract data:
region <- WQI::reportableParamRef %>%
  filter(grepl(116, substr(GSnum,1,3)),
         GSnum != "116001F")
region$name[region$name == "1160131 - WCM - Waterview Creek at Mammarellas Rd"] = "1160131 - WCM - Waterview Creek at Mammarellas Road"




datalist = list()
for (i in 1:dim(region)) { 
  Site <- region$GSnum[i]
  name <- region$name[i]
  
  #Identify NodeID's for LOGGER values
  
  repdex <- reportableParamRef %>%
    dplyr::filter(GSnum == Site)
  
  #Desired variables:
  NNO3 <- repdex$`N-NO3`
  Nitrate <- WQI::EIO_Hist(APIKEY, param = NNO3, START = Start, END = End)
  Nitrate <- Nitrate %>% 
    filter(Quality %in% GOOD)
  
  Nitrate <- Nitrate %>% 
    add_column(GSnum = Site, .before = "ts") %>% 
    add_column(name = name, .before = "ts")
  
  datalist[[i]] <- Nitrate
  
}

catchment = do.call(rbind, datalist)

# remove out of specification values
catchment <- catchment %>% 
  filter(Value > 0.0075) # half minimum probe output

catchment$name <- gsub("(.*-\\s*(.*$))", "\\2", catchment$name)

write_csv(catchment, file="catchment_NNO3.csv")
