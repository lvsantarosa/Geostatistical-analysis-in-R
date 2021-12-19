################ RIMAS wells Geostatistical Analysis ##############

     ################ Working with data set ################

#Temporal data set

RIMAS_TMP = read.csv("RIMAS_NA.csv")

#Spatial data set

RIMAS_SP = read.csv("RIMAS_ID.csv")

#Transform data set from daily to monthly

library(tidyverse)

#Transform in tible
RIMAS_TMP <- as_tibble(RIMAS_TMP)

#Transform character in date
RIMAS_TMP$Date <- lubridate::as_date(RIMAS_TMP$Date,  format = '%m/%d/%Y') 

#Create cokumns of year, month, and day
RIMAS_TMP_YMD <- RIMAS_TMP %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))

#Resume data by mean using month and year
RIMAS_month <- RIMAS_TMP_YMD %>% group_by(year, month) %>%
  summarise_at(vars(PIN:ALT), mean, na.rm = TRUE)


#Convert to longer 
RIMAS_month_pivot <- RIMAS_month %>% 
  pivot_longer(cols = PIN:ALT, names_to = "well", values_to = "WatLev")


#Join to add coordinates e organizar por data
RIMAS_merge <- RIMAS_month_pivot %>%  merge(RIMAS_SP, by  = "well") %>% 
  arrange(year, well, month)
  
RIMAS_merge <-  within(RIMAS_merge, Date <- sprintf("%d-%02d", year, month))

RIMAS_merge <- select(RIMAS_merge, -ne, -idt_ponto, -year, -month)


#Convert to wider
RIMAS_GE <- pivot_wider(RIMAS_merge, names_from = Date, values_from = WatLev)

##########################################################################
      ########################Geostatistics######################

library(gstat)
library(sp)
library(automap)

coordinates(RIMAS_GE) = ~ POINT_X+POINT_Y

#Map visualization 

library(mapview)

proj4string(RIMAS_GE)=CRS("+init=epsg:4326")

mapview(RIMAS_GE, zcol = c("well"), legend = TRUE)

#mapview(RIMAS_merge, zcol = c("NA"), legend = TRUE, map.types = "Esri.WorldImagery")

RIMAS_GE <- na.omit(RIMAS_GE)

#Variogram

ve <- variogram(RIMAS_GE$`2016-05` ~ POINT_X+POINT_Y, data=RIMAS_GE, cutoff =150, width = 35)
plot(ve, pl = T)

