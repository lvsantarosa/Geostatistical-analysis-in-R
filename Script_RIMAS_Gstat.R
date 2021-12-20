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
RIMAS_year <- RIMAS_TMP_YMD %>% group_by(year) %>%
  summarise_at(vars(PIN:ALT), mean, na.rm = TRUE)


#Convert to longer 
RIMAS_year_pivot <- RIMAS_year %>% 
  pivot_longer(cols = PIN:ALT, names_to = "well", values_to = "WatLev")


#Join to add coordinates e organizar por data
RIMAS_merge <- RIMAS_year_pivot %>%  merge(RIMAS_SP, by  = "well") %>% 
  arrange(year, well)

RIMAS_merge <-  within(RIMAS_merge, Date <- sprintf("%d", year))

RIMAS_merge <- select(RIMAS_merge, -ne, -idt_ponto, -year)

RIMAS_merge <- RIMAS_merge[-131:-143,]

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

mapview(RIMAS_GE, zcol = c("2016"), legend = TRUE)

#mapview(RIMAS_merge, zcol = c("NA"), legend = TRUE, map.types = "Esri.WorldImagery")

#Variogram

#experimental
ve <- variogram(RIMAS_GE$`2016` ~ 1, data=RIMAS_GE, cutoff =150, width = 35)
plot(ve, plot.numbers = T, asp=1)

#theoretic
vt <- vgm(psill = 150, model = "Sph", range = 150,nugget = 30) 
plot(ve, pl = T, model = vt)

#fit
va <- fit.variogram(ve, vt) 
va
plot(ve, pl = T, model = va)

#autofit
variog = autofitVariogram(RIMAS_GE$`2016` ~ 1, RIMAS_GE)
plot(variog)

#Create a Grid
library(geoR)

grid_F <- pred_grid(c(-52.245, -48.167), c(-22.60611, -20.34111), by=0.01)
names(grid_F) <- c("POINT_X", "POINT_Y")
coordinates(grid_F) <- c("POINT_X", "POINT_Y")
gridded (grid_F) <- T
proj4string(grid_F)=CRS("+init=epsg:4326")

#plot(grid_F, pch = 3, cex = 0.2)

#kriging

#ordinary kriging (OK)
ok <- krige(RIMAS_GE$`2016` ~ 1, RIMAS_GE,grid_F, model = vt) 

#print
print(spplot(ok, "var1.pred", asp=1, col.regions=rev(heat.colors(50)),
             main="Oridinary Kriging, GW depth (m)"), )

print(spplot(ok, "var1.var", asp=1, col.regions=rev(heat.colors(50)),
             main="Variance"))

# cross validation

krige.cv(RIMAS_GE$`2016` ~ 1, RIMAS_GE, model = va)

# Mean prediction error, close to zero better:
mean(residual$val)

# Mean square prediction error, smaller the better
mean(residual$val^2)

# Mean square normalized error, close to 1 best
mean(val$zscore**2)

val$predicted <-val$observed - val$residual

# Linear correlation between observed and predicted values
cor(val$observed, val$predicted)

# Linear correlation between observed and predicted values
plot (val$observed, val$predicted, xlab = "Observed", ylab = "Predicted") 


