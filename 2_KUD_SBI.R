
# TRY FIRST ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(sp)
library(adehabitatHR)

floor_test <- read_csv("1_filtered_detections_all.csv")

view(floor_test)

floor_test %>% 
summarise(min = min(Date),
          max = max(Date))

#SKip 0.1
floored <- floor_test %>% 
  group_by(Date = floor_date(Date, "day"), Loc,
           Lat, Lon) 


# Date = floor_date() *should* group data by DAY, which is what we want
# position average point per day

head(floored)

#To check # of detections / fish
Fish_detections <- floor_test %>%
       group_by(Year, ID, Loc) %>%
       #filter(Year %in% c(2019, 2020)) %>% 
       count(ID, sort = TRUE)
view(Fish_detections)

#An artifact of past troubleshooting. 
#I *think* all of these data conversions are necessary
#filter(Date > "2019-01-01) removes all previous dates, excludes 2018 data
#NOTE: kernalUD() fails if 2018 is excluded, but NOT if 2020 is excluded... weird. 

floored <- floor_test %>% 
  dplyr::mutate(ID = as.integer(ID)) %>% 
  dplyr::mutate(Loc = as.factor(Loc)) %>% 
  dplyr::mutate(Lat = as.numeric(Lat)) %>% 
  dplyr::mutate(Lon = as.numeric(Lon)) %>% 
  dplyr::select(ID, Loc, Lat, Lon, Date) %>%
  #filter(Date > "2019-01-01")

head(floored)

floored %>% 
  summarise(min = min(Date),
            max = max(Date))

#This is how adehabitatHR creates a spatialpointsDataFrame, which is critical
coordinates(floored) <- ~Lon+Lat

#You can ignore this section. skip tp kernelUD()
library(sf)
mcp_gsb <- mcp(floored[,1], percent = 90)
ggplot() + 
  geom_sf(data = st_as_sf(mcp_gsb)) + 
  geom_sf(data=st_as_sf(floored[,1]))

#This is the money right here: kernel utilization distribution for our fish
fish_ud <- kernelUD(floored[,1],
                    grid = 1000,
                    extent = 0.5,
                    same4all = TRUE)

image(fish_ud)

#Previous code did *not* need grid and extent arguments, now we do (i guess) 
# need same4all set to TRUE (FALSE is default)

#More troubleshooting
area <- kernel.area(fish_ud)
par(mar=c(1,1,1,1))
plot(area)

# These are were we're all hung up. Currently everything works on my end. 
# These provide an entimation of where an individual spets 75% and 95% of their time
#ver_all50 <- getverticeshr(fish_ud, 50)
ver_all75 <- getverticeshr(fish_ud, 75)
ver_all95 <- getverticeshr(fish_ud, 95)

#Visualize in base R
par(mar = rep(2, 4))
plot(ver_all95, col = "blue")
plot(ver_all75, col = "green", add = TRUE)

#Export fish_ud[,1] as a raster file (.tif in this case)
library(raster)

r <- raster(as(fish_ud[[1]], "SpatialPixelsDataFrame"))
writeRaster(r, filename = "SBI_KUD_NEW.tif")

#Export the vertices as shapefiles
library(rgdal)

writeOGR(obj=ver_all75, dsn="ver_all75",
         layer = "ver_all75",
         driver="ESRI Shapefile") # this is in geographical projection

writeOGR(obj=ver_all95, dsn="ver_all95",
         layer = "ver_all95",
         driver="ESRI Shapefile") # this is in geographical projection


# Maybe ignore IDK --------------------------------------------------------
library(tidyverse)
SBI_Data <- read_csv("1_filtered_detections_all.csv")
View(SBI_Data)

Fishpoint <- SBI_Data %>% 
  dplyr::mutate(ID = as.integer(ID)) %>% 
  dplyr::mutate(Loc = as.factor(Loc)) %>% 
  dplyr::mutate(Lat = as.numeric(Lat)) %>% 
  dplyr::mutate(Lon = as.numeric(Lon)) %>% 
  dplyr::select(ID, Loc, Lat, Lon, Date) 

floored <- floor_test %>% 
  group_by(Date = floor_date(Date, "day"), Loc,
           Lat, Lon) 

  filter(ID %in% c("9712", 
                   "9714",
                   "9719",
                   "9722") == F)
#This filter() call removes obs we DON'T want
#save <- c(9712, 1), c(9714, 0), c(9719, 0), c(9722, 0)

#For reference, these are the obs we DO want
#filter(ID %in% c("9716","9718","9720","9721","9723","16712","16715","16716")
library(sp)
coordinates(Fishpoint) <- ~Lat+Lon

library(adehabitatHR)
#NOTE: tibble column order is VERY important for kernelUD to work
#Order: ID, Loc, Lat, Lon, Date

ud <- kernelUD(Fishpoint[,1],
               same4all = TRUE)
image(ud)

#ver <- getverticeshr(ud[["9720"]], 95)

#Get utilization distributions at 50% and 95%
ver_all95 <- getverticeshr(ud, 95)
ver_all50 <- getverticeshr(ud, 50)
ver_all75 <- getverticeshr(ud, 75)

par(mar = rep(2, 4))
plot(ver_all95, col = "blue")
plot(ver_all75, col = "green", add = TRUE)
plot(ver_all50, col = "red", add = TRUE)

# First: make a SpatialPointsDataFrame ------------------------------------

SBI_Data <- read_csv("1_filtered_detections_all.csv")
View(SBI_Data)

SP_gsb <- SBI_Data %>% 
  dplyr::mutate(ID = as.integer(ID)) %>% 
  dplyr::mutate(Loc = as.factor(Loc)) %>% 
  dplyr::mutate(Lat = as.numeric(Lat)) %>% 
  dplyr::mutate(Lon = as.numeric(Lon)) %>% 
  dplyr::select(ID, Loc, Lat, Lon, Date)

view(SP_gsb)
library(sp)
#This is how the SpatialPointsDataFrame is created:
coordinates(SP_gsb) <- ~Lat+Lon


# Then, make and plot KUDs in adehabitHR -------------------------------------------------------

library(adehabitatHR)
#NOTE: tibble column order is VERY important for kernelUD to work
#Order: ID, Loc, Lat, Lon, Date

ud <- kernelUD(SP_gsb[,1],
               same4all = TRUE)
image(ud)

#ver <- getverticeshr(ud[["9720"]], 95)

#Get utilization distributions at 50% and 95%
ver_all95 <- getverticeshr(ud, 95)
ver_all50 <- getverticeshr(ud, 50)

par(mar = rep(2, 4))
plot(ver_all95, col=rainbow(4))
plot(ver_all50, col=rainbow(4))


# #Export "ud" as a raster, ver_all95 and ver_all50 as shape files --------
library(raster)

r <- raster(as(fish_ud[[1]], "SpatialPixelsDataFrame"))
writeRaster(r, filename = "SBI_KUD.tif")

library(rgdal)

writeOGR(obj=ver_all75, dsn="ver_all75",
         layer = "ver_all75",
         driver="ESRI Shapefile") # this is in geographical projection

writeOGR(obj=ver_all95, dsn="ver_all95",
         layer = "ver_all95",
         driver="ESRI Shapefile") # this is in geographical projection


# A little something to add to the TABLE ----------------------------------

#We wanted to add the first and last time each individual was detected in the array
View(floor_test %>% 
  select(ID, Date) %>%
  group_by(ID) %>% 
  summarise(Date = range(Date), .groups = "drop"))

# #Extract pixel values, potential troubleshooting ------------------------
ud1 <- raster(as(ud[[1]], "SpatialPixelsDataFrame"))
class(ud1) 
plot(ud1)


extract(ud1, SP_gsb[,1])

vud <- getvolumeUD(ud)
vud


#MCHu <-  mean convex hull
SP_gsbdf <- as.data.frame(SP_gsb)

#Relocation locations(?)
plot(SP_gsb, col=as.numeric(SP_gsbdf[,1]))

nn <- LoCoH.k(SP_gsb[,1], k=12)
plot(nn, border=NA)


