library(tidyverse)
library(here)

#Import the entire VUE export, don't worry about parsing errors
VUE_export <- read_csv(here::here("Data_pre-processing", "VUE_detect_all.csv"))
View(VUE_export)

#Drop unnecessary columns, rename Transmitter to ID, remove the redundant tag info to help with merging
VUE_export <- VUE_export %>% 
  dplyr::select(`Date and Time (UTC)`,Transmitter, Receiver,
                `Station Name`) %>% 
  rename(ID = Transmitter) %>%
  mutate(ID = str_sub(ID, 10, -1)) 

#This last mutate() uses str_sub() to remove characters (column, start, end)

View(VUE_export)
#Import VR2W Site Information (coordinates for receiver locations)
Sites_all <- read_csv(here::here("Data_pre-processing","GSBsites.csv"))
View(Sites_all)

#Drop unnecessary columns
Sites_all <- Sites_all %>% 
  dplyr::select(`Site`, `Lat_dd`, `Lon_dd`) %>% 
  rename(`Station Name` = Site)

#Import GSB tagging data (from notebook)
library(readxl)
Tagged_GSB <- read_excel("./Data_pre-processing/Tagged GSB (In-House).xlsx", sheet=2)
View(Tagged_GSB)

#Drop unnecessary columns
#NOTE: the last 5 individuals were tagged in OCT 2020, they should NOT appear in subsequent analysis
Tagged_GSB <- Tagged_GSB %>% 
  dplyr::select(`ID`) %>% 
  mutate_if(is.double, as.character)

#Merge "Sites_all" and "Vue_export" by `Station Name` for correct station coordinates
# ~1400 rows dropped because they did not have an associated `Station Name`
#Join ID (individual tags) in "Tagged_GSB" with "SBI_data" to complete the data sheet
# ~9000 rows dropped because they did not contain our fish
#NOTE: Three (3) individuals tagged in OCT 2020 were (apparently) detected 
SBI_data <- merge(VUE_export, Sites_all, by = "Station Name") %>%
  dplyr::semi_join(Tagged_GSB, SBI_data, by = "ID") %>% 
  as_tibble()
View(SBI_data)

#Double check the number of detections:
SBI_data %>% 
  count(ID, sort = TRUE)

#Now should have a tibble with all desired columns (pat yourself on the back)

#Use the lubridate package to split the `Date and Time (UTC)` column into to more workable forms
library(lubridate)

SBI_data <- SBI_data %>% 
  rename(Date = `Date and Time (UTC)`) %>%
  mutate(Year = year(Date)) %>% 
  mutate(Month = month(Date)) %>%
  mutate(Day = day(Date)) %>% 
  mutate(Hour = hour(Date))
View(SBI_data)

#Let's clean up the tibble, then export it as a CSV (to be universally readable)

SBI_data <- SBI_data %>% 
  rename(Loc = `Station Name`) %>% 
  rename(Lat = Lat_dd) %>% 
  rename(Lon = Lon_dd) %>% 
  relocate(Loc, .after = Receiver) %>% 
  relocate(Date, .after = Lon)
  
write.csv(SBI_data, "1_filtered_detections_all.csv")

#Now, get to work analyzing these data!

