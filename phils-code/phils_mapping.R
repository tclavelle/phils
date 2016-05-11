########################################################################
##
## Philippines map analysis 
##
## By Tyler Clavelle
########################################################################

###########################################################################################################
### Load packages and read data  -------------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(rgdal)
library(rgeos)
library(broom)
library(readr)

# shapefile map data
philsmap<-readOGR(dsn = '../../Google Drive/Project Data/phils-data/Provinces', layer = 'Provinces', stringsAsFactors = F)
munimap<-readOGR(dsn = '../../Google Drive/Project Data/phils-data/MuniCities', layer = 'MuniCities', stringsAsFactors = F)

# read data of TURF-able municipalities
# muniturf<-read_excel(path = 'voi/phils-data-antonius/MWD_Table.xlsx', sheet = 2)

###########################################################################################################
### Maps of Results  -------------------------------------------------------------------------
###########################################################################################################

# lookup table with provinces and municipal ids for joining
lktable<-data.frame(province_name=philsmap@data$NAME_1, id=philsmap@data$ID_1)
lktable2<-data.frame(Municipality=munimap@data$NAME_2, id=munimap@data$ID_2)

# convert spatial data frame to a format for ggplot
philsmap<-tidy(philsmap, region = NULL)

# adjust lookup table to account for ERROR with fortify that changes polygon id numbers
lktable<-lktable %>%
  mutate(id            = as.character(lag(id)), 
         province_name = toupper(province_name)) # lag id numbers to account for the 0 added by the tidy function

lktable[1,2]<-"0" # replace NA for first polygon after lagging

# join with region list
lktable<-read_csv(file = '../../Google Drive/Project Data/phils-data/phils_fisheries_regions.csv') %>%
  distinct(REGION_NAME, PROVINCE_NAME) %>%
  select(PROVINCE_NAME, REGION_NAME) %>%
  rename(province_name = PROVINCE_NAME,
         region_name   = REGION_NAME) %>%
  left_join(lktable)

# region results 
region_results %>%
  filter(year == 2014) %>%
  right_join(lktable) %>%
  left_join(philsmap) %>%
  # ggplot(aes(x=long, y=lat, fill = medianb_nat)) + DON"T PLOT THIS - theres an error with the join and the plot will blow up computer
  # geom_polygon()
