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
  
  ## Read in shapefile and tidy to a spatial data frame for use with ggplot
  # shapefile map data
  philsmap<-readOGR(dsn = 'voi/Provinces', layer = 'Provinces', stringsAsFactors = F)

# lookup table with provinces and ids for joining
lktable<-data.frame(municipality=philsmap@data$NAME_1, id=philsmap@data$ID_1)

# convert spatial data frame to a format for ggplot
philsmap<-tidy(philsmap, region = NULL)

# make blank map of Philippines
blank_map<-philsmap %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill='white')

## Summary calculations to join with spatial data frame

# list of tuna stocks to exclude
tunas<-c('Bigeye tuna (Tambakol/ Bariles)',
         'Eastern little tuna (Bonito)',
         'Frigate tuna (Tulingan)',
         'Skipjack (Gulyasan)',
         'Yellowfin tuna (Tambakol/Bariles)')

# 2014 values - Tuna excluded
calc14<-final %>%
  filter(year==2014 & !(species %in% tunas)) %>%
  group_by(municipality) %>%
  summarize(
    totalcatch14     =  sum(catch,na.rm=T),
    logtotalcatch14  =  log10(totalcatch14),
    meanprice14      =  mean(priceperton,na.rm=T)) %>%
  right_join(lktable) %>%
  mutate(id=as.character(id)) %>%
  dplyr::select(-municipality)

# join calc14 with 2014 calculations
philsmap<-left_join(philsmap,calc14)

# percent changes in catch since 2008 - Tuna included
perc09<-final %>%
  filter(year>2009 & !(species %in% tunas)) %>%
  group_by(municipality,species) %>%
  mutate(
    priorcatch = lag(catch),
    percchange = 100*((catch-priorcatch)/priorcatch)) %>%
  group_by(municipality) %>%
  summarize(avgchange=mean(percchange,na.rm=T)) %>%
  right_join(lktable) %>%
  mutate(id=as.character(id)) %>%
  dplyr::select(-municipality)

# join percentage calculations with map data
philsmap<-left_join(philsmap, perc09)

plotA<-philsmap %>%
  ggplot(aes(x = long, y = lat, group = group, fill = logtotalcatch14)) +
  geom_polygon()

plotB<-philsmap %>%
  ggplot(aes(x = long, y = lat, group = group, fill = percchange)) +
  geom_polygon()

ggsave(file='notunaperc.pdf')
