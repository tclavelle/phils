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
library(stringr)

# shapefile map data
philsmap<-readOGR(dsn = '../../Google Drive/Project Data/phils-data/Provinces', layer = 'Provinces', stringsAsFactors = F)
munimap<-readOGR(dsn = '../../Google Drive/Project Data/phils-data/MuniCities', layer = 'MuniCities', stringsAsFactors = F)

# read municipalities data
muniturf<-read_excel(path = '../../Google Drive/Project Data/phils-data/phils-data-antonius/MWD_Table.xlsx', sheet = 2) 

muniturf$concatenate <- paste(muniturf$`REGION NAME`, muniturf$`PROVINCE NAME`, muniturf$`MUNICIPAL NAME`, sep = '')

# current Rare municipalities
# cohorts <- read_excel(('../neda/neda-data/sites_philippines.xlsx'), col_names = 'MUNICIPAL NAME') %>%
#   mutate(`MUNICIPAL NAME` = toupper(`MUNICIPAL NAME`))

sites <- read_csv(file = '../neda/neda-data/rare_sites.csv')

# Identify sites that are previously Rare sites
muniturf <- muniturf %>%
  left_join(sites) %>%
  filter(`Past work Rare` == 'y') %>%
  select(`MUNICIPAL NAME`, `PROVINCE NAME`, `Past work Rare`) %>%
  right_join(muniturf)

###########################################################################################################
### Compile Map Lookup tables  -------------------------------------------------------------------------
###########################################################################################################

###########################################################################################################
### Provincial

# lookup table with provinces ids for joining
lktable<-data.frame(province_name=philsmap@data$NAME_1, id=philsmap@data$ID_1)

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

###########################################################################################################
### Municipal

# Order of municipalities and provinces in shapefile and corresponding polygon id number
ms <- toupper(munimap@data$NAME_2)
prvs <- toupper(munimap@data$NAME_1)

lktable2 <- data_frame(ms, prvs, 'id' = as.character(c(0:(length(ms)-1)))) %>%
  rename(`MUNICIPAL NAME` = ms,
         `PROVINCE NAME`  = prvs)

# find ids for current cohort sites
# cohorts <- rare_sites %>%
#   left_join(lktable2)

### Normalize names
# Replace Capitals
muniturf$`MUNICIPAL NAME` <- gsub(muniturf$`MUNICIPAL NAME`, pattern = ' (Capital)', replacement = '', fixed = T)

# unique municipality names in Rare's dataset (to use for normalizing names)
rare_munis <- unique(muniturf$`MUNICIPAL NAME`)
need_match_rare <- unique(muniturf$`MUNICIPAL NAME`)[!(unique(muniturf$`MUNICIPAL NAME`) %in% lktable2$`MUNICIPAL NAME`)]


# muniturf$`MUNICIPAL NAME` <- gsub(muniturf$`MUNICIPAL NAME`, pattern = ' \\([[:word:]]\\)', replacement = '', perl = T)

# for loop to replace city municipalities
city <- unique(need_match_rare[grepl(need_match_rare,pattern = '^CITY OF')])

for(b in 1:length(city)) {
  old <- city[b]
  new <- gsub(old, pattern = 'CITY OF ', replacement =  '')
  new <- paste(new, ' CITY', sep = '')
  muniturf$`MUNICIPAL NAME`[muniturf$`MUNICIPAL NAME`==old] <- new
}

map_df <- left_join(lktable2, muniturf) 

###########################################################################################################
### Map Results  -------------------------------------------------------------------------
###########################################################################################################

# convert spatial data frame to a format for ggplot
phils_map <- tidy(philsmap)
muni_map <- tidy(munimap)

# join dataframe with municipal data
muni_map <- left_join(muni_map, map_df)

###########################################################################################################
### Maps 

coral_map <- ggplot(muni_map, aes(x = long, y = lat, group = group, fill = as.character(CORALREEFS))) +
  geom_polygon() +
  coord_equal(ratio = 1) +
  guides(fill = F) +
  scale_fill_manual(na.value = 'grey50', values = c(NA, '#EA883A')) +
  theme_bw()

ggsave('Coral presence in Philippine Municipalities.pdf')

rare_map <- ggplot(muni_map, aes(x = long, y = lat, group = group, fill = `Past work Rare`)) +
  geom_polygon() +
  coord_equal(ratio = 1) +
  guides(fill = F) +
  scale_fill_manual(values = c('blue')) +
  theme_bw()

ggsave('Rare sites.pdf')

fishers_map <- ggplot(muni_map, aes(x = long, y = lat, group = group, fill = `Past work Rare`)) +
  geom_polygon() +
  coord_equal(ratio = 1) +
  guides(fill = F) +
  scale_fill_manual(values = c('blue')) +
  theme_bw()

ggsave('Rare sites.pdf')


