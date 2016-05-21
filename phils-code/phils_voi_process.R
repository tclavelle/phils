########################################################################
## Philippines catch data processing and status estimation
##
## Data obtained from the CountrySTAT Philippines website available
## at http://countrystat.psa.gov.ph/?cont=10&pageid=1&ma=D10PNVMP
##
## By Tyler Clavelle
########################################################################

########################################################################
### Load packages ------------------------------------------------------
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(rgdal)
library(rgeos)
library(broom)

###########################################################################################################################
### Read in Philippines catch datasets and GUM results --------------------------------------------------------------------

# Total catch and status by region, species, and archetype
phils_status <- read.csv(file = '../../Google Drive/Project Data/phils-data/phils-results/phils_upside_status.csv', stringsAsFactors = F) %>%
  tbl_df() %>%
  dplyr::select(-X)

# Processed but unaggregated fisheries data
phils_fish <- tbl_df(read.csv(file = '../../Google Drive/Project Data/phils-data/phils_fisheries_processed.csv', stringsAsFactors = F))

###########################################################################################################################
### Compile data necessary for VOI analysis -------------------------------------------------------------------------------

# Build empty VOI input dataframe
phils_input<-data_frame(archetypeName=NA,subArchetypeName=NA, speciesName=NA, fleetName_1=NA, fleetName_2=NA, patchName_1=NA,
                        patchName_2=NA, K_1=NA, K_2 = NA, g_1=NA, g_2=NA, phi_1=NA, phi_2=NA, b0_1=NA, b0_2=NA, m=NA, f0_1_1=NA,
                        f0_1_2=NA, f0_2_1=NA, f0_2_2=NA, q0_1_1=NA, q0_1_2=NA, q0_2_1=NA, q0_2_2=NA, E0_1_1=NA, E0_1_2=NA,
                        E0_2_1=NA, E0_2_2=NA, habitatHealth_1=NA, habitatHealth_2=NA, habitatDegradationRate_1=NA,
                        habitatDegradationRate_2=NA, gHab_1=NA, gHab_2=NA, gamma_1=NA, gamma_2=NA, price_1=NA, price_2=NA,
                        variableCost_1=NA, variableCost_2=NA)

## Note: For the numbered variables, 1 = municiapal/artisanal and 2 = commercial

# Table of habitat parameters
habitat<-data_frame(archetype = c('Hard bottom', 'Soft bottom'),
                    health   = c(1,1),
                    deg_rate = c(0,0),
                    gHab     = c(0,0))

# Extract relevent parameters from GUM status and catch statistics
input<-phils_status %>%
  filter(year==max(year)) %>%
  dplyr::select(Archetype, region_name, CommName, SciName, BvBmsy, FvFmsy, phi, g, k) %>%
  rename(archetypeName = Archetype,
         subArchetypeName = region_name,
         speciesName      = CommName,
         b0_1             = BvBmsy,
         K_1              = k,
         g_1              = g,
         phi_1            = phi)


# Calculate fraction of catch from municipal and commercial fleets by region and species
catch_ratios<-phils_fish %>%
  dplyr::select(archetype,region_name, province_name,scale,species,year,harvest) %>%
  filter(harvest>0) %>%
  spread(scale,harvest) %>%
  mutate(perc_muni = round(Municipal/(Municipal + Commercial), digits = 2),
         perc_comm = 1-perc_muni) %>%
  group_by(region_name,archetype,species,year) %>%
  summarize(wt_mean_muni = sum(Municipal, na.rm =T)/(sum(Municipal, na.rm = T) + sum(Commercial, na.rm = T)),
            wt_mean_comm = sum(Commercial, na.rm =T)/(sum(Municipal, na.rm = T) + sum(Commercial, na.rm = T))) %>%
  filter(year==max(year)) %>%
  ungroup() 

# Calculate fraction of catch from municipal and commercial fleets by species
catch_ratios_nat<-phils_fish %>%
  dplyr::select(archetype,region_name, province_name,scale,species,year,harvest) %>%
  filter(harvest>0) %>%
  spread(scale,harvest) %>%
  mutate(perc_muni = round(Municipal/(Municipal + Commercial), digits = 2),
         perc_comm = 1-perc_muni) %>%
  group_by(archetype,species,year) %>%
  summarize(wt_mean_muni = sum(Municipal, na.rm =T)/(sum(Municipal, na.rm = T) + sum(Commercial, na.rm = T)),
            wt_mean_comm = sum(Commercial, na.rm =T)/(sum(Municipal, na.rm = T) + sum(Commercial, na.rm = T))) %>%
  filter(year==max(year)) %>%
  ungroup()

# Join input dataframe with regional catch ratios
df<-catch_ratios %>%
  rename(archetypeName    = archetype,
         subArchetypeName = region_name,
         speciesName      = species) %>%
  right_join(input, by = c('archetypeName','subArchetypeName','speciesName'))

# Add national percentages
nats<-unique(catch_ratios_nat$species)

for(a in 1:length(nats)) {
  df[df$speciesName==nats[a], c("wt_mean_muni", 'wt_mean_comm')]<-catch_ratios_nat[match(nats[a], catch_ratios_nat$species),c('wt_mean_muni','wt_mean_comm')]
}

# Set year to 2014
df$year<-2014

# Calculate catch-weighted prices by region and sector for inputs
p_diffs<-phils_fish %>%
  dplyr::select(archetype,region_name, province_name,scale,species,year,harvest, value_us, exrate) %>%
  group_by(archetype, region_name, species, year, scale, exrate) %>%
  summarize(wt_mean_price = sum(value_us, na.rm =T)/(sum(harvest, na.rm = T))) %>% # weighted mean
  ungroup() %>%
  dplyr::select(-exrate) %>% # drop exchange rate
  spread(scale,wt_mean_price) %>% 
  mutate(p_diff = Municipal - Commercial) %>%
  rename(price_1 = Municipal,
         price_2 = Commercial) 

# Join price data with input data frame
df<-p_diffs %>%
  filter(year==max(year)) %>%
  dplyr::select(archetype, region_name, species, price_1, price_2) %>%
  rename(archetypeName    = archetype,
         subArchetypeName = region_name,
         speciesName      = species) %>%
  right_join(df)

########################################################################
### summarize data and map to philippines -----------------------------

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


