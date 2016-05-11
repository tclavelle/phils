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
# library(GUM)

# setwd("~/Desktop/GitHub/voi") # temporary until VOI repository is created

########################################################################
### Read in and combine Philippine regional datasets -------------------

# Get all file names in the catch data directory
cfiles<-list.files('voi/phils-catch-data/',full.names = T)

# read all data files, skipping the header lines
phils<-lapply(cfiles,read.csv,skip=3,stringsAsFactors=F,na.strings=c('..','',' ','.'))

# read in fisheries catch and value files from Antonius. **Units are thousands of Philippine Pesos**
philsvalue<-read.csv(file = 'voi/phils-data-antonius/MunVal2002-14.csv',skip = 3, stringsAsFactors = F, na.strings=c('..','',' ','.'))

philscatch<-read_excel(path = 'voi/phils-data-antonius/regional fisheries data philippines_psa.xlsx',sheet = 1)

# read in archetype list matched to philippine data categories
phils_archs<-read.csv(file = 'VOI-Model/Data/phils-data/phils_archetypes.csv',stringsAsFactors = F)

########################################################################
### process each data file -----------------------------------------------

philscatch<-philscatch %>%
  tbl_df() %>%
  gather(key=year,value=catch,4:17) %>%
  dplyr::rename(
    region=Region,
    municipality=Municipality,
    species=Species) %>%
  mutate(
    region  = gsub(region, pattern = '..', replacement = '',fixed = T),
    species = gsub(species, pattern = '..', replacement = '',fixed = T)) %>%
  arrange(region,municipality,species,year)

# Create lookup table for scientific names
species<-unique(philscatch$species)

philscatch<-philscatch %>%
  group_by(region,municipality,species) %>%
  mutate(IdOrig=paste(region,municipality,species,sep='_'),
         SpeciesCat=36) %>%
  ungroup() %>%
  dplyr::rename(Year=year,Catch=catch,SciName=species)

# write function to process wide format output from country stat Philippines
cleanphils<-function(df){

  # change column names and convert to long format
  if(unique(colnames(df) == c("X.","X..1","Annual","X..2","X..3","X..4","X..5","X..6","X..7","X..8","X..9","X..10","X..11","X..12","X..13","X..14" ))){
  df<-df %>%
    tbl_df() %>%
    dplyr::rename(municipality=X., # rename columns
           species=X..1,
           x2002=Annual,
           x2003=X..2,
           x2004=X..3,
           x2005=X..4,
           x2006=X..5,
           x2007=X..6,
           x2008=X..7,
           x2009=X..8,
           x2010=X..9,
           x2011=X..10,
           x2012=X..11,
           x2013=X..12,
           x2014=X..13,
           x2015=X..14) %>%
    gather(year,catch,3:16) %>% # convert to long format
    slice(2:n()) %>% # drop first row
    filter(!(municipality %in% c('. Data not available, .. No reported data'))) # drop this row
  }

  if(unique(colnames(df) == c("X","X.1","Annual","X.2","X.3","X.4","X.5","X.6","X.7","X.8","X.9","X.10","X.11","X.12","X.13","X.14" ))){
    df<-df %>%
      tbl_df() %>%
      dplyr::rename(municipality=X, # rename columns
             species=X.1,
             x2002=Annual,
             x2003=X.2,
             x2004=X.3,
             x2005=X.4,
             x2006=X.5,
             x2007=X.6,
             x2008=X.7,
             x2009=X.8,
             x2010=X.9,
             x2011=X.10,
             x2012=X.11,
             x2013=X.12,
             x2014=X.13,
             x2015=X.14) %>%
      gather(year,catch,3:16) %>% # convert to long format
      slice(2:n()) %>% # drop first row
      filter(!(municipality %in% c('. Data not available, .. No reported data'))) # drop this row
  }

  # find unique non-NA regions
  combos<-unique(df[,c('municipality','year')]) %>%
    filter(is.na(municipality)==F)

  # loop over unique municipalitys and years and fill in municipality name
  for(a in 1:nrow(combos)){
    st<-which((df$municipality==combos$municipality[a] & df$year==combos$year[a])) # starting position
    end<-st+32 # ending position (32 species listed below each municipality)
    df$municipality[st:end]<-combos$municipality[a] # fill section with corresponding municipality
  }

  # remove all rows where species is NA and clean data entries
  df<- df %>%
    filter(is.na(species)==F) %>%
    mutate(
      municipality = gsub(municipality, pattern = '....', replacement = '', fixed = T),
      species = gsub(species, pattern = '..', replacement = '', fixed = T),
      year = gsub(year, pattern = 'x', replacement = ''))

  return(results=df)
}

# flatten phils list using function
philscatch1<-ldply(phils,.fun = cleanphils) %>%
  tbl_df()

# Philippines-US historical exchange rates
exrates<-data.frame(year=as.character(c(2002:2015)),exrate=c(0.019379845, 0.018450185, 0.017844397, 0.018152115,	0.019489378,	0.021668472,	0.022563177,	0.020973154,
0.022168034,0.023089356,0.023679848,0.023557126,0.022522523,0.021978022),stringsAsFactors = F)

# apply function to fishery value file
value<-cleanphils(philsvalue) %>%
  rename(value_1000_pesos=catch) 

# convert value to numeric after removing commas to avoid converting to NAs. Then create new variable that is in units of pesos
value$value_1000_pesos<-as.numeric(gsub(',',replacement = '',value$value_1000_pesos))
value$value_pesos<-value$value_1000_pesos*1000

value<-value %>%
  left_join(exrates,by='year') %>%
  mutate(value_US = value_pesos*exrate) # convert to US dollars using exchange rates

# join catch and value datasets and calculate price
final<-left_join(philscatch1,value) %>%
  mutate(priceperton=value_US/catch) %>%
  arrange(municipality,species,year)

# remove duplicated rows
final<-distinct(final)

# clean mismatched province names
final$municipality[final$municipality=='Tawi-tawi']<-'Tawi-Tawi'
final$municipality[final$municipality=='Metro Manila']<-'Metropolitan Manila'

# change municipality to "province", which is the correct resolution
final <- final %>% rename(province = municipality)

## Merge with archetypes file 
final<-left_join(final,phils_archs, by = 'species')

# write csv
write.csv(final, file = 'VOI-Model/Data/phils-data/phils_provincial_data.csv')


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


