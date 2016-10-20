########################################################################
## Philippines fisheries and aquaculture data processing 
##
## Data obtained from the CountrySTAT Philippines website available 
## at http://countrystat.psa.gov.ph/?cont=10&pageid=1&ma=D10PNVMP
##
## By Tyler Clavelle
########################################################################

########################################################################
### Load packages ------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(broom)
library(readr)

################################################################################
### Read in and combine Philippine regional fishery datasets -------------------
################################################################################

# Data storage folder
DataFolder<-'../../Google Drive/Project Data/phils-data/'

# Value and volume data by sector (municipal, commercial)
phils<-read_excel(path = paste(DataFolder,'phils-data-antonius/phils fish data volume and value 2002-2014.xlsx', sep = ''),sheet = 2) %>%
  tbl_df() 

# read in archetype list matched to philippine data categories
phils_archs<-read.csv(file = paste(DataFolder,'phils_archetypes.csv', sep = ''),stringsAsFactors = F)

################################################################################
## Construct lookup table of provinces and regions -----------------------------
################################################################################

# Load Rare data with info about each region
region_data<-read.csv(file = '../Misc Data/fishforever-data/Addressable market files/rare_addressable_mkt.csv', stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(REGION_NAME = toupper(REGION_NAME))

# Correct ARMM region
region_data$REGION_NAME[region_data$REGION_NAME=='ARMM - AUTONOMOUS REGION IN MUSLIM MINDANAO']<-'AUTONOMOUS REGION IN MUSLIM MINDANAO (ARMM)'

# Load dataset of all regions and provinces
phils_regions<-read.csv(file = '../../Google Drive/Project Data/phils-data/phils_regions.csv', stringsAsFactors = F, skip = 7, na.strings = '') %>%
  tbl_df() %>%
  rename(Reg_code    = Reg,
         Prov_code   = Prov) %>%
  filter(Inter.Level %in% c('Reg','Prov')) %>%
  spread(Inter.Level, Name) %>%
  select(-Urban.Rural.2000.CPH,-PSGC) %>%
  group_by(Reg_code) %>%
  mutate(Reg = toupper(Reg[is.na(Reg)==F])) %>%
  filter(is.na(Prov_code)==F) %>%
  ungroup() %>%
  rename(REGION_NAME = Reg,
         PROVINCE_NAME = Prov)

# join region data from rare with list of all regions
region_data<-region_data %>%
  select(-X) %>%
  right_join(phils_regions)

# write csv of region lookup table
write.csv(region_data, file = '../../Google Drive/Project Data/phils-data/phils_fisheries_regions.csv')

################################################################################
### process fisheries data into tidy format ------------------------------------  
################################################################################

# split volume and value and re-join
phils_catch<-phils %>%
  gather(key = Year, value = Volume, 6:18) %>%
  filter(Type=='Volume') %>%
  select(-Type)

phils_value<-phils %>%
  gather(key = Year, value = Value, 6:18) %>%
  filter(Type=='Value') %>% 
  mutate(
    Species  = gsub(Species,pattern = '0',replacement = '')
  ) %>%
  select(-Type)

phils<-phils_value %>%
  left_join(phils_catch, by = c('Scale','Region','Province','Species','Year'))

# Correct for Antonius assigning 0's to missing data
phils$Value[phils$Value==0]<-NA
phils$Volume[phils$Volume==0]<-NA

# clean mismatched province names
phils$Province[phils$Province=='Tawi-tawi']<-'Tawi-Tawi'
phils$Province[phils$Province=='Metro Manila']<-'Metropolitan Manila'

# add archetype to each stock
phils<-phils_archs %>%
  tbl_df() %>%
  rename(Species = species) %>%
  right_join(phils, by = 'Species')

# assign "Other" category to it's own archetype
phils$archetype[phils$Species=='Others']<-'Other'
phils$ISSCAAP_code[phils$Species=='Others']<-39
phils$ISSCAAP_name[phils$Species=='Others']<-'Marine fishes not identified'
phils$ASFIS_species[phils$Species=='Others']<-'Marine fishes nei'

phils$Province[phils$Province=='Samar']<-'SAMAR (WESTERN SAMAR)'

# assign regions from region list
phils<-phils %>%
  rename(PROVINCE_NAME = Province) %>%
  mutate(PROVINCE_NAME = toupper(PROVINCE_NAME)) %>%
  left_join(phils_regions)

# Standardize province names
phils$REGION_NAME[phils$PROVINCE_NAME=='METROPOLITAN MANILA']<-'NCR'
phils$REGION_NAME[phils$PROVINCE_NAME=='ZAMBOANGA CITY']<-'REGION IX (ZAMBOANGA PENINSULA)'
phils$REGION_NAME[phils$PROVINCE_NAME=='DAVAO CITY']<-'REGION XI (DAVAO REGION)'

phils<-phils %>%
  select(-Reg_code,-Prov_code,-Region) %>%
  rename(catch = Volume)

colnames(phils)<-tolower(colnames(phils))

##############################################################################
### Convert value from PHP to US and calculate price
##############################################################################

# Philippines-US historical exchange rates
exrates<-data.frame(year=as.character(c(2002:2015)),
                    exrate=c(0.019379845, 0.018450185, 0.017844397, 0.018152115,	0.019489378,	0.021668472,
                             0.022563177,	0.020973154, 0.022168034,0.023089356,0.023679848,
                             0.023557126,0.022522523,0.021978022),stringsAsFactors = F)

# create new variable that is in units of pesos rather than 1,000s of pesos
phils$value_pesos<-phils$value*1000

# convert to US dollars using exchange rates and calculate price per ton
phils<-phils %>%
  left_join(exrates,by='year') %>%
  mutate(value_us = value_pesos*exrate, 
         price=value_us/catch) %>%
  rename(harvest = catch)

###############################################################################
### Compile earlier national dataset (1963-2006) to supplement timeseries  
###############################################################################

# Read in national data from Deng and assign proper column types to avoid autoconversion
phils_64 <- read_excel(path = '../../Google Drive/Project Data/phils-data/PhilippineNationalFisheries_1963-2006.xlsx', sheet = 1,
                       col_types = c('text','text','text','text','text','text','text','text','text','text','text',
                                     'text','text','text','numeric','numeric','numeric'))

# Create SciName variable by combining scientific name and genus
phils_64 <- phils_64 %>%
  mutate(SciName = paste(Genus, Species, sep = ' '))

# Where 'NA NA', replace with Family name
phils_64$SciName[phils_64$SciName=='NA NA'] <- phils_64$Family[phils_64$SciName=='NA NA']

# Where NAs replace with Order
phils_64$SciName[is.na(phils_64$SciName)] <- phils_64$Order[is.na(phils_64$SciName)]

# Replace 'Genus NA' with 'Genus spp'
phils_64$SciName <- gsub(pattern = ' NA', replacement = ' spp', phils_64$SciName)

# order by region, species, and year
phils_64 <- phils_64 %>%
  dplyr::select(Region, Zone, Sector, SciName, Year, Landings) %>%
  arrange(Zone, Region, Sector, SciName, Year)

# Read in archetype list
archs <- read_csv(file = '../../Google Drive/Project Data/phils-data/phils_archetypes_63to06.csv') 

# join with phils_64
phils_64 <- phils_64 %>%
  left_join(archs) %>%
  rename(Catch = Landings) %>%
  dplyr::select(-Family, -Order)

# Filter data set
phils_64 <- phils_64 %>%
  filter(is.na(SpeciesCatName)==F) %>%
  dplyr::select(Region, Zone, CommName, SciName, SpeciesCat, SpeciesCatName, Year, Catch) %>%
  ungroup()

# Convert 1976 data to MT from kg
phils_64$Catch[phils_64$Year==1976] <- phils_64$Catch[phils_64$Year==1976] / 1000

# fix scientific name of frigate tuna
phils_64$SciName[phils_64$SciName=='Auxis thazard thazard'] <- 'Auxis thazard'

# Make lookup table of region names and numbers to join old data and new data
reg_lkup <- data_frame(region_name   = unique(phils$region_name),
                       Region = c('NCR','1','2','3','4','4','5','6','NIR','7','8','9','10','11','12','13','ARMM'))

# Join data with region lookup table 
phils_64 <- phils_64 %>%
  left_join(reg_lkup) %>%
  dplyr::select(-Region,-Zone)

# add ASFIS taxonomic data to both data sets for future aggregation purposes
asfis <- read_csv(file = '../Misc Data/ASFIS_Feb2014.csv') %>%
  dplyr::select(-CommName) %>%
  rename(SciName = Species_AFSIS)

# add to historical data
phils_64 <- phils_64 %>%
  left_join(asfis) %>%
  dplyr::select(-SpeciesCat_ISSCAAP_code)

# add to recent data
phils <- phils_archs %>%
  rename(SciName = scientific_name) %>%
  left_join(asfis) %>%
  rename(scientific_name = SciName) %>%
  dplyr::select(species, scientific_name, Family, Order) %>%
  right_join(phils)

# find all current matched species
m1 <- unique(phils_64$SciName)[unique(phils_64$SciName) %in% unique(phils$scientific_name)]


# write csv of phils fisheries
write_csv(phils, path = '../../Google Drive/Project Data/phils-data/phils_fisheries_processed.csv')

########################################################################
### Read and process aquaculture datasets  -----------------------------
########################################################################

## Aquaculture harvest data
# Get all file names in the aquaculture catch data directory
afiles<-list.files('../../Google Drive/Project Data/phils-data/phils-aqua-data/phils-aqua-catch-data',full.names = T)

# read all data files, skipping the header lines
phils_aqua<-lapply(afiles,read.csv,stringsAsFactors=F,na.strings=c('..','',' ','.'))

phils_aqua <- lapply(phils_aqua, function(x) {
  colnames(x)<-c('Region','Species',1996:2015)
  x <- x %>%
    tbl_df() %>%
    gather(year, aqua_harvest, 3:dim(x)[2])
}) %>% bind_rows()

phils_aqua<-phils_aqua %>%
  rename(PROVINCE_NAME = Region) %>%
  filter(grepl('....' ,PROVINCE_NAME, fixed = T)==T) %>%
  mutate(PROVINCE_NAME = toupper(gsub(pattern = '....', replacement = '', PROVINCE_NAME, fixed = T)),
         Species       = gsub(pattern = '....', replacement = '', Species, fixed = T))

# Extract culture types (define as archetype)
phils_aqua_archs<-phils_aqua %>%
  select(Species) %>%
  filter(grepl(pattern = '..', Species, fixed = T)) %>%
  distinct() %>%
  rename(Archetype  = Species) %>%
  mutate(Archetype  = gsub(pattern = '..', replacement = '', Archetype, fixed = T)) %>%
  mutate(Acronymn   = c('BF','BP','BC','FF','FP','FC','MP','MC','OYSTER','MUSSEL','SEAWEED','RF','SFR'))

# Filter out the aggregate results for culture types
phils_aqua<-phils_aqua %>%
  filter(grepl(pattern = '-', Species, fixed = T)==T | Species %in% c('..OYSTER','..MUSSEL','..SEAWEED')) %>%
  mutate(Species = gsub(pattern = '..', replacement = '', Species, fixed = T))

# add in culture type
phils_aqua<-phils_aqua %>%
  separate(Species, into = c('Acronymn','Species'), sep = '-', fill = 'left') # split apart the acronym 

phils_aqua$Acronymn[is.na(phils_aqua$Acronymn)]<-phils_aqua$Species[is.na(phils_aqua$Acronymn)] # fill in missing Acronymns for oysters, seaweed, and mussels

# strip white space resulting from the separate
phils_aqua<-phils_aqua %>%
  mutate(Species  = gsub(pattern = '^ ', replacement = '',Species, fixed =T), 
         Acronymn = gsub(pattern = ' ', replacement = '',Acronymn, fixed =T)) %>%
  left_join(phils_aqua_archs, by = 'Acronymn')

## Aquaculture value data
# Get all file names in the aquaculture catch data directory
afiles_val<-list.files('../../Google Drive/Project Data/phils-data/phils-aqua-data/phils-aqua-value-data',full.names = T)

# read all data files, skipping the header lines
phils_aqua_val<-lapply(afiles_val,read.csv,stringsAsFactors=F,na.strings=c('..','',' ','.'))

phils_aqua_val <- lapply(phils_aqua_val, function(x) {
  colnames(x)<-c('Region','Species',1996:2015)
  x <- x %>%
    tbl_df() %>%
    gather(year, aqua_value, 3:dim(x)[2])
}) %>% bind_rows()

phils_aqua_val<-phils_aqua_val %>%
  rename(PROVINCE_NAME = Region) %>%
  filter(grepl('....' ,PROVINCE_NAME, fixed = T)==T) %>%
  mutate(PROVINCE_NAME = toupper(gsub(pattern = '....', replacement = '', PROVINCE_NAME, fixed = T)),
         Species  = gsub(pattern = '....', replacement = '', Species, fixed = T))

# Filter out the aggregate results for culture types
phils_aqua_val<-phils_aqua_val %>%
  filter(grepl(pattern = '-', Species, fixed = T)==T | Species %in% c('..OYSTER','..MUSSEL','..SEAWEED')) %>%
  mutate(Species = gsub(pattern = '..', replacement = '', Species, fixed = T)) # add price conversion to mutate statment

# add in culture type
phils_aqua_val<-phils_aqua_val %>%
  separate(Species, into = c('Acronymn','Species'), sep = '-', fill = 'left') # split apart the acronym 

phils_aqua_val$Acronymn[is.na(phils_aqua_val$Acronymn)]<-phils_aqua_val$Species[is.na(phils_aqua_val$Acronymn)] # fill in missing Acronymns for oysters, seaweed, and mussels

# strip white space resulting from the separate
phils_aqua_val<-phils_aqua_val %>%
  mutate(Species  = gsub(pattern = '^ ', replacement = '',Species, fixed =T), 
         Acronymn = gsub(pattern = ' ', replacement = '',Acronymn, fixed =T)) %>%
  left_join(phils_aqua_archs, by = 'Acronymn')

## Join aquaculture tables and add culture types
phils_aqua_final<-phils_aqua %>%
  left_join(phils_aqua_val) %>%
  left_join(phils_regions[,c('PROVINCE_NAME','REGION_NAME')]) %>%
  mutate(id    = paste(PROVINCE_NAME,Acronymn,Species, sep = '-'),
         scale = 'Aquaculture')

# find ids for all stocks that have at least one year of data for either catch or value
has_data<-phils_aqua_final %>%
  group_by(id) %>%
  summarize(any_catch_data = sum(is.na(aqua_harvest)==F),
            any_val_data   = sum(is.na(aqua_value)==F)) %>%
  filter(any_catch_data>0 | any_val_data>0) %>%
  ungroup()

phils_aqua_final<-phils_aqua_final %>%
  filter(id %in% has_data$id) %>%
  arrange(PROVINCE_NAME,Species,Archetype,year)

# convert all colunmn names to lower                  
colnames(phils_aqua_final)<-tolower(colnames(phils_aqua_final))

# add variables in order to match with phils fishery dataset 
phils_aqua_final<-phils_aqua_final %>%
  mutate(isscaap_code  = as.integer(NA),
         isscaap_name  = as.character(NA),
         asfis_species = as.character(NA),
         scientific_name = as.character(NA)) %>%
  rename(harvest = aqua_harvest,
         value   = aqua_value) %>%
  select(-acronymn, -id)

# build table for isscaap variables for aquaculture species

##############################################################################
### Convert value from PHP to US and calculate price
##############################################################################

# create new variable that is in units of pesos rather than 1,000s of pesos
phils_aqua_final$value_pesos<-phils_aqua_final$value*1000

# convert to US dollars using exchange rates and calculate price per ton
phils_aqua_final<-phils_aqua_final %>%
  left_join(exrates,by='year') %>%
  mutate(value_us = value_pesos*exrate, 
         price=value_us/harvest)

# write csv of phils aquaculture
write_csv(phils_aqua_final, path = '../../Google Drive/Project Data/phils-data/phils_aqua_processed.csv')

########################################################################
### Standardize and join fishery and aquaculture datasets  -------------
########################################################################

phils_complete<-full_join(phils, phils_aqua_final)

# write csv of complete data set
write_csv(phils_complete, path = '../../Google Drive/Project Data/phils-data/phils_complete_processed.csv')
