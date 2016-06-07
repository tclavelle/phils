########################################################################
## Philippines fisheries and aquaculture data analysis 
##
## Data obtained from the CountrySTAT Philippines website available 
## at http://countrystat.psa.gov.ph/?cont=10&pageid=1&ma=D10PNVMP
##
## By Tyler Clavelle
########################################################################

########################################################################
### Load packages and data ---------------------------------------------

## Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(broom)
library(readr)

## Data files
# Fishery and aquaculture data
phils <- read_csv(file = '../../Google Drive/Project Data/phils-data/phils_complete_processed.csv')
phils_fish <- read_csv(file = '../../Google Drive/Project Data/phils-data/phils_fisheries_processed.csv')
phils_aqua <- read_csv(file = '../../Google Drive/Project Data/phils-data/phils_aqua_processed.csv')

# Region socioeconomic data
region_data <- tbl_df(read.csv(file = '../../Google Drive/Project Data/phils-data/phils_fisheries_regions.csv', stringsAsFactors = F))

colnames(region_data) <- tolower(colnames(region_data))

########################################################################
### Prepare fishery and aquaculture data for regression ----------------

# function to prepare regression data at desired aggregation level
prep_regression <- function(phils_aqua_df = phils_aqua, phils_fish_df = phils_fish, region_data, grp_vars = 'region_name') {
  
  # phils_aqua_df = phils_aqua
  # phils_fish_df = phils_fish
  # grp_vars = 'region_name'
  
  # aggregate all fishery and aquaculture data by region
phils_aqua_nat <- phils_aqua_df %>%
  rename_(grp_var = grp_vars) %>%
  rename(aqua_harvest  = harvest,
         aqua_price    = price,
         aqua_value_us = value_us,
         farm_type     = archetype) %>%
  group_by(grp_var, farm_type, year) %>%
  summarize(aqua_harvest  = sum(aqua_harvest, na.rm = T),
            aqua_value_us = sum(aqua_value_us, na.rm = T),
            aqua_price    = sum(aqua_value_us, na.rm = T) / sum(aqua_harvest, na.rm = T)) %>%
  ungroup()

# fix 0s and infinites to NA
phils_aqua_nat[phils_aqua_nat$aqua_harvest==0 | phils_aqua_nat$aqua_value_us==0, c('aqua_harvest','aqua_value_us','aqua_price')] <- NA

## Assign variables by culture type 
farm_culture <- data_frame(farm_type    = unique(phils_aqua_nat$farm_type),
                           culture_type = c('Marine', 'Marine', 'Seaweed', 'Freshwater', 'Marine', 'Freshwater', 'Freshwater',
                                            'Mussel', 'Oyster', 'Freshwater', 'Freshwater', 'Marine', 'Marine'))
# Spread culture types out 
aqua_harvest <- phils_aqua_nat %>%
  left_join(farm_culture) %>%
  select(grp_var, culture_type, year, aqua_harvest) %>%
  group_by(grp_var, culture_type, year) %>%
  summarize(aqua_harvest = sum(aqua_harvest, na.rm = T)) %>%
  spread(culture_type, aqua_harvest) 

colnames(aqua_harvest) <- c('grp_var','year','freshwater_harvest','marine_harvest','mussel_harvest','oyster_harvest','seaweed_harvest')

aqua_price <- phils_aqua_nat %>%
  left_join(farm_culture) %>%
  select(grp_var, culture_type, year, aqua_value_us, aqua_harvest) %>%
  group_by(grp_var, culture_type, year) %>%
  summarize(aqua_price = sum(aqua_value_us, na.rm = T) / sum(aqua_harvest, na.rm = T)) %>%
  spread(culture_type, aqua_price) 

colnames(aqua_price) <- c('grp_var','year','freshwater_price','marine_price','mussel_price','oyster_price','seaweed_price')

phils_aqua_nat <- aqua_harvest %>%
  left_join(aqua_price)

phils_fish_nat <- phils_fish_df %>%
  rename_(grp_var = grp_vars) %>%
  rename(fish_harvest  = harvest,
         fish_price    = price,
         fish_value_us = value_us) %>%
  group_by(grp_var, isscaap_name, archetype, year) %>%
  summarize(fish_harvest  = sum(fish_harvest, na.rm = T),
            fish_value_us = sum(fish_value_us, na.rm = T),
            fish_price    = sum(fish_price, na.rm = T) / sum(fish_harvest, na.rm = T)) %>%
  ungroup()

# fix 0s and infinites to NA
phils_fish_nat[phils_fish_nat$fish_harvest==0 | phils_fish_nat$fish_value_us==0, c('fish_harvest','fish_value_us','fish_price')] <- NA

# join national aggregates
phils_all_nat <- phils_fish_nat %>%
  left_join(phils_aqua_nat) %>%
  filter(is.na(fish_harvest)==F)

########################################################################
### Prepare socioeconomic data for inclusion in regression 

# Aggregate by region 
reg_1 <- region_data %>%
  tbl_df() %>%
  select(-x, -region_psgc, -province_psgc, -municipal_psgc, -psgc, -component.lgu, -income_class) %>%
  separate(income.class, into = c('income_class','drop'), sep = '[[:lower:]]') %>%
  rename_(grp_var = grp_vars) %>%
  group_by(grp_var) %>%
  summarize(length_coast     = sum(length_of_coastline_km, na.rm = T),
            no_barangays     = sum(no_of_barangays, na.rm = T),
            area_muni_waters = sum(area_of_municipal_waters_ha, na.rm = T),
            coastal_mgmt     = mean(coastal.marine.ecosystems.management, na.rm = T),
            enviro_avg       = mean(environ.average, na.rm = T),
            poverty          = mean(poverty_incidence, na.rm = T),
            gov_avg          = mean(governance.average, na.rm = T),
            income_level     = mean(as.numeric(income_class), na.rm = T)) %>%

  ungroup()

# Extract and spread population data into single variable
reg_pop <- region_data %>%
  rename_(grp_var = grp_vars) %>%
  select(grp_var, contains('population')) %>%
  gather(pop_var, population, 2:5) %>%
  group_by(grp_var, pop_var) %>%
  summarize(area_population = sum(population, na.rm = T)) %>%
  separate(pop_var, into = c('variable', 'years'), sep = '_') %>%
  mutate(years = as.numeric(years)) %>%
  complete(years = 1980:2013) %>%
  rename(year = years) %>%
  ungroup()

regs <- unique(reg_pop$grp_var) # unique regions

# Fill in missing years for population
for(a in 1:length(regs)) {
  reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year < 1990] <- reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year == 1980]
  reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year > 1990 & reg_pop$year < 2000 ] <- reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year == 1990]
  reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year > 2000 & reg_pop$year < 2010 ] <- reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year == 2000]
  reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year > 2010 ] <- reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year == 2010]
}

# join datasets
phils_all_nat <- phils_all_nat %>%
  left_join(reg_pop) %>% 
  left_join(reg_1) %>%
  arrange(grp_var,isscaap_name, archetype, year )

return(phils_all_nat)
}

# create regression data
reg_level<-prep_regression(phils_aqua_df = phils_aqua, phils_fish_df = phils_fish, region_data, grp_var = 'region_name')
prov_level<-prep_regression(phils_aqua_df = phils_aqua, phils_fish_df = phils_fish, region_data, grp_var = 'province_name')

########################################################################
### Run regressions  ---------------------------------------------------

## Regression variables
# Model 1
m1_vars <- c('grp_var','isscaap_name','archetype','year', 'fish_price', 'freshwater_harvest', 
              'marine_harvest','mussel_harvest','oyster_harvest','seaweed_harvest','freshwater_price',
             'marine_price','mussel_price','oyster_price','seaweed_price')  

fmla_1 <- as.formula(paste("fish_harvest ~ ", paste( m1_vars, collapse= "+"))) 

# Model 2
m2_vars <- c('grp_var','isscaap_name','year', 'fish_price', 'freshwater_harvest', 
             'marine_harvest','mussel_harvest','oyster_harvest','seaweed_harvest','freshwater_price',
             'marine_price','mussel_price','oyster_price','seaweed_price') 

fmla_2 <- as.formula(paste("fish_harvest ~ ", paste( m2_vars, collapse= "+")))

# Model 3
m3_vars <- c('grp_var','archetype','year', 'fish_price', 'freshwater_harvest', 
             'marine_harvest','mussel_harvest','oyster_harvest','seaweed_harvest','freshwater_price',
             'marine_price','mussel_price','oyster_price','seaweed_price') 

fmla_3 <- as.formula(paste("fish_harvest ~ ", paste( m3_vars, collapse= "+")))

# Model 4
m4_vars <- c('grp_var','archetype','year', 'fish_price', 'freshwater_harvest', 
             'marine_harvest','mussel_harvest','oyster_harvest','seaweed_harvest','freshwater_price',
             'marine_price','mussel_price','oyster_price','seaweed_price') 

fmla_4 <- as.formula(paste("fish_harvest ~ ", paste( m4_vars, collapse= "+")))

# Model 4
m5_vars <- c('grp_var','isscaap_name','year', 'fish_price', 
             'marine_harvest','mussel_harvest','oyster_harvest','seaweed_harvest') 

fmla_5 <- as.formula(paste("fish_harvest ~ ", paste( m5_vars, collapse= "+")))

# run aggregate model
m1 <- lm(fmla_1, data = reg_level)
m2 <- lm(fmla_2, data = reg_level)
m3 <- lm(fmla_3, data = reg_level)
m4 <- lm(fmla_4, data = reg_level)
m5 <- lm(fmla_5, data = reg_level)

m6 <- lm(fmla_1, data = prov_level)
m7 <- lm(fmla_2, data = prov_level)
m8 <- lm(fmla_3, data = prov_level)
m9 <- lm(fmla_4, data = prov_level)
m10 <- lm(fmla_5, data = prov_level)

glance(m1)
glance(m2)
glance(m3)
glance(m4)
glance(m5)

glance(m6)
glance(m7)
glance(m8)
glance(m9)
glance(m10)

summary(m5)
summary(m10)

# Run model on certain species groups
sb <- prov_level %>%
  filter(archetype == 'Soft bottom')

m11 <- lm(fmla_5, data = sb)

summary(m11)
