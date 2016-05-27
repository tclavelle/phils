########################################################################
##
## Philippines analysis using upside model (GUM) 
##
## By Tyler Clavelle
########################################################################

###########################################################################################################
### Load packages and any functions -----------------------------------------------------------------------
library(GUM)
library(readxl)
library(readr)
library(ggplot2)
library(snowfall)
library(parallel)
library(reshape2)

###########################################################################################################
### Process Philippines catch data to work with GUM package -----------------------------------------------

# Load and clean data to correct column names
phils_data<-read.csv(file = '../../Google Drive/Project Data/phils-data/phils_fisheries_processed.csv', stringsAsFactors = F) %>%
  rename(CommName       = species,
         SciName        = scientific_name,
         SpeciesCat     = isscaap_code,
         Catch          = harvest,
         SpeciesCatName = isscaap_name,
         Archetype      = archetype,
         Year           = year,
         Price          = price) %>% # still need to convert from value to price per ton
  mutate(IdOrig     = paste(province_name,CommName, sep = '_')) %>%
  select(IdOrig,
         region_name,
         province_name,
         CommName,
         SciName,
         SpeciesCat,
         SpeciesCatName,
         Archetype,
         scale,
         Year,
         Catch,
         Price)

#### Aggregate catch series by archetype. Bind dataset back together with unaggregated archetypes
### - Large and small pelagics aggregated to national level
### - Soft and hard bottom aggregated to fishing region 

# Aggregate large and small pelagics to national level
lgsmpelagic<-phils_data %>%
  filter(Archetype %in% c('Large pelagic','Small pelagic')) %>%
  group_by(Archetype,CommName, SciName, SpeciesCat, SpeciesCatName, Year) %>%
  summarize(Catch = sum(Catch,na.rm=T),
            Price = mean(Price,na.rm=T)) %>%
  mutate(province_name = 'National', 
         region_name   = 'National',
         IdOrig        = paste(region_name,CommName, sep = '_')) %>% 
  ungroup() %>%
  arrange(IdOrig,province_name,region_name,CommName,SciName,SpeciesCat,SpeciesCatName,Archetype,Year,Catch,Price)


# Aggregate large and small pelagics by region
demersal<-phils_data %>%
  filter(Archetype %in% c('Soft bottom','Hard bottom')) %>%
  group_by(Archetype,region_name,CommName, SciName, SpeciesCat, SpeciesCatName, Year) %>%
  summarize(Catch = sum(Catch,na.rm=T),
            Price = mean(Price,na.rm=T)) %>%
  mutate(IdOrig   = paste(region_name,CommName, sep = '_')) %>%
  ungroup()

# Bind results of lg and small pelagics and demersal species
phils_final<-lgsmpelagic %>%
  bind_rows(demersal)
  
# Save as dataframe for compatability with GUM !!
phils_final<-data.frame(phils_final,stringsAsFactors = F)

###########################################################################################################
### Run data through GUM package to estimate status -------------------------------------------------------

## Run model
phils_results<-run_gum_assessment(phils_final)

# Save raw results
write.csv(phils_results, file = '../../Google Drive/Project Data/phils-data/phils-results/phils_upside_status.csv')

# Run projections
phils_results<-read.csv(file = '../../Google Drive/Project Data/phils-data/phils-results/phils_upside_status.csv',
                        stringsAsFactors = F) %>% 
  select(-X) %>%
  mutate(BvBmsyOpenAccess = 0.3,
         CatchShare     = 0) 

phils_proj<-RunProjection(Data = phils_results, BaselineYear = 2014, NumCPUs = 1)

phils_projects<-phils_proj[[1]]

# Save projections
write_csv(phils_projects, path = '../../Google Drive/Project Data/phils-data/phils-results/phils_projections.csv')

###########################################################################################################
## Summarize results --------------------------------------------------------------------------------------

### Status results ###

# national summary
nat_results<-phils_results %>%
  group_by(year) %>%
  summarize(medianb_nat           = median(BvBmsy,na.rm=T),
            medianf_nat           = median(FvFmsy,na.rm=T),
            msy_wt_geom_mean_bnat = exp(sum(MSY * log(FvFmsy + 1e-3), na.rm = T)/sum(MSY, na.rm = T)),
            msy_wt_geom_mean_fnat = exp(sum(MSY * log(BvBmsy), na.rm = T)/sum(MSY, na.rm = T)))

# national archetype summary
nat_arch_results<-phils_results %>%
  group_by(Archetype,year) %>%
  summarize(totalcatch            = sum(catch,na.rm=T),
            medianb_nat           = median(BvBmsy,na.rm=T),
            medianf_nat           = median(FvFmsy,na.rm=T),
            msy_wt_geom_mean_bnat = exp(sum(MSY * log(FvFmsy + 1e-3), na.rm = T)/sum(MSY, na.rm = T)),
            msy_wt_geom_mean_fnat = exp(sum(MSY * log(BvBmsy), na.rm = T)/sum(MSY, na.rm = T)))

# summarize results by region, adding national stocks to each region
region_results<-phils_results %>%
  group_by(region_name,year) %>%
  summarize(totalcatch            = sum(catch,na.rm=T),
            medianb_nat           = median(BvBmsy,na.rm=T),
            medianf_nat           = median(FvFmsy,na.rm=T),
            msy_wt_geom_mean_bnat = exp(sum(MSY * log(FvFmsy + 1e-3), na.rm = T)/sum(MSY, na.rm = T)),
            msy_wt_geom_mean_fnat = exp(sum(MSY * log(BvBmsy), na.rm = T)/sum(MSY, na.rm = T)))

### Projection Results ###

# National summary by year and policy
nat_proj_results<-phils_projects %>%
  group_by(Year, Policy) %>%
  summarize(total_catch   = sum(Catch, na.rm = T),
            total_profits = sum(Profits, na.rm = T),
            total_biomass = sum(Biomass, na.rm = T)) %>%
  gather(Metric, Value, 3:5) %>%
  group_by(Policy)

# Extract today results
today<-phils_projects %>%
  filter(Policy=='Historic' & Year == 2014) %>%
  group_by(Year, Policy) %>%
  summarize(total_catch   = sum(Catch, na.rm = T),
            total_profits = sum(Profits, na.rm = T),
            total_biomass = sum(Biomass, na.rm = T))

# add today values to national values
nat_proj_results$baseline_catch <- today$total_catch
nat_proj_results$baseline_profits <- today$total_profits
nat_proj_results$baseline

ggplot(nat_proj_results, aes(x = Year, y = Value, color = Policy)) +
         geom_line() +
         facet_wrap(~Metric, scales = 'free')

ggplot(subset(nat_proj_results, Year == 2044), aes(x = Policy, y = Value, color = Policy)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Metric, scales = 'free')

###########################################################################################################
### Kobe and other summary plots  -------------------------------------------------------------------------

### Kobe plot ###  
phils_results %>%
    filter(year == 2014) %>%
    ggplot(aes(jitter(BvBmsy), jitter(pmin(4,FvFmsy)))) + 
    stat_density_2d(aes(fill = ..density..), geom = 'tile', n = 100, alpha = 0.8, contour = F) + 
    scale_fill_gradient2(guide = F,low = 'skyblue1', mid = 'white', high = 'khaki1', midpoint = 0.2) + 
    geom_hline(aes(yintercept = 1), linetype = 'longdash') + 
    geom_vline(aes(xintercept = 1), linetype = 'longdash') + 
    geom_point(aes(color = factor(Archetype), size = MSY, alpha = (MSY))) + 
    geom_point(data = subset(nat_results,year==2015), aes(medianb_nat, medianf_nat), shape = 17, size = 4) +
    geom_point(data = subset(nat_results,year==2015), aes(x = msy_wt_geom_mean_bnat, y = msy_wt_geom_mean_fnat),
               shape = 15, size = 4) +
    scale_size_continuous(guide = F) + 
    scale_alpha_continuous(guide = F, range = c(0.5,0.9)) + 
    xlab('B/Bmsy') + 
    ylab('F/Fmsy') + 
    labs(color = 'Category') +
    theme_classic() + 
    theme(text = element_text(size = 16)) +
    scale_x_continuous(limits = c(-1,4),breaks = seq(-1,4, by = 0.5),labels = c(seq(-1,2,by = 0.5), expression(phantom(x) >=2.5), seq(3,4, by = 0.5))) +
    scale_y_continuous(limits = c(-1,6),breaks = seq(-1,6, by = 0.5),labels = c(seq(-1,3.5,by = 0.5), expression(phantom(x) >=4), seq(4.5,6,by = 0.5))) + 
    coord_cartesian(xlim = c(0,2.5), ylim = c(0,4))

ggsave(filename = '../../Google Drive/Project Data/phils-data/phils-results/phils_Kobe.pdf')

### Histograms of b and f by year ###
phils_results %>%
  group_by(region_name,year) %>%
  summarize(medianb_prov            = median(BvBmsy,na.rm=T),
            medianf_prov            = median(FvFmsy,na.rm=T),
            msy_wt_geom_mean_b_prov = exp(sum(MSY * log(FvFmsy + 1e-3), na.rm = T)/sum(MSY, na.rm = T)),
            msy_wt_geom_mean_f_prov = exp(sum(MSY * log(BvBmsy), na.rm = T)/sum(MSY, na.rm = T))) %>%
  left_join(nat_results, by = 'year') %>%
  ggplot(aes(x=medianb_prov)) +
  geom_histogram(aes(fill = medianf_nat),binwidth = 0.1,color='black') +
  scale_fill_continuous(low = 'orange', high = 'dark red') +
  geom_vline(mapping = aes(xintercept = medianb_nat), color = 'blue', linetype = 2) +
  facet_wrap(~year, ncol = 5) +
  labs(x    = 'Median B/Bmsy',
       y    = 'Frequency',
       fill = 'Median F/Fmsy' ) +
  theme_bw()


### Line and plots of B/Bmsy and F/Fmsy results

ggplot(nat_arch_results,aes(x=year,y=medianf_nat,color=Archetype)) +
  geom_line() +
  theme_bw()

ggplot(nat_arch_results,aes(x=year,y=medianb_nat,color=Archetype)) +
  geom_line() +
  theme_bw()

ggplot(region_results,aes(x=msy_wt_geom_mean_bnat)) +
  geom_histogram() +
  facet_wrap(~year)






