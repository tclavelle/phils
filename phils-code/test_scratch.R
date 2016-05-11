
library(GUM)

philscatch<-read.csv(file = 'Misc Data/test GUM.csv',stringsAsFactors = F) %>%
  tbl_df() %>%
  rename(CommName=SciName) %>%
  mutate(SpeciesCat=NA)

afsis<-read.csv(file = 'Misc Data/Species_ASFIS_ISSCAAP.csv', stringsAsFactors = F)

isscaap<-read.csv(file = 'Misc Data/ISSCAAP Codes.csv', stringsAsFactors = F)

## Make lookup table for species scientific names and categories
sps<-data_frame(CommName=unique(philscatch$CommName), SciName=NA, SpCat=NA)

# SciNames
sps$SciName[3]<-'Selar crumenophthalmus' # big eye scad
sps$SciName[4]<-'Thunnus obesus' # big eye tuna
sps$SciName[5]<-'Portunus pelagicus' # blue crab
sps$SciName[9]<-'Euthynnus affinis' # eastern little tuna
sps$SciName[12]<-'Auxis thazard' # frigate tuna
sps$SciName[15]<-'Trichiurus lepturus' # hairtail
sps$SciName[17]<-'Rastrelliger kanagurta' # indian mackerel
sps$SciName[18]<-'Scomberomorus guttatus' # indo pacific mackerel
sps$SciName[24]<-'Decapterus punctatus' # round scad
sps$SciName[26]<-'Katsuwonus pelamis' # skip jack
sps$SciName[29]<-'Scomberomorus commerson' # spanish mackerel
sps$SciName[32]<-'Thunnus albacares' # yellowfin tuna

# Species categories


test<-left_join(philscatch,sps, by='CommName') %>%
  filter(is.na(SciName)==F & is.na(Catch)==F & SciName %in% afsis$Species_AFSIS) %>%
  dplyr::select(IdOrig,SciName,SpeciesCat,Year,Catch) %>%
  mutate(
    SpeciesCat = as.integer(SpeciesCat),
    Year       = as.integer(Year),
    Catch      = as.integer(Catch),
    SpeciesCat = NA)

# testing
stocks = unique(test$IdOrig)

sub = sample(stocks, 20, replace = F)

small_dat = filter(test, IdOrig %in% sub)

less_dat = small_dat %>% dplyr::select(IdOrig,SciName,SpeciesCat,Year,Catch) %>%
  mutate(IdOrig = as.character(IdOrig))

less_dat<-data.frame(less_dat)

less_dat<-assign_spcategory(dat = less_dat, afsismatch = afsis)

results = run_gum_assessment(dat = less_dat)

