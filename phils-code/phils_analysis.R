########################################################################
## Philippines fisheries and aquaculture data analysis 
##
## Data obtained from the CountrySTAT Philippines website available 
## at http://countrystat.psa.gov.ph/?cont=10&pageid=1&ma=D10PNVMP and
## processed by Tyler Clavelle in phils_process.R script
##
## By Tyler Clavelle
########################################################################

########################################################################
### Load packages ------------------------------------------------------
# library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(rgdal)
library(rgeos)
library(broom)

# source plotting themes
source(file='../My Scripts/Plot Code/GGplotThemes.R')

########################################################################
### calculate and plot data summaries  ---------------------------------

### Summary function ----
fun1<-function(x,gpvars,years){
  
  gpvars<-lapply(gpvars, as.symbol) # need to convert character vector to symbols to make group_by_ work
  
  x2<-x %>% 
    filter(Year %in% years) %>%
    group_by_(.dots=gpvars) %>% # no idea why the .dots= does but its what StackOverFlow said and it works
    summarize(
      totalcatch  = sum(Volume,na.rm=T),
      totalvalue  = sum(Value,na.rm=T)) %>%
    group_by(Province) %>%
    mutate(
      provcatch   = sum(totalcatch,na.rm=T),
      provvalue   = sum(totalvalue,na.rm=T),
      percofcatch  = 100*(totalcatch/provcatch),
      percofvalue = 100*(totalvalue/provvalue)) %>%
    ungroup()
  return(x2)
}

#### % of catch by species group in each province ####
# calculation
out1<-fun1( x= phils, gpvars = c('Province','Species'), years = 2014) 

# histogram of % of catch in each province by species 
ggplot(out1,aes(x=percofcatch)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Species)

# stacked bar chart of top 5 species % in
out1 %>%
  filter(is.na(percofcatch)==F) %>%
  arrange(Province,desc(totalcatch)) %>%
  group_by(Province) %>%
  mutate(rank=dense_rank(desc(totalcatch))) %>%
  filter(rank<=10) %>%
  ggplot(aes(x=Province,y=percofcatch,fill = Species)) +
  geom_bar(stat='identity',position = 'stack') +
  guides(fill=F)

#### % of catch by commercial and municipal fisheries in each province ####
# calculation
out2<-fun1( x= phils, gpvars = c('Province','Scale'), years = 2014)

# histogram of % of catch in each province by sector
ggplot(out2,aes(x=percofcatch)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~Scale)

# scatterplot of total provincial catch by % of provincial catch from municipal fisheries
out2 %>%
  select(Province,Scale,totalcatch,percofcatch,provcatch) %>%
  group_by(Province) %>%
  mutate(provcatch=sum(totalcatch,na.rm=T)) %>%
  filter(Scale=='Municipal') %>%
  ggplot(aes(x=percofcatch,y=provcatch)) +
  geom_point(size=3) +
  labs(x='% of Provincial Catch from\n Municipal Fisheries', y = 'Total Provincial Catch (MT)') +
  scatterTheme

ggsave(file = 'Percent of Provincial Catch by Sector.pdf', width = 6, height = 6)

#### % of catch by municipal, commercial, and species group by province ####
# calculation
out3<-fun1( x= phils, gpvars = c('Province','Scale','Species'), years = 2014)

# histogram of % of provincial catch from each species group
ggplot(out1,aes(x=percofcatch)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~Species)

# scatterplot of total provincial catch by % of provincial catch from municipal fisheries for each species group
sps<-unique(out3$Species)

pdf(file = 'Percent of Provincial Catch by Species All Provinces.pdf')

for(a in 1:length(sps)) # !! need to rethink calculation of % of total
{
  temp<-out3 %>%
    filter(Species==sps[a]) %>%
    select(Province,Scale,totalcatch,percofcatch,provcatch) %>%
    group_by(Province) %>%
    mutate(provcatch=sum(totalcatch,na.rm=T)) %>%
    filter(Scale=='Municipal') %>%
    ggplot(aes(x=percofcatch,y=provcatch,size=totalcatch)) +
    geom_point() +
    labs(x='% of Provincial Catch from\n Municipal Fisheries', y = 'Total Provincial Catch (MT)',
         title = c(sps[a])) +
    scatterTheme
  print(temp)
}
dev.off()

# Calculate ratio of municpal to commmercial catch by species and region 
# No evidence of consistent ratios...
phils %>%
  select(Province,Scale,Species,Year,Volume) %>%
  filter(Volume>0) %>%
  spread(Scale,Volume) %>%
  mutate(muni_to_comm_ratio = Municipal/Commercial) %>%
  group_by(Species,Year) %>%
  summarize(mean_ratio=mean(muni_to_comm_ratio,na.rm=T)) %>%
  ggplot(aes(x=as.numeric(Year), y=mean_ratio, color = Species)) +
  geom_line() 

## Create a custom color scale for species
myColors <- data_frame(col=rainbow(32),Species=unique(phils$Species))

# Plot species percentage by Province
pdf(file = 'Top 10 Species by Province.pdf',width = 6, height = 6)
provs<-unique(out1$Province)
for(a in 1:length(provs))
{
  temp<-out1 %>%
    left_join(myColors) %>%
    filter(Province == provs[a] & is.na(percofprov)==F) %>%
    arrange(Province,desc(totalcatch)) %>%
    group_by(Province) %>%
    mutate(rank=dense_rank(desc(totalcatch))) %>%
    filter(rank<=9) %>%
    ggplot(aes(x=Province,y=percofprov,fill=col)) +
    geom_bar(stat='identity',position = 'fill')
  
  print(temp)
}
dev.off()

