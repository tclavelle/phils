########################################################################
### map summary data for Philippines -----------------------------------

## Read in province and municipal shapefiles and tidy to a spatial data frame for use with ggplot
# shapefile map data
philsmap<-readOGR(dsn = 'voi/Provinces', layer = 'Provinces', stringsAsFactors = F)
munimap<-readOGR(dsn = 'voi/MuniCities', layer = 'MuniCities', stringsAsFactors = F)

# read data of TURF-able municipalities
muniturf<-read_excel(path = 'voi/phils-data-antonius/MWD_Table.xlsx', sheet = 2)

# lookup table with provinces and municipal ids for joining
lktable<-data.frame(Province=philsmap@data$NAME_1, id=philsmap@data$ID_1)
lktable2<-data.frame(Municipality=munimap@data$NAME_2, id=munimap@data$ID_2)


# convert spatial data frame to a format for ggplot
philsmap<-tidy(philsmap, region = NULL)

# adjust lookup table to account for ERROR with fortify that changes polygon id numbers
lktable<-lktable %>%
  mutate(
    id = as.character(lag(id)) # lag id numbers to account for the 0 added by the tidy function
  )

lktable[1,2]<-"0" # replace NA for first polygon after lagging




# map % catch from municipal
a<-a %>%
  select(Province,percofprov) %>%
  right_join(lktable) %>%
  mutate(id=as.character(id)) %>%
  dplyr::select(-Province)

philsmap<-left_join(philsmap,a)

plotA<-philsmap %>%
  ggplot(aes(x = long, y = lat, group = group, fill = percofprov)) +
  geom_polygon() 
# scale_fill_brewer(palette = 'Blues')

