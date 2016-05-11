
library(dplyr)
library(googleVis)

datSK<-read.csv(file = 'VOI-Model/Data/sankey_phils_input.csv',stringsAsFactors = F)

Sankey <- gvisSankey(datSK, from="From", to="To", weight="Weight",
                     options=list(
                       sankey  ="{link: {color: { fill: '#d799ae' } },
                       node: { color: { fill: '#a61d4c' },
                       label: { color: '#871b47' } }}",
                       width   = '800px',
                       height  = '500px'))
plot(Sankey)
