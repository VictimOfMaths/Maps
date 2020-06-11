rm(list=ls())

library(tidyverse)
library(stringr)
library(sf)
library(curl)

#Read in pubs data - data is not open - was purchased from market research company CGA Strategy
raw.on <- read.csv("")

#Read in postcode data - dataset of every full postcode in the UK with centroid
PClookup <- read.csv("")

#Remove whitespace from postcodes for matching
PClookup$pc <- str_replace_all(PClookup$postcode, " ", "")
raw.on$pc <- str_replace_all(raw.on$OT_Postcode, " ", "")

raw.on <- merge(raw.on, PClookup, by="pc", all.x=TRUE)

#Download shapefile of LA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/6638c31a8e9842f98a037748f72258ed_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

tiff("Outputs/CGAPubs2016.tiff", units="in", width=7, height=9, res=300)
ggplot(data=shapefile)+
  geom_sf(aes(geometry=geometry), fill=NA, colour=NA)+
  geom_point(data=subset(raw.on, OT_SubLicenceDescription=="Pubs"), 
             aes(x=oseast1m, y=osnrth1m), shape=".", colour="seagreen")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(), plot.title=element_text(margin=margin(t=0, b=-70)),
        plot.title.position="plot", plot.subtitle=element_text(vjust=-32))+
  labs(title="Great Britain in pubs",
       subtitle="Location of every pub in Great Britain in 2016",
       caption="Data from CGA Strategy | Plot by @VictimOfMaths")
dev.off()
