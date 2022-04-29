rm(list=ls())

library(googlesheets4)
library(tidyverse)
library(sf)
library(curl)
library(extrafont)
library(ragg)
library(ggtext)
library(paletteer)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Import data on biggest issues by parliamentary consituency
#https://www.royalholloway.ac.uk/about-us/news/royal-holloway-and-survation-launch-new-tracker-revealing-important-issues-across-great-britain-s-parliamentary-constituencies/
gs4_deauth()
data <- read_sheet("https://docs.google.com/spreadsheets/d/1oGsSNopxoRwbG2sVJpAY4a8kKYVZxlmcfVVcyGdJUv4") %>% 
  rename("pcon.code"="ONSConstID")

#Download Carl Bakers lovely hex cartogram template
map <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/Constituencies.gpkg")
map <- curl_download(url=source, destfile=map, quiet=FALSE, mode="wb")

Background <- st_read(map, layer="5 Background")

mapdata <- st_read(map, layer="4 Constituencies") %>% 
  left_join(data, by="pcon.code")

Groups <- st_read(map, layer="2 Group outlines")

Cities <- st_read(map, layer="3 City outlines")

Group_labels <- st_read(map, layer="1 Group names") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

plot1 <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom), fill="White")+
  geom_sf(data=mapdata %>% filter(RegionNati!="Northern Ireland"), 
          aes(geometry=geom, fill=`Most important issue`), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNati!="Northern Ireland"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf(data=Cities %>% filter(RegionNati!="Northern Ireland"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNati!="Northern Ireland"), 
               aes(geometry=geom, label=Group.labe, hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_d("ggthemr::flat")+
  theme_void()+
  theme(plot.title=element_markdown(face="bold", size=rel(1.2)),
        text=element_text(family="Lato"))+
  labs(title="Cities care about COVID, rural areas about the economy",
       subtitle="The most important issue affecting people in each parlimentary constituency in Great Britain\nData from Feb/Mar 2022",
       caption="Data from Royal Holloway/Survation\nPlot by @VictimOfMaths")

agg_tiff("Outputs/GBImportantIssues.tiff", units="in", width=7, height=8, res=500)
plot1
dev.off()
