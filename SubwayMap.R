rm(list=ls())

library(sf)
library(ragg)
library(extrafont)
library(tidyverse)
library(osmdata)
library(ggtext)

#Version 1 - Glasgow
#Get data, queries built with http://overpass-turbo.eu/
data <- getbb("Glasgow") %>% 
  opq(timeout=25*100) %>% 
  add_osm_feature(key="public_transport", value="station") %>% 
  osmdata_sf() 

mapdata <- data$osm_points %>% filter(network=="Glasgow Subway")

agg_tiff("GlasgowSubway.tiff", units="in", width=8, height=7, res=700, background="Black")
ggplot(mapdata, aes(geometry=geometry))+
  geom_sf(colour="#F57C14", size=rel(12))+
  labs(caption="Inspired by @barelymaps\nMap by @VictimOfMaths\n")+
  theme_void()+
  theme(legend.position="none", plot.background=element_rect(fill="Black", colour="Black"),
        plot.caption.position="plot", plot.caption=element_text(family="Helvetica",
                                                                colour="White"))+
  annotate("text", x=-4.285, y=55.845, label="Glasgow", family="Helvetica", colour="White",
           size=rel(10), fontface=2)

dev.off()

#Version 2 - WIP London
data <- getbb("London") %>% 
  opq(timeout=25*100) %>% 
  add_osm_feature(key="public_transport", value="station") %>% 
  osmdata_sf() 

mapdata <- data$osm_points %>% filter(network=="London Underground") %>% 
  arrange(line) %>% 
  separate(line, sep=";", into=c("line1", "line2", "line3", "line4", "line5", "line6")) %>% 
  select(network, geometry, line1, line2, line3, line4, line5, line6) %>% 
  mutate(size2=case_when(
    is.na(line3) ~ 0.3, is.na(line4) ~ 0.5, is.na(line5) ~ 0.6, is.na(line6) ~ 0.7,
    TRUE ~ 0.8))

agg_tiff("LondonUnderground.tiff", units="in", width=8, height=7, res=700, background="Grey10")
ggplot()+
  geom_sf(data=mapdata %>% filter(!is.na(line1)), aes(geometry=geometry, colour=line1), 
          size=rel(2))+
  geom_sf(data=mapdata %>% filter(!is.na(line2)), aes(geometry=geometry, colour=line2,
                                                      size=size2))+
  geom_sf(data=mapdata %>% filter(!is.na(line3)), aes(geometry=geometry, colour=line3), 
          size=rel(0.9))+
  geom_sf(data=mapdata %>% filter(!is.na(line4)), aes(geometry=geometry, colour=line4), 
          size=rel(0.55))+
  geom_sf(data=mapdata %>% filter(!is.na(line5)), aes(geometry=geometry, colour=line5), 
          size=rel(0.3))+
  geom_sf(data=mapdata %>% filter(!is.na(line6)), aes(geometry=geometry, colour=line6), 
          size=rel(0.05))+
  scale_colour_manual(values=c("#b26300", "#dc241f", "#ffd329", "#007d32", "#f4a9be",
                               "#a1a5a7", "#9b0058", "#000000", "#0019a8", "#0098d8",
                               "#93ceba"))+
  theme_void()+
  theme(legend.position="none", plot.background=element_rect(fill="Grey10", colour="Grey10"))+
  annotate("text", x=-0.16, y=51.35, label="London", family="Helvetica", colour="White",
           size=rel(10), fontface=2)
dev.off()


