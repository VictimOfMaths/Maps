rm(list=ls())

library(curl)
library(tidyverse)
library(sf)
library(ragg)
library(extrafont)
library(rnaturalearth)
library(paletteer)
library(forcats)
library(lwgeom)
library(readxl)

font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family=font),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Download osm data with all amenities IN THE WORLD (download is massive)
#THIS IS ABOUT TO BE DEPRECATED, SO GET YOUR DOWNLOAD IN NOW!
url <- "https://data.osmdata.xyz/amenity_EPSG4326.zip"

temp <- tempfile()
temp2 <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
name <- list.files(temp2, pattern=".gpkg")

#Extract all pubs
pubs <- st_read(file.path(temp2, name), layer="amenity_EPSG4326_point",
                query="SELECT * FROM \"amenity_EPSG4326_point\" WHERE amenity IN ('pub')") 

#Extract all bars
bars <- st_read(file.path(temp2, name), layer="amenity_EPSG4326_point",
                query="SELECT * FROM \"amenity_EPSG4326_point\" WHERE amenity IN ('bar')") 

#Extract all nightclubs
clubs <- st_read(file.path(temp2, name), layer="amenity_EPSG4326_point",
                 query="SELECT * FROM \"amenity_EPSG4326_point\" WHERE amenity IN ('nightclub')") 

#Extract all biergartens (suggested by https://wiki.openstreetmap.org/wiki/Tag:amenity%3Dbar)
biergartens <- st_read(file.path(temp2, name), layer="amenity_EPSG4326_point",
                       query="SELECT * FROM \"amenity_EPSG4326_point\" WHERE amenity IN ('biergarten')") 

#Stick it all together
fulldata <- pubs %>% select(osm_id, name, amenity, `_ogr_geometry_`) %>% 
  bind_rows(bars %>% select(osm_id, name, amenity, `_ogr_geometry_`)) %>% 
  bind_rows(clubs %>% select(osm_id, name, amenity, `_ogr_geometry_`)) %>% 
  bind_rows(biergartens %>% select(osm_id, name, amenity, `_ogr_geometry_`))

#Save it out
st_write(fulldata, "Outputs/OSMGlobalBars.shp")

#Bring in map
map <- ne_countries(scale = "large", returnclass = "sf") %>% 
  select(name, sovereignt, sov_a3, pop_est, iso_a3, continent, geometry)

#test <- st_read("X:/ScHARR/SARG_IARP/General/Data/Misc/OpenStreetMap/OSMGlobalBars.shp")

#Place all outlets within a country
countries <- st_join(test, map, join=st_within)

#Get outlets per capita
density <- countries %>% 
  filter(!is.na(name.y)) %>% 
  group_by(name.y) %>% 
  summarise(n=n(), pop_est=unique(pop_est), .groups="drop") %>% 
  mutate(pop_est=as.numeric(pop_est), density=n*100000/pop_est) %>% 
  st_drop_geometry() %>% 
  set_names("name", "outlets", "pop", "density")

densitymap <- left_join(map, density)

agg_png("Day28_2022_ComfortZone.png", units="in", width=9, height=5.5, res=600)
densitymap %>% 
  mutate(density=if_else(is.na(density), 0, density)) %>% 
  filter(density<100 & continent!="Antarctica") %>% 
  st_transform_proj(crs = "ESRI:54030") %>% 
  ggplot(aes(geometry=geometry, fill=density))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, name="Pubs/bars per 100,000 people")+
  theme_custom()+
  theme(legend.position="top", axis.line=element_blank(), 
        axis.ticks=element_blank(), axis.text=element_blank(),
        plot.title=element_text(size=rel(3)))+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Is it far to the nearest bar?",
       subtitle="Per capita density of locations tagged as pubs/bars/nightclubs/biergarten in OpenStreetMap by country",
       caption="Data from OpenStreetMap | Map by @VictimOfMaths")

dev.off()

#Filter only UK data at Local Authority level
#Get shapefile
url2 <- "https://opendata.arcgis.com/api/v3/datasets/420e691a2e8e4db0a0e5acc8ea3d0ce4_0/downloads/data?format=shp&spatialRefId=27700&where=1%3D1"

temp <- tempfile()
temp2 <- tempfile()
temp <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
name <- list.files(temp2, pattern=".shp")

UKmap <- st_read(file.path(temp2, name))

#Reproject global data into GB LTLAs
LACounts <- st_transform(fulldata, crs=27700) %>% 
#Place all outlets within a country
  st_join(UKmap, join=st_within) %>% 
#Get outlets per capita
  filter(!is.na(LAD21CD)) %>% 
  group_by(LAD21CD) %>% 
  summarise(Count=n(), .groups="drop") %>% 
  st_drop_geometry() 

#Join back into the map
LTLAmap <- left_join(UKmap, LACounts)

ggplot(LTLAmap, aes(fill=Count, geometry=geometry))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("viridis::rocket", direction=-1, limits=c(0,NA))+
  theme_void()

#Download Carl Baker's lovely map
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="7 Background")

ltlapubs <- st_read(ltla, layer="6 LTLA-2021") %>% 
  rename("LAD21CD"="Lacode") %>% 
  left_join(LTLAs, by="LAD21CD")

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

ggplot()+
  geom_sf(data=Background, aes(geometry=geom), fill="White")+
  geom_sf(data=ltlapubs, aes(geometry=geom, fill=Count), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("viridis::rocket", direction=-1, limits=c(0,NA))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Lato"))

#Bring in populations
url3 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2021/ukpopestimatesmid2021on2021geographyfinal.xls"
temp <- tempfile()
temp <- curl_download(url=url3, destfile=temp, quiet=FALSE, mode="wb")

pops <- read_excel(temp, sheet="MYE2 - Persons", range="A8:D428") %>% 
  select(c(1,4)) %>% 
  set_names("LAD21CD", "Pop")

PubPerCapita <- ltlapubs %>% 
  left_join(pops, by="LAD21CD") %>% 
  #Merge Scilly with Cornwall and City of London with Hackney, because otherwise their numbers are 
  #So extreme they dwarf everything else
  mutate(LAD21CD=case_when(
    LAD21CD=="E09000001" ~ "E09000012",
    LAD21CD=="E06000053" ~ "E06000052",
    TRUE ~ LAD21CD),
    Laname=case_when(
      Laname %in% c("Cornwall", "Isles of Scilly") ~ "Cornwall & Scilly",
      Laname %in% c("Hackney", "City of London") ~ "Hackney & City",
      TRUE ~ Laname)) %>% 
  group_by(LAD21CD, Laname) %>% 
  summarise(Count=sum(Count), Pop=sum(Pop), .groups="drop") %>% 
  mutate(ppc=Count*100000/Pop)

agg_tiff("Outputs/OSMPubsxLACartogram.tiff", units="in", width=6, height=8, res=600)
ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom), fill="White")+
  geom_sf(data=PubPerCapita %>% filter(substr(LAD21CD, 1, 1)!="N"), 
          aes(geometry=geom, fill=ppc), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(Group!="Northern Ireland"), aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(Group.labe!="Northern Ireland"), 
               aes(geometry=geom, label=Group.labe, hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("viridis::mako", direction=-1, limits=c(0,NA),
                         name="Pubs per\n100,000\npopulation")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Lato"))+
  labs(title="How many locals for the locals?",
       subtitle="Number of pubs and bars per capita in Great Britain",
       caption="Data from OpenStreetMap\nCartogram by Carl Baker\nMap by @VictimOfMaths")

dev.off()
