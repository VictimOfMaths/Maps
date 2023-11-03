rm(list=ls())

library(sf)
library(ragg)
library(extrafont)
library(tidyverse)
library(curl)
library(xml2)
library(rvest)
library(ggrepel)

#Grab html table of Premier league grounds from Wikipedia
EngGroundurl <- "https://en.wikipedia.org/wiki/List_of_Premier_League_stadiums"
temp <- EngGroundurl %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
England <- as.data.frame(html_table(temp[1])) %>% 
  select(Club, Stadium, Capacity) %>% 
  mutate(Stadium=sub("\\(also.*", "", Stadium),
         Stadium=sub("Formerly.*", "", Stadium),
         Capacity=sub("\\[.*", "", Capacity),
         Capacity=as.numeric(gsub(",", "", Capacity)))

EngCoords <- "https://tools.wmflabs.org/kmlexport?article=List_of_Premier_League_stadiums"

temp <- tempfile()
temp <- curl_download(url=EngCoords, destfile=temp, quiet=FALSE, mode="wb")

England <- England %>% 
  merge(st_read(temp) %>% 
          select(-Description) %>% 
          rename("Stadium"="Name"),all=TRUE)

England2324 <- England %>% 
  filter(Stadium %in% c("Emirates Stadium", "Villa Park", "Dean Court", "Brentford Community Stadium",
                        "Falmer Stadium", "Turf Moor", "Stamford Bridge", "Selhurst Park", 
                        "Goodison Park", "Craven Cottage", "Anfield", "Kenilworth Road",
                        "City of Manchester Stadium", "Old Trafford", 
                        "St James' Park", "City Ground", "Bramall Lane", "Tottenham Hotspur Stadium",
                        "London Stadium", "Molineux Stadium")) %>%
  group_by(Stadium) %>% 
  slice(1) %>% 
  st_as_sf()%>% 
  st_transform(crs=27700)

#Scottish Premier League grounds
ScotGroundurl <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_Scotland"
temp <- ScotGroundurl %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Scotland <- as.data.frame(html_table(temp[3])) %>% 
  select(Team, Capacity, `Stadium.notes.1.`) %>% 
  set_names("Club", "Capacity", "Stadium") %>% 
  mutate(Capacity=as.numeric(gsub(",", "", Capacity)),
         Stadium=if_else(Stadium=="Tynecastle Park", "Tynecastle Stadium", Stadium))

ScotCoords <- "https://tools.wmflabs.org/kmlexport?article=List_of_football_stadiums_in_Scotland"

temp <- tempfile()
temp <- curl_download(url=ScotCoords, destfile=temp, quiet=FALSE, mode="wb")

Scotland <- Scotland %>% 
  merge(st_read(temp) %>% 
          select(-Description) %>% 
          rename("Stadium"="Name"),all=TRUE)

Scotland2324 <- Scotland %>% 
  filter(Stadium %in% c("Pittodrie Stadium", "Celtic Park", "Dens Park",
                        "Tynecastle Stadium", "Easter Road","Rugby Park", 
                        "Almondvale Stadium", "Fir Park", "Ibrox Stadium", "Victoria Park", 
                        "McDiarmid Park", "St Mirren Park")) %>% 
  filter(!Club %in% c("Vale of Leithen F.C.", "Buckie Thistle F.C.")) %>% 
  group_by(Stadium) %>% 
  slice(1) %>% 
  st_as_sf()%>% 
  st_transform(crs=27700)

EngScot <- bind_rows(England2324, Scotland2324)

#Download outline map
temp <- tempfile()
temp2 <- tempfile()
source <- "https://stg-arcgisazurecdataprod1.az.arcgis.com/exportfiles-1559-18061/NUTS_Level_1_January_2018_FCB_in_the_United_Kingdom_2022_7279368953270783580.zip?sv=2018-03-28&sr=b&sig=V9zAYVlR032Cgy5GCGf1MDk6QN%2FNBZZMuCFbfWznb6g%3D&se=2023-11-03T11%3A46%3A41Z&sp=r"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
outline <- st_read(file.path(temp2, name)[1]) %>% 
  summarise()

#Generate voronoi polygons
voronoi <- EngScot %>% 
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()

# Put them back in their original order
voronoi <- voronoi[unlist(st_intersects(EngScot,voronoi))]

voronoi <- st_intersection(st_cast(voronoi), outline, col=0)

voronoi <- EngScot %>% 
  st_combine() %>% 
  st_voronoi() %>% 
  st_cast() %>% 
  st_intersection(outline) %>%
  st_cast() %>% 
  st_sf()

agg_png("Day3_2023_Polygons.png", units="in", width=8, height=9, res=700, background="lightcyan")
ggplot()+
  geom_sf(data=outline, aes(geometry=geometry), fill="White", colour=NA)+
  geom_sf(data=voronoi, aes(geometry=geometry, fill=rownames(voronoi)), colour="Black",
          size=0.2, show.legend=FALSE)+
  geom_sf(data=EngScot, aes(geometry=geometry, size=Capacity), shape=21, 
          colour="#38003c", fill="White", alpha=0.6)+
  scale_fill_manual(values=c("#2f368f", "#FBBA2D", "#FFCC00", "#231F20", "#9F1931",
                             "#00753B", "#DA291C", "#C91318", "#FDB913", "#C8102E",
                             "#E2001a", "#040957", "#EE2737", "#97C1E7", "#DD0000",
                             "#670E36", "#F78F1E", "white", "#D20000", "#EF0107",
                             "#034694","#1B458F","#003399","white","#7A263A",
                             "#0057B8","#6C1D45","black","#1b458f","#1a315a",
                             "#018749","#243f90"))+
  scale_size(name="Stadium Capacity")+
  xlim(-200000, 800000)+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), text=element_text(family="Merriweather"),
        plot.title=element_text(face="bold", size=rel(2)),
        plot.title.position="plot",
        plot.caption.position="plot", plot.background=element_rect(fill="lightcyan", colour="lightcyan"),
        panel.background=element_rect(fill="lightcyan", colour="lightcyan"), 
        legend.background=element_rect(fill="lightcyan", colour="lightcyan"))+
  labs(title="Support your local (Premier League) team",
       subtitle="The nearest current English or Scottish Premier League club to every point in the UK",
       caption="Stadium data from Wikipedia | Map by @VictimOfMaths")
dev.off()
