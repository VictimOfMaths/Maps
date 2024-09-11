rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(sf)
library(extrafont)
library(ragg)
library(paletteer)

#2019
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/5d8b3abded915d0373d3540f/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

IMD19 <- read_excel(temp, sheet="IMD2019", range="A1:F32845") %>% 
  select(1,2,5) %>% 
  set_names("LSOA11CD", "LSOA11Name", "IMD19Rank")

#2015
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/5a805f96ed915d74e33fa0df/File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

IMD15 <- read_excel(temp, sheet="IMD 2015", range="A1:F32845")%>% 
  select(1,2,5) %>% 
  set_names("LSOA11CD", "LSOA11Name", "IMD15Rank")

#2010
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/5a79b8c6ed915d042206a8e2/1871524.xls"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

IMD10 <- read_excel(temp, sheet="IMD 2010", range="A1:G32483") %>% 
  select(1,2,7) %>% 
  set_names("LSOA01CD", "LSOA01Name", "IMD10Rank")

#2007
temp <- tempfile()
temp2 <- tempfile()
source <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20100411141238mp_/http://www.communities.gov.uk/documents/communities/zip/indices2007.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

IMD07 <- read_excel(file.path(temp2, "IMD 2007 for DCLG 4 dec.xls"),
                    sheet="IMD 2007", range=c("A1:G32483")) %>% 
  select(1,2,7) %>% 
  set_names("LSOA01CD", "LSOA01Name", "IMD07Rank")

#2004
temp <- tempfile()
temp2 <- tempfile()
source <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20100407164233mp_/http://www.communities.gov.uk/documents/communities/zip/soalevelid.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

IMD04 <- read_excel(file.path(temp2, "SOA levelid2004.xls"),
                    sheet="IMD 2004", range=c("A1:G32483"))%>% 
  select(1,2,7) %>% 
  set_names("LSOA01CD", "LSOA01Name", "IMD04Rank")

#Read in LSOA2001 - LSOA2011 code lookup
temp <- tempfile()
url <- "https://opendata.arcgis.com/api/v3/datasets/3dd1bc5dd053426aa84a068c7afbb3b2_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

LSOA0111Lookup <- read.csv(temp) %>% 
  select(LSOA01CD, LSOA11CD, LAD11NM)

#Stick it all together on LSOA11s
LSOAdata <- LSOA0111Lookup %>% 
  merge(IMD04 %>% select(-LSOA01Name), all.x=T) %>% 
  merge(IMD07 %>% select(-LSOA01Name), all.x=T) %>% 
  merge(IMD10 %>% select(-LSOA01Name), all.x=T) %>% 
  merge(IMD15 %>% select(-LSOA11Name), all.x=T) %>% 
  merge(IMD19 %>% select(-LSOA11Name), all.x=T) %>% 
  filter(substr(LSOA11CD,1,1)=="E") %>% 
  #Combine LSOAs by taking simple mean of ranks where necessary (not many)
  group_by(LSOA11CD, LAD11NM) %>% 
  summarise(across(IMD04Rank:IMD19Rank, ~mean(.x)), .groups="drop") %>% 
  #Recalculate ranks
  arrange(IMD04Rank) %>% 
  mutate(IMD04Rank=1:nrow(.)) %>% 
  arrange(IMD07Rank) %>% 
  mutate(IMD07Rank=1:nrow(.)) %>%  
  arrange(IMD10Rank) %>% 
  mutate(IMD10Rank=1:nrow(.)) %>%  
  arrange(IMD15Rank) %>% 
  mutate(IMD15Rank=1:nrow(.)) %>% 
  arrange(IMD19Rank) %>% 
  mutate(IMD19Rank=1:nrow(.)) %>%
  mutate(Change0419=IMD19Rank-IMD04Rank)

#Bring in LSOA shapefile
#Read in shapefile of LA boundaries
#Download shapefile of LA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://stg-arcgisazurecdataprod1.az.arcgis.com/exportfiles-1559-15681/Lower_Layer_Super_Output_Areas_Dec_2011_Boundaries_Full_Extent_BFE_EW_V3_2022_6804401862095015907.zip?sv=2018-03-28&sr=b&sig=tDtStx3vlVnYfNKVQpx9y6qHHJxgyjuWz26qRG30QU4%3D&se=2024-09-11T14%3A08%3A51Z&sp=r"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

#Merge shapefile with LSOA level data
LSOA.map.data <- full_join(shapefile, LSOAdata, by="LSOA11CD")

#Plot map for any LAD of your choice (LSOA is too small a geography for a national map)
#Just put your Local Authority name of choice in here:
LSOAPlotLA <- "Sheffield"

LSOAPlotScale <- LSOA.map.data %>% filter(LAD11NM==LSOAPlotLA) %>% 
  st_drop_geometry() %>% 
  slice(which.max(abs(Change0419))) %>% 
  pull(Change0419) 
LSOAPlotBoundary <- LSOA.map.data %>% filter(LAD11NM==LSOAPlotLA) %>% 
  st_union()

agg_png(paste0("Outputs/LSOAIMDChange", LSOAPlotLA, ".png"), units="in", width=8.5, height=6, res=600)
LSOA.map.data %>% filter(LAD11NM==LSOAPlotLA) %>% 
  ggplot(aes(fill=Change0419))+
  geom_sf(colour="transparent")+
  geom_sf(data=LSOAPlotBoundary, colour="grey20", fill="transparent")+ 
  scale_fill_paletteer_c("pals::ocean.curl", name="IMD rank change\n2004-19", direction=-1,
                         limit=c(-1,1)*abs(LSOAPlotScale))+
  theme_void()+
  theme( plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                 margin=margin(0,0,5.5,0)),
         text=element_text(family="Lato"),
         plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
         plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)))+
  labs(title=paste("Changes in deprivation across ", LSOAPlotLA, " between 2004 and 2019"),
       subtitle="Change in rank of Lower Super Outputs Areas between IMD2004 and IMD2019\nIncreases in rank equate to reductions in relative deprivation\n",
       caption="Data from MHCLG and ONS | Plot by @VictimOfMaths")

dev.off()
