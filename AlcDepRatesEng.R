rm(list=ls())

library(tidyverse)
library(curl)
library(readODS)
library(sf)
library(paletteer)
library(scales)

#Read in dependence data
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/846690/Estimates_of_Alcohol_Dependent_Adults_in_England_2017-18.ods"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read_ods(temp, sheet=2, range="A4:J154", col_names=FALSE) %>% 
  select(c(A, B, C, H)) %>% 
  rename(Region=A, LA=B, ctyua17cd=C, deprate=H)

#Read in shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/6638c31a8e9842f98a037748f72258ed_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

#Bring in data
map.data <- full_join(shapefile, data, by="ctyua17cd")

tiff("Outputs/AlcDepRateEng.tiff", units="in", width=9, height=10, res=500)
map.data %>% 
  filter(!is.na(deprate)) %>% 
  ggplot(aes(geometry=geometry, fill=deprate/100))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("viridis::magma", limits=c(0,NA), direction=-1, name="Dependence rate",
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.3)))+
  labs(title="Blackpool has a much higher rate of alcohol dependence than the rest of England",
       subtitle="Proportion of adults estimated to be dependent on alcohol in English Upper Tier Local Authorities in 2017/18",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()
