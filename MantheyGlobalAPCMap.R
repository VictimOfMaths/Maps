rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(rnaturalearth)
library(paletteer)

temp <- tempfile()
source <- "https://ars.els-cdn.com/content/image/1-s2.0-S0140673618327442-mmc2.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read_excel(temp, sheet="Tab 2", range="A2:Q2270", col_names=TRUE)

#Keep only total per capita consumption in 2017
data <- data %>% 
  filter(year==2017 & sex=="TOTAL")

#Bring in shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")
names(world)[names(world) == "iso_a3"] <- "iso3a"

map <- full_join(world, data, by="iso3a", all.y=TRUE)

png("Outputs/GlobalAPCBW.png", units="in", width=14, height=10, res=500)
ggplot(map)+
  geom_sf(aes(geometry=geometry, fill=APC), colour=NA, show.legend=FALSE)+
  scale_fill_paletteer_c("oompaBase::greyscale", na.value="transparent")+
  theme(plot.background=element_rect(fill="black"), panel.background=element_rect(fill="black"),
        panel.grid.major=element_line(colour="transparent"))
dev.off()
