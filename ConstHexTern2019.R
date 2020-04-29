rm(list=ls())

library(ggtern)
library(tidyverse)
library(curl)
library(readxl)
library(parlitools)
library(tricolore)

temp <- tempfile()
source <- "https://data.parliament.uk/resources/constituencystatistics/general-election-results-2019.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data2019 <- read_excel(temp, sheet="voting-summary")

#Remove non-English constituencies
data2019 <-data2019[,c(1,3,6,19:21)]

data2019$ConProp <- data2019$con/(data2019$con+data2019$ld+data2019$lab)
data2019$LabProp <- data2019$lab/(data2019$con+data2019$ld+data2019$lab)
data2019$LibProp <- data2019$ld/(data2019$con+data2019$ld+data2019$lab)
data2019$year <- 2019
colnames(data2019)[1] <- "id"

#Set up ternary colour scheme
tricolore2019 <- Tricolore(data2019, "LibProp", "ConProp", "LabProp", breaks=100)

#Tidy up the key
key <- tricolore2019$key+
  labs(L='Lib Dem', R='Labour', T='Conservative', x="", y="", z="")+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"))+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"))+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"))

data2019$rgb <- tricolore2019$rgb

#Fetch hex map from {parlitools} package
west_hex_map$id <- west_hex_map$gss_code

map.data <- full_join(west_hex_map, data2019, by="id")

tiff("Outputs/ConstHexTern2019.tiff", units="in", width=14, height=10, res=300)
ggplot()+
  geom_sf(data=subset(map.data, country_name=="England"), aes(geometry=geometry, fill=rgb))+
  scale_fill_identity()+
  theme_void()+
  annotation_custom(
    ggplotGrob(key),
    xmin = 0.5, xmax =6.5, ymin = 52, ymax = 58 )+
  labs(title="Vote share across England for the three main parties in the 2019 General Election",
       subtitle="Data from House of Commons Library | Plot by @VictimOfMaths")
dev.off()
