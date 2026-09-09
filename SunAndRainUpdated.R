rm(list=ls())

#library(ncdf4)
library(raster)
library(sf)
library(tidyverse)
library(paletteer)
library(extrafont)
library(ragg)
library(ggtext)
library(gtools)
library(cowplot)

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

#Read in data in netCDF format and convert to raster (the error messages seem to not matter, don't ask me)
#Approach borrowed from https://rpubs.com/boyerag/297592
#Data from CEDA
#hhttps://data.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.3.2.ceda/1km/rainfall/ann/v20260512
#http://dx.doi.org/10.5285/bbca3267dc7d4219af484976734c9527

#Download data from 2020-2025
#2020 Rainfall
nc.rain20 <- raster("Data/CEDA Data/rainfall_hadukgrid_uk_1km_ann_202001-202012.nc", varname="rainfall")
proj4string(nc.rain20)=CRS("+init=EPSG:27700")
plot(nc.rain20)
writeRaster(nc.rain20, "Data/CEDA Data/Rainfall20.tif", "GTiff", overwrite=TRUE)

#2021 Rainfall
nc.rain21 <- raster("Data/CEDA Data/rainfall_hadukgrid_uk_1km_ann_202101-202112.nc", varname="rainfall")
proj4string(nc.rain21)=CRS("+init=EPSG:27700")
plot(nc.rain21)
writeRaster(nc.rain21, "Data/CEDA Data/Rainfall21.tif", "GTiff", overwrite=TRUE)

#2022 Rainfall
nc.rain22 <- raster("Data/CEDA Data/rainfall_hadukgrid_uk_1km_ann_202201-202212.nc", varname="rainfall")
proj4string(nc.rain22)=CRS("+init=EPSG:27700")
plot(nc.rain22)
writeRaster(nc.rain22, "Data/CEDA Data/Rainfall22.tif", "GTiff", overwrite=TRUE)

#2023 Rainfall
nc.rain23 <- raster("Data/CEDA Data/rainfall_hadukgrid_uk_1km_ann_202301-202312.nc", varname="rainfall")
proj4string(nc.rain23)=CRS("+init=EPSG:27700")
plot(nc.rain23)
writeRaster(nc.rain23, "Data/CEDA Data/Rainfall23.tif", "GTiff", overwrite=TRUE)

#2024 Rainfall
nc.rain24 <- raster("Data/CEDA Data/rainfall_hadukgrid_uk_1km_ann_202401-202412.nc", varname="rainfall")
proj4string(nc.rain24)=CRS("+init=EPSG:27700")
plot(nc.rain24)
writeRaster(nc.rain24, "Data/CEDA Data/Rainfall24.tif", "GTiff", overwrite=TRUE)

#2025 Rainfall
nc.rain25 <- raster("Data/CEDA Data/rainfall_hadukgrid_uk_1km_ann_202501-202512.nc", varname="rainfall")
proj4string(nc.rain25)=CRS("+init=EPSG:27700")
plot(nc.rain25)
writeRaster(nc.rain25, "Data/CEDA Data/Rainfall25.tif", "GTiff", overwrite=TRUE)

#####
#https://data.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.3.2.ceda/1km/sun/ann/v20260512

#2020 Sunshine
nc.sun20 <- raster("Data/CEDA Data/sun_hadukgrid_uk_1km_ann_202001-202012.nc", varname="sun")
proj4string(nc.sun20)=CRS("+init=EPSG:27700")
plot(nc.sun20)
writeRaster(nc.sun20, "Data/CEDA Data/Sunshine20.tif", "GTiff", overwrite=TRUE)

#2021 Sunshine
nc.sun21 <- raster("Data/CEDA Data/sun_hadukgrid_uk_1km_ann_202101-202112.nc", varname="sun")
proj4string(nc.sun21)=CRS("+init=EPSG:27700")
plot(nc.sun21)
writeRaster(nc.sun21, "Data/CEDA Data/Sunshine21.tif", "GTiff", overwrite=TRUE)

#2022 Sunshine
nc.sun22 <- raster("Data/CEDA Data/sun_hadukgrid_uk_1km_ann_202201-202212.nc", varname="sun")
proj4string(nc.sun22)=CRS("+init=EPSG:27700")
plot(nc.sun22)
writeRaster(nc.sun22, "Data/CEDA Data/Sunshine22.tif", "GTiff", overwrite=TRUE)

#2023 Sunshine
nc.sun23 <- raster("Data/CEDA Data/sun_hadukgrid_uk_1km_ann_202301-202312.nc", varname="sun")
proj4string(nc.sun23)=CRS("+init=EPSG:27700")
plot(nc.sun23)
writeRaster(nc.sun23, "Data/CEDA Data/Sunshine23.tif", "GTiff", overwrite=TRUE)

#2024 Sunshine
nc.sun24 <- raster("Data/CEDA Data/sun_hadukgrid_uk_1km_ann_202401-202412.nc", varname="sun")
proj4string(nc.sun24)=CRS("+init=EPSG:27700")
plot(nc.sun24)
writeRaster(nc.sun24, "Data/CEDA Data/Sunshine24.tif", "GTiff", overwrite=TRUE)

#2025 Sunshine
nc.sun25 <- raster("Data/CEDA Data/sun_hadukgrid_uk_1km_ann_202501-202512.nc", varname="sun")
proj4string(nc.sun25)=CRS("+init=EPSG:27700")
plot(nc.sun25)
writeRaster(nc.sun25, "Data/CEDA Data/Sunshine25.tif", "GTiff", overwrite=TRUE)

#Bivariate map of rain and sun coverage
rain20.df <- as.data.frame(as(nc.rain20, "SpatialPixelsDataFrame"))
rain21.df <- as.data.frame(as(nc.rain21, "SpatialPixelsDataFrame"))
rain22.df <- as.data.frame(as(nc.rain22, "SpatialPixelsDataFrame"))
rain23.df <- as.data.frame(as(nc.rain23, "SpatialPixelsDataFrame"))
rain24.df <- as.data.frame(as(nc.rain24, "SpatialPixelsDataFrame"))
rain25.df <- as.data.frame(as(nc.rain25, "SpatialPixelsDataFrame"))

sun20.df <-  as.data.frame(as(nc.sun20, "SpatialPixelsDataFrame"))
sun21.df <-  as.data.frame(as(nc.sun21, "SpatialPixelsDataFrame"))
sun22.df <-  as.data.frame(as(nc.sun22, "SpatialPixelsDataFrame"))
sun23.df <-  as.data.frame(as(nc.sun23, "SpatialPixelsDataFrame"))
sun24.df <-  as.data.frame(as(nc.sun24, "SpatialPixelsDataFrame"))
sun25.df <-  as.data.frame(as(nc.sun25, "SpatialPixelsDataFrame"))

rainvssun20.df <- merge(rain20.df, sun20.df) %>% 
  mutate(raintert=quantcut(Total.rainfall, q=3, labels=FALSE),
         suntert=quantcut(Sunshine.hours, q=3, labels=FALSE),
         key=case_when(
           raintert==1 & suntert==1 ~ 1,
           raintert==2 & suntert==1 ~ 2,
           raintert==3 & suntert==1 ~ 3,
           raintert==1 & suntert==2 ~ 4,
           raintert==2 & suntert==2 ~ 5,
           raintert==3 & suntert==2 ~ 6,
           raintert==1 & suntert==3 ~ 7,
           raintert==2 & suntert==3 ~ 8,
           raintert==3 & suntert==3 ~ 9),
         colour=case_when(
           key==1 ~ "#f3f3f3", key==2 ~ "#b4d3e1", key==3 ~ "#509dc2",
           key==4 ~ "#f3e6b3", key==5 ~ "#b3b3b3", key==6 ~ "#376387",
           key==7 ~ "#f3b300", key==8 ~ "#b36600", key==9 ~ "#000000"))

rainvssun21.df <- merge(rain21.df, sun21.df) %>% 
  mutate(raintert=quantcut(Total.rainfall, q=3, labels=FALSE),
         suntert=quantcut(Sunshine.hours, q=3, labels=FALSE),
         key=case_when(
           raintert==1 & suntert==1 ~ 1,
           raintert==2 & suntert==1 ~ 2,
           raintert==3 & suntert==1 ~ 3,
           raintert==1 & suntert==2 ~ 4,
           raintert==2 & suntert==2 ~ 5,
           raintert==3 & suntert==2 ~ 6,
           raintert==1 & suntert==3 ~ 7,
           raintert==2 & suntert==3 ~ 8,
           raintert==3 & suntert==3 ~ 9),
         colour=case_when(
           key==1 ~ "#f3f3f3", key==2 ~ "#b4d3e1", key==3 ~ "#509dc2",
           key==4 ~ "#f3e6b3", key==5 ~ "#b3b3b3", key==6 ~ "#376387",
           key==7 ~ "#f3b300", key==8 ~ "#b36600", key==9 ~ "#000000"))

rainvssun22.df <- merge(rain22.df, sun22.df) %>% 
  mutate(raintert=quantcut(Total.rainfall, q=3, labels=FALSE),
         suntert=quantcut(Sunshine.hours, q=3, labels=FALSE),
         key=case_when(
           raintert==1 & suntert==1 ~ 1,
           raintert==2 & suntert==1 ~ 2,
           raintert==3 & suntert==1 ~ 3,
           raintert==1 & suntert==2 ~ 4,
           raintert==2 & suntert==2 ~ 5,
           raintert==3 & suntert==2 ~ 6,
           raintert==1 & suntert==3 ~ 7,
           raintert==2 & suntert==3 ~ 8,
           raintert==3 & suntert==3 ~ 9),
         colour=case_when(
           key==1 ~ "#f3f3f3", key==2 ~ "#b4d3e1", key==3 ~ "#509dc2",
           key==4 ~ "#f3e6b3", key==5 ~ "#b3b3b3", key==6 ~ "#376387",
           key==7 ~ "#f3b300", key==8 ~ "#b36600", key==9 ~ "#000000"))

rainvssun23.df <- merge(rain23.df, sun23.df) %>% 
  mutate(raintert=quantcut(Total.rainfall, q=3, labels=FALSE),
         suntert=quantcut(Sunshine.hours, q=3, labels=FALSE),
         key=case_when(
           raintert==1 & suntert==1 ~ 1,
           raintert==2 & suntert==1 ~ 2,
           raintert==3 & suntert==1 ~ 3,
           raintert==1 & suntert==2 ~ 4,
           raintert==2 & suntert==2 ~ 5,
           raintert==3 & suntert==2 ~ 6,
           raintert==1 & suntert==3 ~ 7,
           raintert==2 & suntert==3 ~ 8,
           raintert==3 & suntert==3 ~ 9),
         colour=case_when(
           key==1 ~ "#f3f3f3", key==2 ~ "#b4d3e1", key==3 ~ "#509dc2",
           key==4 ~ "#f3e6b3", key==5 ~ "#b3b3b3", key==6 ~ "#376387",
           key==7 ~ "#f3b300", key==8 ~ "#b36600", key==9 ~ "#000000"))

rainvssun24.df <- merge(rain24.df, sun24.df) %>% 
  mutate(raintert=quantcut(Total.rainfall, q=3, labels=FALSE),
         suntert=quantcut(Sunshine.hours, q=3, labels=FALSE),
         key=case_when(
           raintert==1 & suntert==1 ~ 1,
           raintert==2 & suntert==1 ~ 2,
           raintert==3 & suntert==1 ~ 3,
           raintert==1 & suntert==2 ~ 4,
           raintert==2 & suntert==2 ~ 5,
           raintert==3 & suntert==2 ~ 6,
           raintert==1 & suntert==3 ~ 7,
           raintert==2 & suntert==3 ~ 8,
           raintert==3 & suntert==3 ~ 9),
         colour=case_when(
           key==1 ~ "#f3f3f3", key==2 ~ "#b4d3e1", key==3 ~ "#509dc2",
           key==4 ~ "#f3e6b3", key==5 ~ "#b3b3b3", key==6 ~ "#376387",
           key==7 ~ "#f3b300", key==8 ~ "#b36600", key==9 ~ "#000000"))

rainvssun25.df <- merge(rain25.df, sun25.df) %>% 
  mutate(raintert=quantcut(Total.rainfall, q=3, labels=FALSE),
         suntert=quantcut(Sunshine.hours, q=3, labels=FALSE),
         key=case_when(
           raintert==1 & suntert==1 ~ 1,
           raintert==2 & suntert==1 ~ 2,
           raintert==3 & suntert==1 ~ 3,
           raintert==1 & suntert==2 ~ 4,
           raintert==2 & suntert==2 ~ 5,
           raintert==3 & suntert==2 ~ 6,
           raintert==1 & suntert==3 ~ 7,
           raintert==2 & suntert==3 ~ 8,
           raintert==3 & suntert==3 ~ 9),
         colour=case_when(
           key==1 ~ "#f3f3f3", key==2 ~ "#b4d3e1", key==3 ~ "#509dc2",
           key==4 ~ "#f3e6b3", key==5 ~ "#b3b3b3", key==6 ~ "#376387",
           key==7 ~ "#f3b300", key==8 ~ "#b36600", key==9 ~ "#000000"))

#generate dataframe for key
keydata20 <- rainvssun20.df %>%
  filter(!is.na(colour)) %>%
  group_by(raintert, suntert) %>%
  summarise(RGB=unique(colour))

keydata21 <- rainvssun21.df %>%
  filter(!is.na(colour)) %>%
  group_by(raintert, suntert) %>%
  summarise(RGB=unique(colour))

keydata22 <- rainvssun22.df %>%
  filter(!is.na(colour)) %>%
  group_by(raintert, suntert) %>%
  summarise(RGB=unique(colour))

keydata23 <- rainvssun23.df %>%
  filter(!is.na(colour)) %>%
  group_by(raintert, suntert) %>%
  summarise(RGB=unique(colour))

keydata24 <- rainvssun24.df %>%
  filter(!is.na(colour)) %>%
  group_by(raintert, suntert) %>%
  summarise(RGB=unique(colour))

keydata25 <- rainvssun25.df %>%
  filter(!is.na(colour)) %>%
  group_by(raintert, suntert) %>%
  summarise(RGB=unique(colour))

key20 <- ggplot(keydata20)+
  geom_tile(aes(x=raintert, y=suntert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More rain" %->%  ""),
       y = expression("More sun" %->%  "")) +
  theme_custom() +
  # make font small enough
  theme(
    axis.title = element_text(size = 12), axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

key21 <- ggplot(keydata21)+
  geom_tile(aes(x=raintert, y=suntert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More rain" %->%  ""),
       y = expression("More sun" %->%  "")) +
  theme_custom() +
  # make font small enough
  theme(
    axis.title = element_text(size = 12), axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

key22 <- ggplot(keydata22)+
  geom_tile(aes(x=raintert, y=suntert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More rain" %->%  ""),
       y = expression("More sun" %->%  "")) +
  theme_custom() +
  # make font small enough
  theme(
    axis.title = element_text(size = 12), axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

key23 <- ggplot(keydata23)+
  geom_tile(aes(x=raintert, y=suntert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More rain" %->%  ""),
       y = expression("More sun" %->%  "")) +
  theme_custom() +
  # make font small enough
  theme(
    axis.title = element_text(size = 12), axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

key24 <- ggplot(keydata24)+
  geom_tile(aes(x=raintert, y=suntert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More rain" %->%  ""),
       y = expression("More sun" %->%  "")) +
  theme_custom() +
  # make font small enough
  theme(
    axis.title = element_text(size = 12), axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

key25 <- ggplot(keydata25)+
  geom_tile(aes(x=raintert, y=suntert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More rain" %->%  ""),
       y = expression("More sun" %->%  "")) +
  theme_custom() +
  # make font small enough
  theme(
    axis.title = element_text(size = 12), axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

bivar20 <- ggplot(rainvssun20.df, aes(x=x, y=y, fill=colour, colour=colour))+
  geom_tile()+
  scale_fill_identity()+
  scale_colour_identity()+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),plot.title=element_text(size=rel(3)))+
  labs(title="The 🌞 and the 🌧️",
       subtitle="\nAnnual hours of sunshine vs. total precipitation in 2020",
       caption="Data from Met Office/Hollis et al./CEDA\nPlot by @VictimOfMaths")

bivar21 <- ggplot(rainvssun21.df, aes(x=x, y=y, fill=colour, colour=colour))+
  geom_tile()+
  scale_fill_identity()+
  scale_colour_identity()+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),plot.title=element_text(size=rel(3)))+
  labs(title="The 🌞 and the 🌧️",
       subtitle="\nAnnual hours of sunshine vs. total precipitation in 2021",
       caption="Data from Met Office/Hollis et al./CEDA\nPlot by @VictimOfMaths")

bivar22 <- ggplot(rainvssun22.df, aes(x=x, y=y, fill=colour, colour=colour))+
  geom_tile()+
  scale_fill_identity()+
  scale_colour_identity()+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),plot.title=element_text(size=rel(3)))+
  labs(title="The 🌞 and the 🌧️",
       subtitle="\nAnnual hours of sunshine vs. total precipitation in 2022",
       caption="Data from Met Office/Hollis et al./CEDA\nPlot by @VictimOfMaths")

bivar23 <- ggplot(rainvssun23.df, aes(x=x, y=y, fill=colour, colour=colour))+
  geom_tile()+
  scale_fill_identity()+
  scale_colour_identity()+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),plot.title=element_text(size=rel(3)))+
  labs(title="The 🌞 and the 🌧️",
       subtitle="\nAnnual hours of sunshine vs. total precipitation in 2023",
       caption="Data from Met Office/Hollis et al./CEDA\nPlot by @VictimOfMaths")

bivar24 <- ggplot(rainvssun24.df, aes(x=x, y=y, fill=colour, colour=colour))+
  geom_tile()+
  scale_fill_identity()+
  scale_colour_identity()+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),plot.title=element_text(size=rel(3)))+
  labs(title="The 🌞 and the 🌧️",
       subtitle="\nAnnual hours of sunshine vs. total precipitation in 2024",
       caption="Data from Met Office/Hollis et al./CEDA\nPlot by @VictimOfMaths")

bivar25 <- ggplot(rainvssun25.df, aes(x=x, y=y, fill=colour, colour=colour))+
  geom_tile()+
  scale_fill_identity()+
  scale_colour_identity()+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),plot.title=element_text(size=rel(3)))+
  labs(title="The 🌞 and the 🌧️",
       subtitle="\nAnnual hours of sunshine vs. total precipitation in 2025",
       caption="Data from Met Office/Hollis et al./CEDA\nPlot by @VictimOfMaths")

agg_png("Outputs/SunvsRain20.png", units="in", width=6, height=10, res=800)
ggdraw()+
  draw_plot(bivar20, 0, 0, 1, 1)+
  draw_plot(key20, 0.65, 0.5, 0.3, 0.3)

dev.off()

agg_png("Outputs/SunvsRain21.png", units="in", width=6, height=10, res=800)
ggdraw()+
  draw_plot(bivar21, 0, 0, 1, 1)+
  draw_plot(key21, 0.65, 0.5, 0.3, 0.3)

dev.off()

agg_png("Outputs/SunvsRain22.png", units="in", width=6, height=10, res=800)
ggdraw()+
  draw_plot(bivar22, 0, 0, 1, 1)+
  draw_plot(key22, 0.65, 0.5, 0.3, 0.3)

dev.off()

agg_png("Outputs/SunvsRain23.png", units="in", width=6, height=10, res=800)
ggdraw()+
  draw_plot(bivar23, 0, 0, 1, 1)+
  draw_plot(key23, 0.65, 0.5, 0.3, 0.3)

dev.off()

agg_png("Outputs/SunvsRain24.png", units="in", width=6, height=10, res=800)
ggdraw()+
  draw_plot(bivar24, 0, 0, 1, 1)+
  draw_plot(key24, 0.65, 0.5, 0.3, 0.3)

dev.off()

agg_png("Outputs/SunvsRain25.png", units="in", width=6, height=10, res=800)
ggdraw()+
  draw_plot(bivar25, 0, 0, 1, 1)+
  draw_plot(key25, 0.65, 0.5, 0.3, 0.3)

dev.off()