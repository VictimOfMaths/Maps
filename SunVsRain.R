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
#https://data.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.1.0.0/1km
#http://dx.doi.org/10.5285/bbca3267dc7d4219af484976734c9527

#2021 Rainfall
nc.rain <- raster("Data/CEDA Data/rainfall_hadukgrid_uk_1km_ann_202101-202112.nc", varname="rainfall")
proj4string(nc.rain)=CRS("+init=EPSG:27700")
plot(nc.rain)
writeRaster(nc.rain, "Data/CEDA Data/Rainfall21.tif", "GTiff", overwrite=TRUE)

#2021 Sunshine
nc.sun <- raster("Data/CEDA Data/sun_hadukgrid_uk_1km_ann_202101-202112.nc", varname="sun")
proj4string(nc.sun)=CRS("+init=EPSG:27700")
plot(nc.sun)
writeRaster(nc.sun, "Data/CEDA Data/Sunshine21.tif", "GTiff", overwrite=TRUE)

#Bivariate map of rain and sun coverage
rain.df <- as.data.frame(as(nc.rain, "SpatialPixelsDataFrame"))
sun.df <-  as.data.frame(as(nc.sun, "SpatialPixelsDataFrame"))

rainvssun.df <- merge(rain.df, sun.df) %>% 
  mutate(raintert=quantcut(Total.precipitation.amount, q=3, labels=FALSE),
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
keydata <- rainvssun.df %>%
  filter(!is.na(colour)) %>%
  group_by(raintert, suntert) %>%
  summarise(RGB=unique(colour))

key <- ggplot(keydata)+
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

bivar <- ggplot(rainvssun.df, aes(x=x, y=y, fill=colour, colour=colour))+
  geom_tile()+
  scale_fill_identity()+
  scale_colour_identity()+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),plot.title=element_text(size=rel(3)))+
  labs(title="The 🌞 and the 🌧️",
       subtitle="Annual hours of sunshine vs. total precipitation in 2021",
       caption="Data from Met Office/Hollis et al./CEDA\nPlot by @VictimOfMaths")

agg_tiff("Outputs/SunvsRain.tiff", units="in", width=6, height=10, res=500)
ggdraw()+
  draw_plot(bivar, 0, 0, 1, 1)+
  draw_plot(key, 0.65, 0.5, 0.3, 0.3)
dev.off()
