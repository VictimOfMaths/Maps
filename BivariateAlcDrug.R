rm(list=ls())

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(paletteer)
library(gtools)
library(rgdal)
library(cowplot)
library(purrr)
library(magrittr)
#library(fingertipsR)

#read in alcohol data downloaded from LAPE 
dataalc <- fread("Data/LAPEAlcSpecMort.csv")

#pull out 2015-17 data for Poole, Dorset & Bournemouth
temp <- subset(dataalc, `Area Code` %in% c("E06000028", "E06000029", "E10000009") & 
                 `Time period`=="2015 - 17" & Sex=="Persons")

dataalc <- subset(dataalc, `Area Type`=="County & UA (pre 4/19)" & `Time period`=="2016 - 18" & Sex=="Persons")

dataalc <- rbind(dataalc, temp)

dataalc <- dataalc[,c(5,6,13)]

colnames(dataalc) <- c("id", "LA", "alcrate")

#calcualte tertiles 1=lowest
dataalc$alctert <- quantcut(dataalc$alcrate, q=3, labels=FALSE)

#read in drugs data
datadrg <- fread("Data/DrugDeathsLA.csv", header=TRUE)

#recode Dorset, Poole & Bournemouth
datadrg$id <- case_when(
  datadrg$id=="E06000058" ~ "E06000029",
  datadrg$id=="E06000059" ~ "E10000009",
  TRUE ~ datadrg$id
)

#merge into alcohol
data <- merge(dataalc, datadrg)

data <- data[,c(1:4, 8)]

data$Rate <- as.numeric(ifelse(data$Rate==":", "NA", data$Rate))

#calculate drug tertiles
data$drgtert <- quantcut(data$Rate, q=3, labels=FALSE)

#generate 9-category index for map key
data$key <- case_when(
  data$alctert==1 & data$drgtert==1 ~ 1,
  data$alctert==1 & data$drgtert==2 ~ 2,
  data$alctert==1 & data$drgtert==3 ~ 3,
  data$alctert==2 & data$drgtert==1 ~ 4,
  data$alctert==2 & data$drgtert==2 ~ 5,
  data$alctert==2 & data$drgtert==3 ~ 6,
  data$alctert==3 & data$drgtert==1 ~ 7,
  data$alctert==3 & data$drgtert==2 ~ 8,
  data$alctert==3 & data$drgtert==3 ~ 9
)

#fill in corresponding colours
data$colour <- case_when(
  data$key==1 ~ "#CABED0",
  data$key==2 ~ "#BC7C5F",
  data$key==3 ~ "#AE3A4E",
  data$key==4 ~ "#89A1C8",
  data$key==5 ~ "#806A8A",
  data$key==6 ~ "#77324C",
  data$key==7 ~ "#4885C1",
  data$key==8 ~ "#435786",
  data$key==9 ~ "#3f2949"
  
)

#generate dataframe for key
keydata <- data %>%
  filter(!is.na(colour)) %>%
  group_by(alctert, drgtert) %>%
  summarise(RGB=unique(colour))

#plot it all on a map
#Shapefile from https://data.gov.uk/dataset/d6f97a1a-25dc-485c-9af3-0e5681465d77/counties-and-unitary-authorities-december-2016-full-clipped-boundaries-in-england-and-wales
polygons <- readOGR("England LAs/Counties_and_Unitary_Authorities_December_2016_Full_Clipped_Boundaries_in_England_and_Wales.shp")
polygons$id <- as.character(polygons$ctyua16cd)
polygons<-fortify(polygons, region="id")


map <- ggplot(data)+
  geom_map(aes(map_id=id, fill=colour), map=polygons, colour="White")+
  xlim(-6,2.5)+
  ylim(50,56)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="Regional patterns in deaths from alcohol and drugs in England",
       subtitle="Comparative rates of alcohol-specific deaths and deaths from drug poisoning by Local Authority",
       caption="Data from Office for National Statistics | Plot by @VictimOfMaths")+
  annotate("text", x=0, y=55, label="Purple areas mean\nhigh rates of alcohol and \nhigh rates of drug deaths", size=3)+
  annotate("text", x=1.3, y=53.3, label="Red areas mean\nlow rates of alcohol and \nhigh rates of drug deaths", size=3)+
  annotate("text", x=-4, y=51.7, label="Blue areas mean\nhigh rates of alcohol and \nlow rates of drug deaths", size=3)+
  annotate("text", x=-1, y=50.3, label="Grey areas mean\nlow rates of alcohol and \nlow rates of drug deaths", size=3)+
  geom_curve(aes(x=0, y=54.8, xend=-1, yend=54.55), curvature=-0.15)+
  geom_curve(aes(x=1, y=53.1, xend=1.1, yend=52.7), curvature=-0.2)+
  geom_curve(aes(x=-3.8, y=51.9, xend=-2.5, yend=52.75), curvature=0.1)+
  geom_curve(aes(x=-1, y=50.45, xend=-1.3, yend=51.15), curvature=0.1)+
theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
      axis.title=element_blank())

key <- ggplot(keydata)+
  geom_tile(aes(x=alctert, y=drgtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More alcohol-specific deaths" %->%  ""),
       y = expression("More drug poisoning deaths" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

tiff("Outputs/BivariateAlcDrugs.tiff", units="in", width=8, height=10, res=300)
ggdraw()+
  draw_plot(map, 0,0,1,1)+
  draw_plot(key, 0.05,0.25,0.25,0.45)
dev.off()