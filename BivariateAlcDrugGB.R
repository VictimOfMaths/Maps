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
library(readxl)
library(curl)
library(forcats)

#England alcohol data
#read in data downloaded from LAPE 
dataalc <- fread("Data/LAPEAlcSpecMort.csv")

#pull out 2015-17 data for Poole, Dorset & Bournemouth as they are missing from 2016-18 data
#due to new LA boundaries
temp <- subset(dataalc, `Area Code` %in% c("E06000028", "E06000029", "E10000009") & 
                 `Time period`=="2015 - 17" & Sex=="Persons")

dataalc <- subset(dataalc, `Area Type`=="County & UA (pre 4/19)" & `Time period`=="2016 - 18" & Sex=="Persons")

dataalc <- rbind(dataalc, temp)

dataalc <- dataalc[,c(5,6,13,14,15)]

colnames(dataalc) <- c("id", "LA", "alcrate", "alcrate_lower_ci", "alcrate_upper_ci" )

#Wales alcohol data
temp <- tempfile()
source <- "https://www.healthmapswales.wales.nhs.uk/IAS/data/csv?viewId=155&geoId=108&subsetId=&viewer=CSV"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
walesalc <- fread(temp)[,c(2,158:160)]
colnames(walesalc) <- c("LA", "alcrate", "alcrate_lower_ci", "alcrate_upper_ci" )

#Read in LA codes to match into Welsh data
temp <- tempfile()
source <- "http://geoportal1-ons.opendata.arcgis.com/datasets/a267b55f601a4319a9955b0197e3cb81_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LAcodes <- fread(temp)[,c(1,2)]

#rename Vale of Glamorgan to ensure matching
LAcodes$LAD17NM <- ifelse(LAcodes$LAD17NM=="Vale of Glamorgan", "The Vale of Glamorgan", LAcodes$LAD17NM)
walesalc <- merge(walesalc, LAcodes, by.x="LA", by.y="LAD17NM")
names(walesalc)[names(walesalc)=="LAD17CD"] <- "id"

dataalc <- rbind(dataalc, walesalc)

#read in drugs data from the ONS for England & Wales
datadrg <- fread("Data/DrugDeathsLA.csv", header=TRUE)

#recode Dorset, Poole & Bournemouth
datadrg$id <- case_when(
  datadrg$id=="E06000058" ~ "E06000029",
  datadrg$id=="E06000059" ~ "E10000009",
  TRUE ~ datadrg$id
)

#merge into alcohol
data <- merge(dataalc, datadrg, by="id", all.x=TRUE)

data <- data[,c(1:5,9,11,12)]
colnames(data) <- c("id", "LA", "alcrate", "alcrate_lower_ci", "alcrate_upper_ci", "drgrate", "drgrate_lower_ci", "drgrate_upper_ci")

data$drgrate <- as.numeric(ifelse(data$drgrate==":", "NA", data$drgrate))
data$drgrate_lower_ci <- as.numeric(ifelse(data$drgrate_lower_ci==":", "NA", data$drgrate_lower_ci))
data$drgrate_upper_ci <- as.numeric(ifelse(data$drgrate_upper_ci==":", "NA", data$drgrate_upper_ci))

#Allocate Bournemouth drug deaths from new combined authority
data$drgrate <- ifelse(data$LA=="Bournemouth", data[data[,LA]=="Poole",]$drgrate, data$drgrate)
data$drgrate_lower_ci <- ifelse(data$LA=="Bournemouth", data[data[,LA]=="Poole",]$drgrate_lower_ci, 
                                data$drgrate_lower_ci)
data$drgrate_upper_ci <- ifelse(data$LA=="Bournemouth", data[data[,LA]=="Poole",]$drgrate_upper_ci, 
                                data$drgrate_upper_ci)

#Add in country indicator
data$Country <- case_when(
  substr(data$id, 1, 1)=="E" ~ "England",
  substr(data$id, 1, 1)=="W" ~ "Wales",
  TRUE ~ "Error")

#Read in Scottish data
#Replace ... with local file path of Scottish data
dataalc <- read_excel("...Scotland LA alc drug deaths.xlsx", sheet="Alcohol-specific deaths",
                      range="A9:D40", col_names=FALSE)

colnames(dataalc) <- c("LA", "alcrate", "alcrate_lower_ci", "alcrate_upper_ci")

#Replace ... with local file path of Scottish data
datadrg <- read_excel("...Scotland LA alc drug deaths.xlsx", sheet="Drug-related deaths",
                      range="A9:D40", col_names=FALSE)

colnames(datadrg) <- c("LA", "drgrate", "drgrate_lower_ci", "drgrate_upper_ci")

#merge
scotdata <- merge(dataalc, datadrg, by="LA")
scotdata$Country <- "Scotland"

#bring in LA codes for Scotland
temp <- tempfile()
source <- "http://geoportal1-ons.opendata.arcgis.com/datasets/a267b55f601a4319a9955b0197e3cb81_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
scotcodes <- fread(temp)[,c(1,2)]
colnames(scotcodes) <- c("id", "LA")
scotdata <- merge(scotdata, scotcodes, by="LA", fill=TRUE)

data <- rbind(data, scotdata, fill=TRUE)

#generate tertiles
data$alctert <- quantcut(data$alcrate, q=3, labels=FALSE)
data$drgtert <- quantcut(data$drgrate, q=3, labels=FALSE)

#save cutoffs
alccut1 <- quantile(data$alcrate, probs=1/3, na.rm=TRUE)
alccut2 <- quantile(data$alcrate, probs=2/3, na.rm=TRUE)
drgcut1 <- quantile(data$drgrate, probs=1/3, na.rm=TRUE)
drgcut2 <- quantile(data$drgrate, probs=2/3, na.rm=TRUE)

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

#Read in shapefile of LA boundaries
#from http://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-december-2017-full-clipped-boundaries-in-uk/data
polygons <- readOGR("Shapefiles/Counties_and_Unitary_Authorities_December_2017_Full_Clipped_Boundaries_in_UK.shp")
polygons$id <- as.character(polygons$ctyua17cd)
polygons<-fortify(polygons, region="id")

map <- ggplot(data)+
  geom_map(aes(map_id=id, fill=colour), map=polygons, colour="White")+
  xlim(-116,655644)+
  ylim(5337,1220302)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="Regional patterns in deaths from alcohol and drugs in Great Britain",
       subtitle="Comparative rates of alcohol-specific deaths and deaths from drug misuse by Local Authority",
       caption="Data from Public Health England, NHS Wales, Office for National Statistics & National Records of Scotland\nPlot by @VictimOfMaths")+
  #Highland S12000017
  annotate("text", x=500000, y=970000, label="Purple areas mean\nhigh rates of alcohol and \nhigh rates of drug deaths", size=3)+
  #York E06000014
  annotate("text", x=550000, y=582000, label="Red areas mean\nlow rates of alcohol and \nhigh rates of drug deaths", size=3)+
  #Walsall E08000030
  annotate("text", x=230000, y=455000, label="Blue areas mean\nhigh rates of alcohol and \nlow rates of drug deaths", size=3)+
  #Hampshire E10000014
  annotate("text", x=480000, y=27000, label="Grey areas mean\nlow rates of alcohol and \nlow rates of drug deaths", size=3)+
  geom_curve(aes(x=434000, y=955000, xend=220000, yend=850000), curvature=0.15)+
  geom_curve(aes(x=550000, y=540000, xend=463000, yend=452000), curvature=-0.25)+
  geom_curve(aes(x=297000, y=450000, xend=405000, yend=300000), curvature=-0.4)+
  geom_curve(aes(x=470000, y=57000, xend=455000, yend=130000), curvature=0.1)+
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

tiff("Outputs/BivariateAlcDrugsEngScot.tiff", units="in", width=8, height=14, res=300)
ggdraw()+
  draw_plot(map, 0,0,1,1)+
  draw_plot(key, 0.03,0.47,0.29,0.73)
dev.off()

#Explore data a bit more

#scatter coloured by country
tiff("Outputs/LAALcDrgESW.tiff", units="in", width=7, height=6, res=500)
ggplot(data, aes(x=alcrate, y=drgrate, colour=Country))+
  geom_point()+
  geom_segment(x=-10, xend=45, y=-10, yend=45, colour="Black")+
  theme_classic()+
  scale_x_continuous(name="Alcohol-specific deaths per 100,000 population", limits=c(0,42))+
  scale_y_continuous(name="Drug misuse deaths per 100,000 population", limits=c(0,42))+
  scale_colour_manual(values=c("#cc5522", "#00ccff", "#443333"))+
  annotate("text", x=30, y=6, label="More deaths from alcohol", colour="DarkGrey")+
  annotate("text", x=10, y=32, label="More deaths from drugs", colour="DarkGrey")+
  labs(title="Deaths from alcohol and drugs by Local Authority", 
       subtitle="Alcohol-specific and drug misuse deaths",
       caption="Data from PHE, ONS, NHS Wales & NRS | Plot by @VictimOfMaths")
dev.off()

#repeat with bivariate key overlaid
tiff("Outputs/LAALcDrgTertESW.tiff", units="in", width=7, height=6, res=500)
ggplot(data, aes(x=alcrate, y=drgrate, colour=Country))+
  geom_rect(aes(xmin=0,xmax=alccut1, ymin=0, ymax=drgcut1), fill="#CABED0", colour=NA)+
  geom_rect(aes(xmin=0,xmax=alccut1, ymin=drgcut1, ymax=drgcut2), fill="#BC7C5F", colour=NA)+
  geom_rect(aes(xmin=0,xmax=alccut1, ymin=drgcut2, ymax=42), fill="#AE3A4E", colour=NA)+
  geom_rect(aes(xmin=alccut1,xmax=alccut2, ymin=0, ymax=drgcut1), fill="#89A1C8", colour=NA)+
  geom_rect(aes(xmin=alccut1,xmax=alccut2, ymin=drgcut1, ymax=drgcut2), fill="#806A8A", colour=NA)+
  geom_rect(aes(xmin=alccut1,xmax=alccut2, ymin=drgcut2, ymax=42), fill="#77324C", colour=NA)+
  geom_rect(aes(xmin=alccut2,xmax=42, ymin=0, ymax=drgcut1), fill="#4885C1", colour=NA)+
  geom_rect(aes(xmin=alccut2,xmax=42, ymin=drgcut1, ymax=drgcut2), fill="#435786", colour=NA)+
  geom_rect(aes(xmin=alccut2,xmax=42, ymin=drgcut2, ymax=42), fill="#3f2949", colour=NA)+
  geom_point(size=1.5)+
  geom_point(shape=21, colour="White", size=1.5)+
  #geom_segment(x=-10, xend=45, y=-10, yend=45, colour="Black")+
  theme_classic()+
  scale_x_continuous(name="Alcohol-specific deaths per 100,000 population", limits=c(0,42))+
  scale_y_continuous(name="Drug misuse deaths per 100,000 population", limits=c(0,42))+
  scale_colour_manual(values=c("#cc5522", "#00ccff", "#443333"))+
  labs(title="Deaths from alcohol and drugs by Local Authority", 
       subtitle="Alcohol-specific and drug misuse deaths coloured by tertile",
       caption="Data from PHE, ONS, NHS Wales & NRS | Plot by @VictimOfMaths")
dev.off()

#Bar charts with CIs
tiff("Outputs/LAALcCIsESW.tiff", units="in", width=8, height=6, res=500)
ggplot(data, aes(x=fct_reorder(as.factor(LA), alcrate), y=alcrate, ymin=alcrate_lower_ci, 
                 ymax=alcrate_upper_ci, fill=Country))+
  geom_crossbar()+
  theme_classic()+
  scale_fill_manual(values=c("#cc5522", "#00ccff", "#443333"))+
  scale_x_discrete(name="Local Authority")+
  scale_y_continuous(name="Alcohol-specific deaths per 100,000")+
  labs(title="Local Authority variation in alcohol-specific deaths", 
       subtitle="Mean annual death rates with 95% Confidence Intervals",
       caption="Data from PHE, ONS, NHS Wales & NRS | Plot by @VictimOfMaths")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off()

tiff("Outputs/LADrgCIsESW.tiff", units="in", width=8, height=6, res=500)
ggplot(data, aes(x=fct_reorder(as.factor(LA), drgrate), y=drgrate, ymin=drgrate_lower_ci, 
                 ymax=drgrate_upper_ci, fill=Country))+
  geom_crossbar()+
  theme_classic()+
  scale_fill_manual(values=c("#cc5522", "#00ccff", "#443333"))+
  scale_x_discrete(name="Local Authority")+
  scale_y_continuous(name="Drug misuse deaths per 100,000")+
  labs(title="Local Authority variation in drug misuse deaths", 
       subtitle="Mean annual death rates with 95% Confidence Intervals",
       caption="Data from PHE, ONS, NHS Wales & NRS | Plot by @VictimOfMaths")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off()

#Plot choropleths for each outcome separately
AlcLAGB <- ggplot(data)+
  geom_map(aes(map_id=id, fill=alcrate), map=polygons, colour="White")+
  xlim(-116,655644)+
  ylim(5337,1220302)+
  theme_classic()+
  scale_fill_distiller(palette="YlGnBu", direction=1, name="Rate per\n100,000", na.value="White")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())

tiff("Outputs/AlcLAGB.tiff", units="in", width=8, height=14, res=300)
AlcLAGB+
labs(title="Regional patterns in deaths from alcohol in Great Britain",
     subtitle="Comparative rates of alcohol-specific deaths by Local Authority",
     caption="Data from Public Health England, NHS Wales & National Records of Scotland\nPlot by @VictimOfMaths")
dev.off()

DrgLAGB <- ggplot(data)+
  geom_map(aes(map_id=id, fill=drgrate), map=polygons, colour="White")+
  xlim(-116,655644)+
  ylim(5337,1220302)+
  theme_classic()+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="Rate per\n100,000", na.value="White")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())

tiff("Outputs/DrgLAGB.tiff", units="in", width=8, height=14, res=300)
DrgLAGB+
  labs(title="Regional patterns in deaths from drugs in Great Britain",
       subtitle="Comparative rates of drug misuse deaths by Local Authority",
       caption="Data from Office for National Statistics & National Records of Scotland\nPlot by @VictimOfMaths")
dev.off()

#Stick them together
title <- ggdraw()+
  draw_label("Deaths from alcohol-specific causes and drug misuse in Great Britain",
             x=0.02, hjust=0, size=24)

caption <- ggdraw()+
  draw_label("Data from PHE, ONS, NHS Wales & NRS | Plot by @VictimOfMaths",
             x=0.5, hjust=0)

maps <- plot_grid(AlcLAGB, DrgLAGB, align="hv", labels=c("Alcohol", "Drugs"))

tiff("Outputs/AlcDrgLAGB.tiff", units="in", width=14, height=14, res=300)
plot_grid(title, maps, caption, ncol=1, rel_heights=c(0.08, 1, 0.05))
dev.off()
