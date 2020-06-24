rm(list=ls())

library(tidyverse)
library(stringr)
library(sf)
library(maptools)
library(curl)
library(readxl)
library(spatstat)
library(paletteer)
library(forcats)

#Read in pubs data (commercially sensitive and not open data, alas)
raw.on <- read.csv("")

#Read in off-trade data (commercially sensitive and not open data, alas)
raw.off <- read.csv("")

#Read in postcode data (commercially sensitive and not open data, alas)
PClookup <- read.csv("")

#Remove whitespace from postcodes for matching
PClookup$pc <- str_replace_all(PClookup$postcode, " ", "")
raw.on$pc <- str_replace_all(raw.on$OT_Postcode, " ", "")
raw.off$pc <- str_replace_all(raw.off$OT_Postcode, " ", "")

raw.on <- merge(raw.on, PClookup, by="pc", all.x=TRUE)
raw.off <- merge(raw.off, PClookup, by="pc", all.x=TRUE)

#Separate pubs data for name analysis
pubs <- subset(raw.on, OT_SubLicenceDescription=="Pubs")[,c(1,6,20,27,32,33,42,43)]

#Download shapefile of country boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "http://geoportal1-ons.opendata.arcgis.com/datasets/f2c2211ff185418484566b2b7a5e1300_0.zip?outSR={%22latestWkid%22:27700,%22wkid%22:27700}"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

tiff("Outputs/CGAPubs2016.tiff", units="in", width=7, height=9, res=300)
ggplot(data=shapefile)+
  geom_sf(aes(geometry=geometry), fill=NA, colour=NA)+
  geom_point(data=subset(raw.on, OT_SubLicenceDescription=="Pubs"), 
             aes(x=oseast1m, y=osnrth1m), shape=".", colour="seagreen")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(), plot.title=element_text(margin=margin(t=0, b=-70)),
        plot.title.position="plot", plot.subtitle=element_text(vjust=-32))+
  labs(title="Great Britain in pubs",
       subtitle="Location of every pub in Great Britain in 2016",
       caption="Data from CGA Strategy | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/CGAOffLicense2016.tiff", units="in", width=7, height=9, res=300)
ggplot(data=shapefile)+
  geom_sf(aes(geometry=geometry), fill=NA, colour=NA)+
  geom_point(data=raw.off, 
             aes(x=oseast1m, y=osnrth1m), shape=".", colour="tomato4")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(), plot.title=element_text(margin=margin(t=0, b=-70)),
        plot.title.position="plot", plot.subtitle=element_text(vjust=-32))+
  labs(title="Great Britain in off-licenses",
       subtitle="Location of every off-license in Great Britain in 2016",
       caption="Data from CGA Strategy | Plot by @VictimOfMaths")
dev.off()

#Collapse to one row per postcode
raw.on <- raw.on[,c(1,20,27,32,33)]
colnames(raw.on) <- c("pc", "cat", "pop", "oseast1m", "osnrth1m")
raw.on$channel <- "On"

raw.off <- raw.off[,c(1,26,31, 36, 37)]
colnames(raw.off) <- c("pc", "cat", "pop", "oseast1m", "osnrth1m")
raw.off$channel <- "Off"

fulldata <- bind_rows(raw.on, raw.off)

temp1 <- fulldata %>% 
  group_by(pc, cat) %>% 
  summarise(count=n(), pop=unique(pop))

temp2 <- fulldata %>% 
  group_by(pc, channel) %>% 
  summarise(count=n(), pop=unique(pop))

tiff("Outputs/CGAPubHeight.tiff", units="in", width=7, height=9, res=300)
ggplot()+
  stat_density_2d(data=subset(raw.on, cat=="Pubs"), geom="tile", aes(x=oseast1m, y=osnrth1m, fill=..density..), 
                  contour=FALSE, n=800, show.legend=FALSE)+
  scale_fill_paletteer_c("oompaBase::greyscale")+
  geom_sf(data=shapefile, aes(geometry=geometry, x=long, y=lat), fill=NA, colour=NA)+
  theme_classic()+
  theme(axis.line=element_blank(), axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank())
dev.off()

#######################
#Analysis of pub names#
#######################

#Capture top 10 names
top10 <- data.frame(sort(table(pubs$OT_Name), decreasing=TRUE)[1:10])
top10$Var1 <- factor(top10$Var1)
                    
#Filter out non top-10 names
pubs10 <- pubs %>%
  filter(OT_Name %in% top10$Var1)

pubs10$OT_Name <- factor(pubs10$OT_Name, levels=levels(top10$Var1))

tiff("Outputs/CGAPubNames.tiff", units="in", width=10, height=8, res=500)
ggplot()+
  geom_sf(data=shapefile, aes(geometry=geometry), fill=NA, colour="Grey50")+
  geom_point(data=pubs10, aes(x=oseast1m, y=osnrth1m, colour=OT_Name), size=0.5, show.legend=FALSE)+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  facet_wrap(~OT_Name, nrow=2)+
  labs(title="The most popular pub names in the UK",
       caption="Data from CGA Strategy | Plot by @VictimOfMaths")
dev.off()

#####################################################
#Analysis of per capita pubs
#Collapse to LSOA-level data
pubspc <- pubs %>% 
  group_by(lsoa11) %>% 
  summarise(count=n())

#Drop duplicate rows which appear here for mysterious reasons
pubspc <- pubspc %>% distinct(.keep_all=TRUE)

#Read in population by LSOA
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
LSOApop <- read_excel(file.path(temp2,"SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 
                      sheet="Mid-2018 Persons", range=c("A6:D35097"), col_names=FALSE)[,c(1,4)]
colnames(LSOApop) <- c("LSOAcode", "pop")

pubspc <- merge(pubspc, LSOApop, by.x="lsoa11", by.y="LSOAcode")

#LSOA to town/city
temp <- tempfile()
source <- ("https://opendata.arcgis.com/datasets/dc0b24da0880417abc979c705bce3fde_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LSOAtoTown <- read.csv(temp)
colnames(LSOAtoTown) <- c("lsoa11", "LSOAname", "TCcode", "TownCity", "FID")

pubspc <- merge(pubspc, LSOAtoTown, by="lsoa11")

#Merge in LAD
temp <- tempfile()
source <- ("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LSOAtoLAD <- read.csv(temp)
names(LSOAtoLAD)[names(LSOAtoLAD)=="LSOA11CD"] <- "lsoa11"

pubspc <- merge(pubspc, LSOAtoLAD, by="lsoa11")[,c(1:6,11,14,15,16,17,21,23)]
pubspc <- pubspc %>% distinct(.keep_all=TRUE)

#Return top 10 LSOAs
pubspc$percap <- pubspc$count*1000/pubspc$pop

top10LSOA <- pubspc %>% 
  select(lsoa11, count, pop, LSOAname, percap, LAD17NM, RGN11NM) %>% 
  mutate(meanpercap=mean(percap)) %>% 
  arrange(-percap) %>% 
  top_n(n=10, wt=percap)

top10LSOA$LSOAname <- fct_reorder(top10LSOA$LSOAname, top10LSOA$percap)

#Return top 10 towns
top10town <- pubspc %>% 
  group_by(TownCity) %>%
  select(TownCity, count, pop, percap, RGN11NM) %>% 
  summarise(count=sum(count), pop=sum(pop), percap=count*1000/pop) %>% 
  mutate(meanpercap=mean(percap)) %>% 
  arrange(-percap) %>% 
  top_n(n=10, wt=percap)

top10town$TownCity <- fct_reorder(top10town$TownCity, top10town$percap)

#Return top 10 MSOAs
top10MSOA <- pubspc %>% 
  group_by(MSOA11NM) %>%
  select(MSOA11NM, count, pop, percap, RGN11NM) %>% 
  summarise(count=sum(count), pop=sum(pop), percap=count*1000/pop) %>% 
  mutate(meanpercap=mean(percap)) %>% 
  arrange(-percap) %>% 
  top_n(n=10, wt=percap)

top10MSOA$MSOA11NM <- fct_reorder(top10MSOA$MSOA11NM, top10MSOA$percap)


#Return top 10 LAs
top10LAD <- pubspc %>% 
  group_by(LAD17NM) %>%
  select(LAD17NM, count, pop, percap, RGN11NM) %>% 
  summarise(count=sum(count), pop=sum(pop), percap=count*1000/pop) %>%
  mutate(meanpercap=mean(percap)) %>% 
  arrange(-percap) %>% 
  top_n(n=10, wt=percap)

top10LAD$LAD17NM <- fct_reorder(top10LAD$LAD17NM, top10LAD$percap)

#Tidy up a little
#Bring in new MSOA names
temp <- tempfile()
source <- ("https://visual.parliament.uk/msoanames/static/MSOA-Names-v1.1.0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
MSOAnames <- read.csv(temp)

top10MSOA <- merge(top10MSOA, MSOAnames, by.x="MSOA11NM", by.y="msoa11nm")
top10MSOA$msoa11hclnm <- fct_reorder(top10MSOA$msoa11hclnm, top10MSOA$percap)

tiff("Outputs/CGALSOATop10.tiff", units="in", width=8, height=6, res=500)
ggplot(top10LSOA[c(1:10),], aes(x=percap, y=LSOAname, fill=percap))+
  geom_col(show.legend=FALSE)+
  geom_segment(aes(x=meanpercap[1], xend=meanpercap[1], y=0, yend=10.5), colour="grey20")+
  scale_x_continuous(name="Pubs per 1,000 inhabitants")+
  scale_y_discrete(name="LSOA name")+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  theme_classic()+
  theme(plot.title.position="plot")+
  coord_cartesian(clip="off")+
  annotate("text", x=top10LSOA$meanpercap[1]+0.3, y=0.3, 
           label=paste0("National average = ",round(top10LSOA$meanpercap[1],1), " pubs per 1,000"), 
           hjust=0, colour="grey30", size=rep(3.5))+
  labs(title="Central London has a lot more pubs per capita than anywhere else",
       subtitle="The 10 Lower Super Output Areas with the highest number of pubs per capita in England & Wales",
       caption="Data from CGA Strategy & ONS | Plot by @VictimOfMaths")
dev.off()

#Read in LSOA shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/e886f1cd40654e6b94d970ecf437b7b5_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))
names(shapefile)[names(shapefile) == "LSOA11CD"] <- "lsoa11"

shapefile <- st_transform(map.data, crs=4326)

map.data <- full_join(shapefile, top10LSOA, by="lsoa11")

library(ggmap)
library(osmdata)
library(ggExtra)

capsize <- 1

London.map <- get_stamenmap(getbb("Greater London"), maptype="watercolor", zoom=12)

LondonLSOA <- London.map %>%
  ggmap()+
  geom_sf(data=subset(map.data, !is.na(percap)), aes(fill=percap, geometry=geometry), 
          inherit.aes=FALSE, alpha=0.8, show.legend=FALSE)+
  xlim(-0.15,-0.05)+
  ylim(51.48, 51.54)+
  theme_classic()+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  labs(title="Central London (1st, 2nd & 7th)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.margin = unit(c(0,0,0,0), "cm"), plot.title=element_text(size=rel(capsize)))

Hull.map <- get_stamenmap(getbb("Hull"), maptype="watercolor", zoom=12)

HullLSOA <- Hull.map %>%
  ggmap()+
  geom_sf(data=subset(map.data, !is.na(percap)), aes(fill=percap, geometry=geometry), 
          inherit.aes=FALSE, alpha=0.8, show.legend=FALSE)+
  xlim(-0.4,-0.25)+
  ylim(53.72, 53.78)+
  theme_classic()+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  labs(title="Hull (3rd)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.margin = unit(c(0,0,0,0), "cm"), plot.title=element_text(size=rel(capsize)))

Brighton.map <- get_stamenmap(getbb("Brighton"), maptype="watercolor", zoom=12)

BrightonLSOA <- Brighton.map %>%
  ggmap()+
  geom_sf(data=subset(map.data, !is.na(percap)), aes(fill=percap, geometry=geometry), 
          inherit.aes=FALSE, alpha=0.8, show.legend=FALSE)+
  xlim(0.17, 0.12)+
  ylim(50.81, 50.85)+
  theme_classic()+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  labs(title="Brighton (4th)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.margin = unit(c(0,0,0,0), "cm"), plot.title=element_text(size=rel(capsize)))

Weymouth.map <- get_stamenmap(getbb("Weymouth"), maptype="watercolor", zoom=12)

WeymouthLSOA <- Weymouth.map %>%
  ggmap()+
  geom_sf(data=subset(map.data, !is.na(percap)), aes(fill=percap, geometry=geometry), 
          inherit.aes=FALSE, alpha=0.8, show.legend=FALSE)+
  xlim(-2.49, -2.42)+
  ylim(50.57, 50.64)+
  theme_classic()+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  labs(title="Weymouth (5th)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.margin = unit(c(0,0,0,0), "cm"), plot.title=element_text(size=rel(capsize)))

Darlington.map <- get_stamenmap(getbb("Darlington"), maptype="watercolor", zoom=12)

DarlingtonLSOA <- Darlington.map %>%
  ggmap()+
  geom_sf(data=subset(map.data, !is.na(percap)), aes(fill=percap, geometry=geometry), 
          inherit.aes=FALSE, alpha=0.8, show.legend=FALSE)+
  theme_classic()+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  labs(title="Darlington (6th)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.margin = unit(c(0,0,0,0), "cm"), plot.title=element_text(size=rel(capsize)))

Chester.map <- get_stamenmap(getbb("Chester"), maptype="watercolor", zoom=12)

ChesterLSOA <- Chester.map %>%
  ggmap()+
  geom_sf(data=subset(map.data, !is.na(percap)), aes(fill=percap, geometry=geometry), 
          inherit.aes=FALSE, alpha=0.8, show.legend=FALSE)+
  xlim(-2.97, -2.81)+
  ylim(53.15, 53.25)+  
  theme_classic()+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  labs(title="Chester (8th)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.margin = unit(c(0,0,0,0), "cm"), plot.title=element_text(size=rel(capsize)))

York.map <- get_stamenmap(getbb("York"), maptype="watercolor", zoom=12)

YorkLSOA <- York.map %>%
  ggmap()+
  geom_sf(data=subset(map.data, !is.na(percap)), aes(fill=percap, geometry=geometry), 
          inherit.aes=FALSE, alpha=0.8, show.legend=FALSE)+
  xlim(-1.15, -1)+
  ylim(53.91, 54)+  
  theme_classic()+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  labs(title="York (9th)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.margin = unit(c(0,0,0,0), "cm"), plot.title=element_text(size=rel(capsize)))

Manchester.map <- get_stamenmap(getbb("Manchester"), maptype="watercolor", zoom=12)

ManchesterLSOA <- Manchester.map %>%
  ggmap()+
  geom_sf(data=subset(map.data, !is.na(percap)), aes(fill=percap, geometry=geometry), 
          inherit.aes=FALSE, alpha=0.8, show.legend=FALSE)+
  xlim(-2.28, -2.2)+
  ylim(53.46, 53.51)+  
  theme_classic()+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  labs(title="Manchester (10th)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.margin = unit(c(0,0,0,0), "cm"), plot.title=element_text(size=rel(capsize)))

grid <- plot_grid(LondonLSOA, HullLSOA, BrightonLSOA, WeymouthLSOA, DarlingtonLSOA, ChesterLSOA,
          YorkLSOA, ManchesterLSOA)

title <-  ggdraw() + 
  draw_label("The 10 Lower Super Output Areas (LSOAs) with the most pubs per capita",
    fontface = 'bold', x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

caption <- ggdraw() + 
  draw_label("Data from CGA Strategy & ONS | Plot by @VictimOfMaths",
             x = 0, hjust = -1.6, size=rel(8)) +
  theme(plot.margin = margin(0, 0, 0, 7))

tiff("Outputs/CGALSOAmap.tiff", units="in", width=8, height=8, res=500)
plot_grid(title, grid, caption, ncol=1, rel_heights=c(0.1,1,0.05))
dev.off()

tiff("Outputs/CGAMSOATop10.tiff", units="in", width=8, height=6, res=500)
ggplot(top10MSOA[c(1:10),], aes(x=percap, y=msoa11hclnm, fill=percap))+
  geom_col(show.legend=FALSE)+
  geom_segment(aes(x=meanpercap[1], xend=meanpercap[1], y=0, yend=10.5), colour="grey20")+
  scale_x_continuous(name="Pubs per 1,000 inhabitants")+
  scale_y_discrete(name="MSOA name")+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  theme_classic()+
  theme(plot.title.position="plot")+
  coord_cartesian(clip="off")+
  annotate("text", x=top10MSOA$meanpercap[1]+0.2, y=0.3, 
           label=paste0("National average = ",round(top10MSOA$meanpercap[1],1), " pubs per 1,000"), 
           hjust=0, colour="grey30", size=rep(3.5))+
  labs(title="Central London has a lot more pubs per capita than anywhere else",
       subtitle="The 10 Middle Super Output Areas with the highest number of pubs per capita in England & Wales",
       caption="Data from CGA Strategy & ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/CGALADTop10.tiff", units="in", width=8, height=6, res=500)
ggplot(top10LAD[c(1:10),], aes(x=percap, y=LAD17NM, fill=percap))+
  geom_col(show.legend=FALSE)+
  geom_segment(aes(x=meanpercap[1], xend=meanpercap[1], y=0, yend=10.5), colour="grey20")+
  scale_x_continuous(name="Pubs per 1,000 inhabitants")+
  scale_y_discrete(name="Local Authority name")+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  theme_classic()+
  theme(plot.title.position="plot")+
  coord_cartesian(clip="off")+
  annotate("text", x=top10LAD$meanpercap[1]+0.3, y=0.3, 
           label=paste0("National average = ",round(top10LAD$meanpercap[1],1), " pubs per 1,000"), 
           hjust=0, colour="grey30", size=rep(3.5))+
  labs(title="Central London has a lot more pubs per capita than anywhere else",
       subtitle="The 10 Local Authority Districts with the highest number of pubs per capita in England & Wales",
       caption="Data from CGA Strategy & ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/CGATownTop10.tiff", units="in", width=8, height=6, res=500)
ggplot(top10town[c(1:10),], aes(x=percap, y=TownCity, fill=percap))+
  geom_col(show.legend=FALSE)+
  geom_segment(aes(x=meanpercap[1], xend=meanpercap[1], y=0, yend=10.5), colour="grey20")+
  scale_x_continuous(name="Pubs per 1,000 inhabitants")+
  scale_y_discrete(name="Town name")+
  scale_fill_paletteer_c("scico::hawaii", direction=-1)+
  theme_classic()+
  theme(plot.title.position="plot")+
  coord_cartesian(clip="off")+
  annotate("text", x=top10town$meanpercap[1]+0.05, y=0.3, 
           label=paste0("National average = ",round(top10town$meanpercap[1],1), " pubs per 1,000"), 
           hjust=0, colour="grey30", size=rep(3.5))+
  labs(title="Chester has more pubs per capita than anywhere else",
       subtitle="The 10 large Towns/Cities with the highest number of pubs per capita in England & Wales",
       caption="Data from CGA Strategy & ONS | Plot by @VictimOfMaths")
dev.off()
