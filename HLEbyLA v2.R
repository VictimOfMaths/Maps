rm(list=ls())

library(tidyverse)
library(readxl)
library(curl)
library(sf)
library(paletteer)

#Download data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyatbirthandatage65bylocalareasuk/current/hsleatbirthandatage65byukla201618.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

maledata <- read_excel(temp, sheet="HE - Male at birth", range="A4:Q490") %>% 
  mutate(name=coalesce(`Area Names`, `...3`, `...4`),
         sex="Male") %>% 
  rename(ctyua17cd=`Area Codes`) %>% 
  select(ctyua17cd, name, sex, LE, HLE, DfLE) %>% 
  na.omit()

femaledata <- read_excel(temp, sheet="HE - Female at birth", range="A4:Q490") %>% 
  mutate(name=coalesce(`Area Names`, `...3`, `...4`),
         sex="Female") %>% 
  rename(ctyua17cd=`Area Codes`) %>% 
  select(ctyua17cd, name, sex, LE, HLE, DfLE) %>% 
  na.omit()

data <- bind_rows(maledata, femaledata) %>% 
  mutate(ctyua17cd=case_when(
    ctyua17cd=="S12000048" ~ "S12000024",
    ctyua17cd=="E06000059" ~ "E10000009",
    ctyua17cd=="S12000047" ~ "S12000015",
    ctyua17cd=="E06000058" ~ "E06000028",
    TRUE ~ ctyua17cd))

data <- data %>% 
  filter(ctyua17cd=="E06000028") %>% 
  mutate(ctyua17cd="E06000029") %>% 
  bind_rows(data)

#Read in shapefile of LA boundaries
#Download shapefile of LA boundaries
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


tiff("Outputs/HLEbyLA.tiff", units="in", width=12, height=7, res=500)
ggplot(data=subset(map.data, !is.na(HLE)), aes(fill=HLE, geometry=geometry))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("pals::ocean.haline", name="Healthy Life\nExpectancy", direction=-1)+
  facet_wrap(~sex)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), strip.background=element_blank(),
        strip.text=element_text(face="bold"))+
  labs(title="Healthy Life Expectancy varies hugely across the UK",
       subtitle="Average years of life lived in 'good' or 'very good' health in UK Local Authorities",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/LEbyLA.tiff", units="in", width=12, height=7, res=500)
ggplot(data=subset(map.data, !is.na(LE)), aes(fill=LE, geometry=geometry))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("pals::ocean.matter", name="Life Expectancy", direction=-1)+
  facet_wrap(~sex)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), strip.background=element_blank(),
        strip.text=element_text(face="bold"))+
  labs(title="Women live longer than men across the UK",
       subtitle="Life Expectancy at birth in UK Local Authorities",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/DFLEbyLA.tiff", units="in", width=12, height=7, res=500)
ggplot(data=subset(map.data, !is.na(DfLE)), aes(fill=DfLE, geometry=geometry))+
  geom_sf(colour=NA)+
  theme_classic()+
  scale_fill_paletteer_c("pals::ocean.speed", name="Life Expectancy", direction=-1)+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), strip.background=element_blank(),
        strip.text=element_text(face="bold"))+
  facet_wrap(~sex)+
  labs(title="Disease free life expectancy by Local Authority in England",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Analysis of data
analysis.data <- map.data %>% 
  st_drop_geometry() %>% 
  select(ctyua17cd, ctyua17nm, sex, LE, HLE, DfLE) %>% 
  mutate(country=case_when(
    substr(ctyua17cd,1,1)=="S" ~ "Scotland",
    substr(ctyua17cd,1,1)=="E" ~ "England",
    substr(ctyua17cd,1,1)=="N" ~ "Northern Ireland",
    substr(ctyua17cd,1,1)=="W" ~ "Wales"))

tiff("Outputs/HLEbyLAScatter.tiff", units="in", width=8, height=6, res=500)
analysis.data %>% 
  select(ctyua17cd, country, sex, LE) %>% 
  filter(!is.na(country)) %>% 
  spread(sex, LE) %>% 
  ggplot(aes(x=Male, y=Female, colour=country))+
  geom_point()+
  geom_abline()+
  scale_x_continuous(name="Male Life Expectancy at birth", limits=c(72, 87))+
  scale_y_continuous(name="Female Life Expectancy at Birth", limits=c(72, 87))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="Country")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Life Expectancy is higher for women in every UK Local Authority",
       subtitle="Life Expectancy at Birth based on 2017-19 data",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/LEbyLAScatter.tiff", units="in", width=8, height=6, res=500)
analysis.data %>% 
  select(ctyua17cd, country, sex, HLE) %>% 
  filter(!is.na(country)) %>% 
  spread(sex, HLE) %>% 
  ggplot(aes(x=Male, y=Female, colour=country))+
  geom_point()+
  geom_abline()+
  scale_x_continuous(name="Male Healthy Life Expectancy at birth", limits=c(50, 75))+
  scale_y_continuous(name="Female Healthy Life Expectancy at Birth", limits=c(50, 75))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="Country")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Healthy Life Expectancy is broadly similar for men and women",
       subtitle="Healthy Life Expectancy at Birth based on 2017-19 data",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HLEGap.tiff", units="in", width=8, height=6, res=500)
analysis.data %>% 
  filter(!is.na(sex) & !is.na(country)) %>% 
  mutate(flag=case_when(
    ctyua17nm=="Orkney Islands" & sex=="Female" ~ "a",
    ctyua17nm=="Nottingham" & sex=="Female" ~ "a",
    ctyua17nm=="Richmond upon Thames" & sex=="Male" ~ "a",
    ctyua17nm=="Blackpool" & sex=="Male" ~ "a",
    TRUE ~ "b"
  )) %>% 
  ggplot(aes(x=sex, y=HLE, colour=flag))+
  geom_jitter(width=0.05, show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Healthy Life Expectancy")+
  scale_colour_manual(values=c("Tomato", "Grey70"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  annotate("text", x=1.05, y=73.3, label="Orkney", colour="Tomato", hjust=0)+
  annotate("text", x=1.05, y=54.2, label="Nottingham", colour="Tomato", hjust=0)+
  annotate("text", x=2.05, y=71.9, label="Richmond upon Thames", colour="Tomato", hjust=0)+
  annotate("text", x=2.05, y=53.3, label="Blackpool", colour="Tomato", hjust=0)+
  annotate("text", x=1.2, y=(73.3+54.2)/2, label="Gap of 19.1 years", angle=90, 
           colour="Tomato2", fontface=2)+
  annotate("text", x=2.2, y=(71.9+53.3)/2, label="Gap of 18.6 years", angle=90, 
           colour="Tomato2", fontface=2)+
  labs(title="There is huge inequality in Healthy Life Expectancy across the UK",
       subtitle="Average years of life lived in 'good' or 'very good' health by Local Authority",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

analysis.data %>% 
  filter(sex=="Male") %>% 
  mutate(ULE=LE-HLE) %>% 
  gather(metric, outcome, c("HLE", "ULE")) %>% 
  ggplot(aes(x=fct_reorder(ctyua17cd, outcome), y=outcome, fill=metric))+
  geom_col()


