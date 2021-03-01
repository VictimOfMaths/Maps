rm(list=ls())

library(curl)
library(readxl)
library(tidyverse)
library(sf)
library(paletteer)
library(ragg)
library(ggridges)

#Download LSOA-level indicators
temp <- tempfile()
temp <- curl_download(url="https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833992/File_8_-_IoD2019_Underlying_Indicators.xlsx", 
                      destfile=temp, quiet=FALSE, mode="wb")

LSOAIMD <- read_excel(temp, sheet="IoD2019 Health Domain", range="A2:H32845", col_names=FALSE) %>% 
  select(1, 5:8) %>% 
  rename(LSOA11CD=`...1`, PYLLs=`...5`, Benefits=`...6`, Emergency=`...7`, Mood=`...8`)

#Download LSOA to MSOA lookup
temp <- tempfile()
source <- ("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

lookup <- read.csv(temp) %>% 
  select(LSOA11CD, MSOA11CD, RGN11NM) %>% 
  unique()

#Merge into IMD data
LSOAIMD <- merge(LSOAIMD, lookup, by="LSOA11CD")

#Bring in population data for LSOAs
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimatesnationalstatistics%2fmid2019sape22dt13/sape22dt13mid2019lsoabroadagesestimatesunformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

pop <- read_excel(file.path(temp2, "SAPE22DT13-mid-2019-lsoa-Broad_ages-estimates-unformatted.xlsx"),
                  sheet="Mid-2019 Persons", range="A6:G34758", col_names=FALSE)[,c(1,7)]
colnames(pop) <- c("LSOA11CD", "pop")

#Merge into IMD data
LSOAIMD <- merge(LSOAIMD, pop)

#Calculate IMD rank at MSOA level as weighted average of LSOA level ranks, weight by population
IMD_MSOA <- LSOAIMD %>% 
  group_by(MSOA11CD) %>% 
  summarise(PYLLs.score=weighted.mean(PYLLs, pop), 
            Benefits.score=weighted.mean(Benefits, pop),
            Emergency.score=weighted.mean(Emergency), 
            Mood.score=weighted.mean(Mood, pop), 
            pop=sum(pop)) %>% 
  ungroup() 

#Bring in median population per MSOA
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates/mid2019sape22dt4/sape22dt4mid2019msoasyoaestimatesunformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

pop <- read_excel(file.path(temp2, "SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx"),
                  sheet="Mid-2019 Persons", range="A6:CT6796", col_names=FALSE) %>% 
  select(c(1, 8:98)) %>% 
  rename(MSOA11CD=`...1`) %>% 
  gather(age, pop, c(2:92)) %>% 
  mutate(age=as.numeric(substr(age, 4, 5))-8) %>% 
  group_by(MSOA11CD) %>% 
  mutate(weightpop=cumsum(pop)/sum(pop), error=weightpop-0.5) %>% 
  arrange(abs(error)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(MSOA11CD, age) %>% 
  rename(medianage=age)

#Merge into rest of data
IMD_MSOA <- merge(IMD_MSOA, pop) %>% 
  rename(msoa11cd=MSOA11CD)


agg_tiff("Outputs/IMDPYLLRidges.tiff", units="in", width=8, height=6, res=800)
ggplot(IMD_MSOA)+
  geom_density_ridges_gradient(aes(x=PYLLs.score, y=medianage, group=medianage, fill=stat(x)), 
                               rel_min_height=0.01, show.legend=FALSE)+
  scale_x_continuous(name="Potential Years of Life Lost score\nLower = better")+
  scale_y_continuous(name="Median age")+
  scale_fill_paletteer_c("scico::vik")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Potential Years of Life Lost are highest in younger areas,\neven after age-standardising",
       subtitle="Premature mortality sub-domain score from the Index of Multiple Deprivation for English MSOAs",
       caption="Date from MHCLG | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/IMDBenefitsRidges.tiff", units="in", width=8, height=6, res=800)
ggplot(IMD_MSOA)+
  geom_density_ridges_gradient(aes(x=Benefits.score, y=medianage, group=medianage, fill=stat(x)), 
                               rel_min_height=0.01, show.legend=FALSE)+
  scale_x_continuous(name="Benefits claimed due to poor health score\nLower = better")+
  scale_y_continuous(name="Median age")+
  scale_fill_paletteer_c("scico::vik")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Work-limiting ill health is highest in younger areas,\neven after age-standardising",
       subtitle="Ill health and disability that limits work sub-domain score from the Index of Multiple Deprivation\nfor English MSOAs",
       caption="Date from MHCLG | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/IMDEmergencyRidges.tiff", units="in", width=8, height=6, res=800)
ggplot(IMD_MSOA)+
  geom_density_ridges_gradient(aes(x=Emergency.score, y=medianage, group=medianage, fill=stat(x)), 
                               rel_min_height=0.01, show.legend=FALSE)+
  scale_x_continuous(name="Emergency hospital admissions score\nLower = better")+
  scale_y_continuous(name="Median age")+
  scale_fill_paletteer_c("scico::vik")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Emergency admissions to hospital are higher in younger areas",
       subtitle="Acute morbidity sub-domain score from the Index of Multiple Deprivation for English MSOAs",
       caption="Date from MHCLG | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/IMDMoodRidges.tiff", units="in", width=8, height=6, res=800)
ggplot(IMD_MSOA)+
  geom_density_ridges_gradient(aes(x=Mood.score, y=medianage, group=medianage, fill=stat(x)), 
                               rel_min_height=0.01, show.legend=FALSE)+
  scale_x_continuous(name="Mood and anxiety score\nLower better")+
  scale_y_continuous(name="Median age")+
  scale_fill_paletteer_c("scico::vik")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Mental health is worse in older areas",
       subtitle="Mood and anxiety sub-domain score from the Index of Multiple Deprivation for English MSOAs",
       caption="Date from MHCLG | Plot by @VictimOfMaths")
dev.off()

#Download Carl Baker's lovely cartogram
msoa <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
msoa <- curl_download(url=source, destfile=msoa, quiet=FALSE, mode="wb")

BackgroundMSOA <- st_read(msoa, layer="5 Background")

MSOA <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(IMD_MSOA, by="msoa11cd")

GroupsMSOA <- st_read(msoa, layer="2 Groups")

Group_labelsMSOA <- st_read(msoa, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAsMSOA <- st_read(msoa, layer="3 Local authority outlines (2019)")

plotage <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=medianage), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                                          hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("viridis::viridis", 
                         name="Median age")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2)))+
  labs(title="England's cities are young",
       subtitle="Median age for Middle Super Output Areas (MSOAs) in England based on 2019 population estimates",       
       caption="Data from MHCLG and ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/IMDMedianAgeMSOACartogram.tiff", units="in", width=10, height=8, res=800)
plotage
dev.off()

plotPYLLs <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=PYLLs.score), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("scico::vik", 
                         name="Higher scores\nrepresent\ngreater deprivation")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Premature mortality is highest in Northern inner cities",
       subtitle="Sub-domain score from the Index of Multiple Deprivation for Potential Years of Life lost\nbased on deaths before the age of 75. Age-standardised to account for differences in the\nages of MSOA populations",       
       caption="Data from MHCLG and ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/IMDPYLLsMSOACartogram.tiff", units="in", width=10, height=8, res=800)
plotPYLLs
dev.off()

plotBenefits <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=Benefits.score), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("scico::vik", 
                         name="Higher scores\nrepresent\ngreater deprivation")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Work-limiting health problems are greatest in the North West",
       subtitle="Sub-domain score from the Index of Multiple Deprivation for work-limiting morbidity or disability\nbased on receipt of relevant benefits. Age-standardised to account for differences in the ages\nof MSOA populations",       
       caption="Data from MHCLG and ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/IMDBenefitsMSOACartogram.tiff", units="in", width=10, height=8, res=800)
plotBenefits
dev.off()

plotEmergency <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=Emergency.score), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("scico::vik", 
                         name="Higher scores\nrepresent\ngreater deprivation")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Emergency hospital admissions are highest in cities across the Midlands and the North",
       subtitle="Sub-domain score from the Index of Multiple Deprivation for acute morbidity lost\nbased on rates of emergency hospital admissions.",       
       caption="Data from MHCLG and ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/IMDEmergencyMSOACartogram.tiff", units="in", width=10, height=8, res=800)
plotEmergency
dev.off()

Moodplot <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=Mood.score), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("scico::vik", 
                         name="Higher scores\nrepresent\ngreater deprivation")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Mood and anxiety scores are best in London and Birmingham",
       subtitle="Sub-domain score from the Index of Multiple Deprivation for mood and anxiety disorders",       
       caption="Data from MHCLG and ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/IMDMoodMSOACartogram.tiff", units="in", width=10, height=8, res=800)
Moodplot
dev.off()
