rm(list=ls())

library(tidyverse)
library(extrafont)
library(paletteer)
library(scales)
library(curl)
library(xml2)
library(rvest)
library(readxl)
library(sf)
library(ragg)
library(stringr)
library(fingertipsR)

#Grab html table of new alcohol & drug treatment funding
url2425 <- "https://www.gov.uk/government/publications/extra-funding-for-drug-and-alcohol-treatment-2024-to-2025/additional-drug-and-alcohol-treatment-funding-allocations-2024-to-2025"
temp <- url2425 %>% read_html %>% html_nodes("table")

data2425 <- as.data.frame(html_table(temp[1])) %>% 
  set_names("LAName", "DrugStrategy", "Detox") %>% 
  mutate(DrugStrategy=gsub("£", "", DrugStrategy),
         DrugStrategy=gsub(",", "", DrugStrategy),
         DrugStrategy=as.numeric(DrugStrategy),
         Detox=gsub("£", "", Detox),
         Detox=gsub(",", "", Detox),
         Detox=as.numeric(Detox),
         Total=DrugStrategy+Detox,
         LAName=gsub(" (see note)", "", LAName, fixed=TRUE)) %>% 
  mutate(Year="24/25")

#Grab previous year's allocations
#23/24
url2324 <- "https://www.gov.uk/government/publications/extra-funding-for-drug-and-alcohol-treatment-2023-to-2025/additional-drug-and-alcohol-treatment-funding-allocations-2023-to-2024-and-2024-to-2025"
temp <- url2324 %>% read_html %>% html_nodes("table")

data2324 <- as.data.frame(html_table(temp[1])) %>% 
  select(1:3) %>% 
  set_names("LAName", "DrugStrategy", "Detox") %>% 
  mutate(DrugStrategy=gsub("£", "", DrugStrategy),
         DrugStrategy=gsub(",", "", DrugStrategy),
         DrugStrategy=as.numeric(DrugStrategy),
         Detox=gsub("£", "", Detox),
         Detox=gsub(",", "", Detox),
         Detox=as.numeric(Detox),
         Total=DrugStrategy+Detox,
         LAName=gsub(" [note]", "", LAName, fixed=TRUE)) %>% 
  mutate(Year="23/24")

#22/23
url2223 <- "https://www.gov.uk/government/publications/extra-funding-for-drug-and-alcohol-treatment-2022-to-2023/additional-drug-and-alcohol-treatment-funding-allocations-2022-to-2023"
temp <- url2223 %>% read_html %>% html_nodes("table")

data2223 <- as.data.frame(html_table(temp[1])) %>% 
  set_names("LAName", "DrugStrategy", "Detox") %>% 
  mutate(DrugStrategy=gsub("£", "", DrugStrategy),
         DrugStrategy=gsub(",", "", DrugStrategy),
         DrugStrategy=as.numeric(DrugStrategy),
         Detox=gsub("£", "", Detox),
         Detox=gsub(",", "", Detox),
         Detox=as.numeric(Detox),
         Total=DrugStrategy+Detox,
         LAName=gsub(" (enhanced funding area)", "", LAName, fixed=TRUE),
         LAName=gsub("[footnote 1]", "", LAName, fixed=TRUE),
         Year="22/23",
         LAName=if_else(LAName=="St Helens", "St. Helens", LAName))

#Combine and merge with population figures
rawdata <- bind_rows(data2223, data2324, data2425)

#Get mid-year population estimates (have to use 2021, because ONS)
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2021/ukpopestimatesmid2021on2021geographyfinal.xls"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

popdata <- read_excel(temp, sheet="MYE2 - Persons", range="A8:D428") %>% 
  select(-Geography) %>% 
  set_names("LACode", "LAName", "Pop") %>% 
  filter(substr(LACode, 1, 1)=="E") %>% 
  mutate(LAName=str_trim(LAName, side="right"),
         LAName=case_when(
           LAName %in% c("Allerdale", "Carlisle", "Copeland") ~ "Cumberland",
           LAName %in% c("Barrow-in-Furness", "Eden", "South Lakeland") ~ "Westmorland and Furness",
           TRUE ~ LAName),
         LACode=case_when(
           LAName=="Cumberland" ~ "E06000063",
           LAName=="Westmorland and Furness" ~ "E06000064",
           TRUE ~ LACode)) %>% 
  group_by(LAName, LACode) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

interimdata <- rawdata %>% 
  filter(LAName %in% c("Cornwall and Isles of Scilly", 
                       "Northamptonshire", "Cumbria")) %>% 
  mutate(SharedGroup=case_when(
    LAName=="Cornwall and Isles of Scilly" ~ 1,
    LAName=="Northamptonshire" ~ 2,
    LAName=="Cumbria" ~ 3),
         LAName=case_when(
           LAName=="Cornwall and Isles of Scilly" ~ "Cornwall",
           LAName=="Northamptonshire" ~ "West Northamptonshire",
           LAName=="Cumbria" ~ "Cumberland",
           TRUE ~ LAName)) %>% 
  bind_rows(rawdata %>% 
              mutate(SharedGroup=case_when(
                LAName=="Cornwall and Isles of Scilly" ~ 1,
                LAName=="Northamptonshire" ~ 2,
                LAName=="Cumbria" ~ 3),
                     LAName=case_when(
                       LAName=="Cornwall and Isles of Scilly" ~ "Isles of Scilly",
                       LAName=="Northamptonshire" ~ "North Northamptonshire",
                       LAName=="Cumbria" ~ "Westmorland and Furness",
                       TRUE ~ LAName))) %>% 
  merge(popdata, all=TRUE) %>% 
  mutate(LACode=case_when(
    LAName=="Somerset" ~ "E06000066", 
    LAName=="North Yorkshire" ~ "E06000065",
    TRUE ~ LACode))

data <- interimdata %>% 
  filter(!is.na(SharedGroup)) %>% 
  group_by(Year, SharedGroup) %>% 
  mutate(Pop=sum(Pop)) %>% 
  ungroup %>% 
  bind_rows(interimdata %>% filter(is.na(SharedGroup))) %>% 
  mutate(InvPerPop=Total/Pop)

#Download Carl Baker's lovely hex cartograms
hexmap <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-uppertier-2023.gpkg")
hexmap <- curl_download(url=source, destfile=hexmap, quiet=FALSE, mode="wb")

Background <- st_read(hexmap, layer="LocalAuthorities-uppertier 2023 version — 8 Background")

UTLAs <- st_read(hexmap, layer="LocalAuthorities-uppertier 2023 version — 7 UTLA-2023") %>% 
  left_join(data, by=c("cua.code"="LACode"))

Groups <- st_read(hexmap, layer="LocalAuthorities-uppertier 2023 version — 2 Group 2023")

Group_labels <- st_read(hexmap, layer="LocalAuthorities-uppertier 2023 version — 1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

#Plot
agg_tiff("Outputs/AlcDrugFundingMap.tiff", units="in", width=9, height=6, res=800)
ggplot()+
  geom_sf(data=Background %>% filter(Name=="England & Wales"), aes(geometry=geom), fill="#a1d99b")+
  geom_sf(data=UTLAs %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom, fill=InvPerPop), colour=NA)+
  geom_sf(data=Groups %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.3), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.amp", name="Funding per person",
                         labels=dollar_format(prefix="£"), limits=c(0,NA))+
  facet_wrap(~Year)+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  theme_void()+
  theme(legend.position="top", plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Additional funding for alcohol and drug treatment",
       subtitle="Additional funding allocated to Local Authority specialist alcohol and drug treatment services on a per capita basis\n\n",
       caption="Data from Department of Health and Social Care and ONS\nCartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

dev.off()

#Merge years
alldata <- data %>% 
  group_by(LAName, LACode) %>% 
  summarise(InvPerPop=sum(InvPerPop), .groups="drop") %>% 
  filter(!is.na(InvPerPop))

UTLAs2 <- st_read(hexmap, layer="LocalAuthorities-uppertier 2023 version — 7 UTLA-2023") %>% 
  left_join(data, by=c("cua.code"="LACode"))

agg_tiff("Outputs/AlcDrugFundingMapCombined.tiff", units="in", width=7, height=8, res=800)
ggplot()+
  geom_sf(data=Background %>% filter(Name=="England & Wales"), aes(geometry=geom), fill="#a1d99b")+
  geom_sf(data=UTLAs %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom, fill=InvPerPop), colour=NA)+
  geom_sf(data=Groups %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.3), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.amp", name="Funding\nper person",
                         labels=dollar_format(prefix="£"), limits=c(0,NA))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Additional funding for alcohol and drug treatment",
       subtitle="Combined additional funding allocated to Local Authority specialist alcohol and drug\ntreatment services for 2022/23, 23/24 and 24/25 on a per capita basis",
       caption="Data from Department of Health and Social Care and ONS\nCartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

dev.off()

#Import LA-level deaths data
#Read in English & Welsh data at LTLA level
#DRDs 2019-21
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/drugmisusedeathsbylocalauthority/current/2021localauthorities.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

DRD_LA.ew <-  read_excel(temp, sheet="Table 6", range="A9:DM431", col_names=FALSE) %>% 
  mutate(`...2`=coalesce(`...2`, `...3`, `...4`)) %>% 
  select(`...1`, `...2`, `...6`, `...12`, `...18`, `...24`, `...30`, `...36`, `...42`, `...48`, `...54`,
         `...60`, `...66`, `...72`, `...78`, `...84`, `...90`, `...96`, `...102`, `...108`, `...114`) %>% 
  set_names("Area Codes", "Area Names", "2019-21", "2018-20", "2017-19", "2016-18", "2015-17", "2014-16",
            "2013-15", "2012-14", "2011-13", "2010-12", "2009-11", "2008-10", "2007-09", "2006-08", 
            "2005-07", "2004-06", "2003-05", "2002-04", "2001-03") %>% 
  filter(!is.na(`Area Codes`)) %>% 
  mutate(across(starts_with("20"), ~as.numeric(gsub(":", "", .x)))) %>% 
  gather(year, DRD, c(3:21)) %>% 
  mutate(`Area Names`=gsub(", City of", "", `Area Names`),
         `Area Names`=gsub(", County of", "", `Area Names`),
         `Area Names`=gsub(" UA", "", `Area Names`),
         `Area Names`=gsub("&", "and", `Area Names`),
         `Area Names`=gsub("King's", "King’s", `Area Names`),
         `Area Names`=gsub(" /.*", "", `Area Names`))

ASD_LA.e <- fingertips_data(IndicatorID=91380, AreaTypeID=401) %>% 
  filter(Sex=="Persons") %>% 
  select(AreaCode, AreaName, Value, Timeperiod) %>% 
  set_names("Area Codes", "Area Names", "ASD", "year") %>% 
  mutate(year=gsub(" ", "", year),
         `Area Names`=gsub(" UA", "", `Area Names`))

Deaths=DRD_LA.ew %>% 
  filter(year=="2019-21") %>% 
  select(-year) %>% 
  set_names("LACode", "LAName", "DRD") %>% 
  merge(ASD_LA.e %>% filter(year=="2017-19") %>% 
          select(-year) %>% 
          set_names("LACode", "LAName", "ASD"), all=TRUE) %>% 
  mutate(Deaths=ASD+DRD)

alldata %>% merge(Deaths) %>% 
  ggplot(aes(x=Deaths, y=InvPerPop))+
  geom_point(shape=21)+
  theme_classic()

