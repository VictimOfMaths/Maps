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
         Total=DrugStrategy+Detox) %>% 
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
         Total=DrugStrategy+Detox) %>% 
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
  mutate(LAName=case_when(
    LAName %in% c("Cornwall", "Isles of Scilly") ~ "Cornwall and Isles of Scilly",
    LAName %in% c("North Northamptonshire", "West Northamptonshire")
  ))

data <- merge(rawdata, popdata, all=TRUE) %>% 
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
ggplot()+
  geom_sf(data=Background %>% filter(Name=="England & Wales"), aes(geometry=geom), fill="#a1d99b")+
  geom_sf(data=UTLAs %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland") & Year=="24/25"), 
          aes(geometry=geom, fill=InvPerPop), colour=NA)+
  geom_sf(data=Groups %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.8), colour="Black")+
  theme_void()

