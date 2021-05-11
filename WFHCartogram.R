rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(sf)
library(extrafont)
library(scales)

url <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/adhocs/13196homeworkingintheukbrokendownbyunitaryandlocalauthoritydistricts2020/localauthorityreftable21.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata=read_excel(temp, sheet="Local Authority District", range="A5:G372", col_names=FALSE) %>% 
  gather(response, prop, c(3:7)) %>% 
  rename(Lacode=`...1`, LAname=`...2`) %>% 
  mutate(response=case_when(
    response=="...3"~ "Never",
    response=="...4" ~ "Mainly",
    response=="...5"~ "Recently",
    response=="...6" ~ "Occasionally",
    TRUE ~ "Total"),
    prop=as.numeric(prop))

#Download Carl Baker's lovely map
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="7 Background")

ltladata <- st_read(ltla, layer="4 LTLA-2019") %>% 
  left_join(rawdata, by="Lacode")

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

plot1 <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltladata %>% filter(response=="Total"), 
          aes(geometry=geom, fill=prop), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1,
                         name="Proportion ever\nworking from home", limits=c(0,NA),
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)),
        text=element_text(family="Roboto"), legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                                barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="People in the South East are more likely to work from home",
       subtitle="Self-reported prevalence of ever working from home based on data from the 2020\nAnnual Population Survey. The survey asked about working arrangements 'in normal times'\nso may not fully reflect working patterns during the pandemic.\n ",
       caption="Data from ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/ONSWFHCartogram.tiff", units="in", width=8, height=10, res=800)
plot1
dev.off()


