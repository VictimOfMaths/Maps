rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(sf)
library(extrafont)
library(scales)
library(gtools)

#Download self-reported WFH data from ONS
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
                         name="Proportion ever working from home", limits=c(0,NA),
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

#Download case data from the PHE dashboard
url2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumCasesBySpecimenDateRate&metric=newCasesBySpecimenDateRollingRate&format=csv"
temp2 <- tempfile()
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

casedata <- read.csv(temp2) %>% 
  mutate(date=as.Date(date)) %>% 
  rename(Lacode=areaCode, cum.cases=cumCasesBySpecimenDateRate, 
         cases=newCasesBySpecimenDateRollingRate)

maxdate=max(casedata$date)-days(1)
maxrolldate=max(casedata$date[!is.na(casedata$cases)])

#Pull out only the most recent complete day's data
casedata <- casedata %>% 
  filter(date==maxdate) %>% 
  select(Lacode, cum.cases) %>% 
  merge(casedata %>% 
              filter(date==maxrolldate) %>% 
              select(Lacode, cases)) %>% 
  #And merge in the WFH data
  merge(rawdata %>% filter(response=="Total"), all=TRUE)

#merge into geographical data
ltlacases <- st_read(ltla, layer="4 LTLA-2019") %>% 
  left_join(casedata, by="Lacode") %>% 
  filter(RegionNation!="Northern Ireland") %>% 
  #Set up colours for bivariate map
  mutate(cum.casetert=quantcut(cum.cases, q=3, labels=FALSE),
         casetert=quantcut(cases, q=3, labels=FALSE),
         wfhtert=quantcut(prop, q=3, labels=FALSE),
         key1=case_when(
           cum.casetert==1 & wfhtert==1 ~ 1,
           cum.casetert==1 & wfhtert==2 ~ 2,
           cum.casetert==1 & wfhtert==3 ~ 3,
           cum.casetert==2 & wfhtert==1 ~ 4,
           cum.casetert==2 & wfhtert==2 ~ 5,
           cum.casetert==2 & wfhtert==3 ~ 6,
           cum.casetert==3 & wfhtert==1 ~ 7,
           cum.casetert==3 & wfhtert==2 ~ 8,
           TRUE ~ 9),
         fillcolour1=case_when(
           key1==1 ~ "#f0f0f0", key1==2 ~ "#a0dcdd", key1==3 ~ "#00cfc1",
           key1==4 ~ "#ffa2aa", key1==5 ~ "#afa7b7", key1==6 ~ "#44b4cb",
           key1==7 ~ "#ff3968", key1==8 ~ "#c066b2", TRUE ~ "#6d87cc"),
         key2=case_when(
           casetert==1 & wfhtert==1 ~ 1,
           casetert==1 & wfhtert==2 ~ 2,
           casetert==1 & wfhtert==3 ~ 3,
           casetert==2 & wfhtert==1 ~ 4,
           casetert==2 & wfhtert==2 ~ 5,
           casetert==2 & wfhtert==3 ~ 6,
           casetert==3 & wfhtert==1 ~ 7,
           casetert==3 & wfhtert==2 ~ 8,
           TRUE ~ 9),
         fillcolour2=case_when(
           key2==1 ~ "#f0f0f0", key2==2 ~ "#a0dcdd", key2==3 ~ "#00cfc1",
           key2==4 ~ "#ffa2aa", key2==5 ~ "#afa7b7", key2==6 ~ "#44b4cb",
           key2==7 ~ "#ff3968", key2==8 ~ "#c066b2", TRUE ~ "#6d87cc"))

#generate dataframe for key
keydata1 <- ltlacases %>%
  filter(!is.na(fillcolour1)) %>%
  group_by(cum.casetert, wfhtert) %>%
  summarise(RGB=unique(fillcolour1))

keydata2 <- ltlacases %>%
  filter(!is.na(fillcolour2)) %>%
  group_by(casetert, wfhtert) %>%
  summarise(RGB=unique(fillcolour2))

ggplot(ltlacases, aes(x=cases, y=prop))+
  geom_point()

plot2 <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases, 
          aes(geometry=geom, fill=fillcolour1), colour=NA)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black", family="Lato")+
  scale_fill_identity(na.value="Black")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), plot.title.position = "panel")+
  labs(title="Comparing total COVID-19 case rates across the pandemic\nwith rates of working from home",
       subtitle="Total COVID case rates since March 2020  and self-reported prevalence of ever working from home\nbased on data from the 2020 Annual Population Survey. The survey asked about working\narrangements 'in normal times' so may not fully reflect working patterns during the pandemic.",       
       caption="Data from coronavirus.data.gov.uk and ONS, cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

key1 <- ggplot(keydata1)+
  geom_tile(aes(x=cum.casetert, y=wfhtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More COVID-19 cases" %->%  ""),
       y = expression("More people working from home" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), text=element_text(family="Lato"),
    axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

agg_tiff("Outputs/ONSBivariateWFHxcumcases.tiff", units="in", width=8, height=10, res=800)
ggdraw()+
  draw_plot(plot2, 0,0,1,1)+
  draw_plot(key1, 0.66,0.62,0.28,0.28)
dev.off()

plot3 <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases, 
          aes(geometry=geom, fill=fillcolour2), colour=NA)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black", family="Lato")+
  scale_fill_identity(na.value="Black")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), plot.title.position = "panel")+
  labs(title="Comparing current COVID-19 case rates with\nrates of working from home",
       subtitle="Current 7-day average COVID case rates and self-reported prevalence of ever working from home\nbased on data from the 2020 Annual Population Survey. The survey asked about working\narrangements 'in normal times' so may not fully reflect working patterns during the pandemic.",       
       caption="Data from coronavirus.data.gov.uk and ONS, cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

key2 <- ggplot(keydata2)+
  geom_tile(aes(x=casetert, y=wfhtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More COVID-19 cases" %->%  ""),
       y = expression("More people working from home" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), text=element_text(family="Lato"),
    axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

agg_tiff("Outputs/ONSBivariateWFHxcases.tiff", units="in", width=8, height=10, res=800)
ggdraw()+
  draw_plot(plot3, 0,0,1,1)+
  draw_plot(key2, 0.66,0.62,0.28,0.28)
dev.off()

