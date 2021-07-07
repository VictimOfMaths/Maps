rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(snakecase)
library(lubridate)
library(scales)
library(gtools)
library(sf)
library(extrafont)
library(ragg)
library(ggtext)
library(cowplot)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Get vaccination rates
tempvax <- tempfile()
vaxurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/COVID-19-weekly-announced-vaccinations-01-July-2021-revised.xlsx"
tempvax <- curl_download(url=vaxurl, destfile=tempvax, quiet=FALSE, mode="wb")

vaxnos <- read_excel(tempvax, sheet="Constituency", range="B16:R548", col_names=FALSE) %>% 
  rowwise() %>% 
  mutate(vaxed=sum(c_across(`...4`:`...17`))) %>% 
  select(`...1`, `...3`, vaxed) %>% 
  set_names(c("code", "constituency", "vaxed"))

#Get populations
vaxpop <- read_excel(tempvax, sheet="Population estimates (NIMS)", range="AP16:BE548", col_names=FALSE) %>% 
#vaxpop <- read_excel(tempvax, sheet="Population estimates (ONS)", range="DC16:DR548", col_names=FALSE) %>% 
  rowwise() %>% 
  mutate(pop=sum(c_across(`...3`:`...16`))) %>% 
  select(`...1`, `...2`, pop) %>% 
  set_names(c("code", "constituency", "pop"))

vaxdata <- vaxnos %>% 
  merge(vaxpop) %>% 
  mutate(per_vax=vaxed/pop,
         constituency=toupper(constituency))

#Get GE2019 vote data
tempvote <- tempfile()
voteurl <- "https://researchbriefings.files.parliament.uk/documents/CBP-8647/1918_2019election_results.csv"
tempvote <- curl_download(url=voteurl, destfile=tempvote, quiet=FALSE, mode="wb")

votedata <- read.csv(tempvote) %>% 
  #Keep only English data as other parties are too large a factor elsewhere
  filter(!country.region %in% c("Scotland", "Wales", "Northern Ireland") & election==2019) %>% 
  select(constituency, lab_share) %>% 
  #Tidy up the constituency names
  mutate(constituency=toupper(gsub("&", "and", constituency)))

#Merge
data <- vaxdata %>% 
  merge(votedata, all=TRUE)

#Save data for anyone who wants to draw their own map
write.csv(data, "Data/COVIDVaxvsVLabConstits.csv")

#Scatter plot
agg_tiff("Outputs/COVIDVaxvsLabour.tiff", units="in", width=7, height=7, res=800)
ggplot(data, aes(x=lab_share, y=per_vax))+
  geom_point(shape=21, fill="tomato", alpha=0.7)+
  scale_x_continuous(name="Proportion of votes cast for Labour",
                     labels=label_percent(accuracy=1))+
  scale_y_continuous(name="Proportion of population vaccinated\nwith at least one dose",
                     label=label_percent(accuracy=1), limits=c(0,1))+
  theme_custom()+
  coord_equal()+
  labs(title="Labour-voting constituencies have lower vaccination rates",
       subtitle="Proportion of votes cast for Labour in the 2019 UK General Election against 1st dose\nCOVID vaccination coverage in English parliamentary constituencies.",
       caption="Vaccination data from NHS England | Voting data from the House of Commons Library | Plot by @VictimOfMaths")

dev.off()

#Set up for bivariate map
#Define tertiles
plotdata <- data %>% 
  filter(!is.na(per_vax)) %>% 
  mutate(vaxtert=quantcut(per_vax, 3, labels=FALSE, na.rm=TRUE),
         votetert=quantcut(lab_share, 3, labels=FALSE, na.rm=TRUE),
         key=case_when(
           vaxtert==1 & votetert==1 ~ 1,
           vaxtert==1 & votetert==2 ~ 2,
           vaxtert==1 & votetert==3 ~ 3,
           vaxtert==2 & votetert==1 ~ 4,
           vaxtert==2 & votetert==2 ~ 5,
           vaxtert==2 & votetert==3 ~ 6,
           vaxtert==3 & votetert==1 ~ 7,
           vaxtert==3 & votetert==2 ~ 8,
           vaxtert==3 & votetert==3 ~ 9),
         fillcolour=case_when(
           key==1 ~ "#00cfc1", key==2 ~ "#a0dcdd", key==3 ~ "#f0f0f0",
           key==4 ~ "#44b4cb", key==5 ~ "#afa7b7", key==6 ~ "#ffa2aa",
           key==7 ~ "#6d87cc", key==8 ~ "#c066b2", TRUE ~ "#ff3968"))
           
#generate dataframe for key
keydata <- plotdata %>%
  filter(!is.na(fillcolour)) %>%
  group_by(votetert, vaxtert) %>%
  summarise(RGB=unique(fillcolour))

#Set up legend
key <- ggplot(keydata)+
  geom_tile(aes(x=votetert, y=vaxtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More Labour votes" %->%  ""),
       y = expression("More vaccinations" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 10),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank(),
    text=element_text(family="Lato", colour="White"),
    plot.background=element_rect(fill="Grey10", colour="Grey10"))+
  # quadratic tiles
  coord_fixed()

#Download Carl Baker's lovely map
parl <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/Constituencies.gpkg")
parl <- curl_download(url=source, destfile=parl, quiet=FALSE, mode="wb")

Background <- st_read(parl, layer="5 Background") %>% 
  filter(Name=="England & Wales")

votes <- st_read(parl, layer="4 Constituencies") %>%
  mutate(constituency=toupper(pcon.name)) %>% 
  filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")) %>% 
  left_join(plotdata, by="constituency", all=TRUE)

Cities <- st_read(parl, layer="3 City outlines") %>% 
  filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")) 

Groups <- st_read(parl, layer="2 Group outlines") %>% 
  filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")) 

Group_labels <- st_read(parl, layer="1 Group names") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1)) %>%
  filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")) 


maxdatelab <- "27th June 2021"

#Set up labels
labels <- data.frame(x=c(13, 11, 53, 50), y=c(45, 12, 15, 35),
                     label=c("<span style='color:#ff3968;'>Red<span style='color:white;'> areas are Labour-leaning and have high vaccine coverage",
                             "<span style='color:#6d87cc;'>Blue<span style='color:white;'> areas are Conservative-leaning and have high vaccine coverage",
                             "<span style='color:#f0f0f0;'>White<span style='color:white;'> areas are Labour-leaning and have low vaccine coverage",
                             "<span style='color:#00cfc1;'>Turquoise<span style='color:white;'> areas are Conservative-leaning and have low vaccine coverage"))

map <-  ggplot()+
  geom_sf(data=Background, aes(geometry=geom), fill="Grey10", colour="White")+
  geom_sf(data=votes, aes(geometry=geom, fill=fillcolour), colour="White", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="White")+
  geom_sf(data=Cities, aes(geometry=geom), fill=NA, colour="White")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="White")+
  scale_fill_identity()+
  theme_void()+
  labs(title="Labour-voting constituencies have lower vaccination rates",
       subtitle=paste0("Proportion of votes cast for Labour in the 2019 UK General Election compared to the proportion of the population\nwho have received at least 1 dose of COVID vaccine in England. Vaccination data up to ",
                       maxdatelab, "."),
       caption="Vaccination data from NHS England | Voting data and cartogram from the House of Commons Library | Plot by @VictimOfMaths")+
  theme(text=element_text(family="Lato", colour="White"), plot.title.position="plot",
        plot.caption.position="plot", plot.title=element_text(face="bold", size=rel(1.65), family="Merriweather"),
        plot.background=element_rect(fill="Grey10", colour="Grey10"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
        plot.subtitle=element_text(family="Merriweather", size=rel(0.9)))+
  geom_textbox(data=labels, aes(x=x, y=y, label=label), 
               width=grid::unit(0.12, "npc"), hjust=0, vjust=1, halign=0.5, size=rel(3),
               fill="Grey10", colour="Grey10", box.padding = grid::unit(rep(0, 4), "pt"))


agg_png("Outputs/COVIDVaxvsLabourMap.png", units="in", width=9, height=10.2, res=800)
ggdraw()+
  draw_plot(map, 0,0,1,1)+
  draw_plot(key, 0.66,0.66,0.25,0.25)
dev.off()

