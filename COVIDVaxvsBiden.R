rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(scales)
library(gtools)
library(sf)
library(tigris)
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

#Vaccination data form the CDC's web page
#https://covid.cdc.gov/covid-data-tracker/#vaccinations-county-view
vaxurl <- "https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD"

#Voting data via Tony McGovern's GitHub page
#https://github.com/tonmcg/US_County_Level_Election_Results_08-20
voteurl <- "https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv"

tempvax <- tempfile()
tempvote <- tempfile()

tempvax <- curl_download(url=vaxurl, destfile=tempvax, quiet=FALSE, mode="wb")
tempvote <- curl_download(url=voteurl, destfile=tempvote, quiet=FALSE, mode="wb")

vaxdata <- read.csv(tempvax) %>% 
  mutate(date=as.Date(Date, format="%m/%d/%Y")) %>% 
  filter(date==max(date)) %>% 
  select(FIPS, Administered_Dose1_Pop_Pct) %>% 
  set_names(c("FIPS", "per_vax")) %>% 
  mutate(FIPS=as.numeric(FIPS),
         per_vax=if_else(per_vax==0, NA_real_, per_vax/100))

votedata <- read.csv(tempvote) %>% 
  select(county_fips, per_dem) %>% 
  set_names(c("FIPS", "per_dem"))

data <- merge(vaxdata, votedata)

#Save data for anyone who wants to draw their own map
write.csv("Data/COVIDVaxvsVBiden.csv")

#Scatter plot
agg_tiff("Outputs/COVIDVaxvsBiden.tiff", units="in", width=7, height=7, res=800)
ggplot(data, aes(x=per_dem, y=per_vax))+
  geom_point(shape=21, fill="tomato", alpha=0.7)+
  scale_x_continuous(name="Proportion of votes cast for Joe Biden",
                     labels=label_percent(accuracy=1))+
  scale_y_continuous(name="Proportion of population vaccinated\nwith at least one dose",
                     label=label_percent(accuracy=1))+
  theme_custom()+
  coord_equal()+
  labs(title="Biden voting counties have higher vaccination rates",
       subtitle="Proportion of votes case for Joe Biden in the 2020 US presidential election against 1st dose\nCOVID vaccination coverage in US counties reporting data for both.",
       caption="Vaccination data from CDC | Voting data from Tony McGovern | Plot by @VictimOfMaths")
dev.off()

#Set up for bivariate map
#Define tertiles
plotdata <- data %>% 
  filter(!is.na(per_vax)) %>% 
  mutate(vaxtert=quantcut(per_vax, 3, labels=FALSE, na.rm=TRUE),
         votetert=quantcut(per_dem, 3, labels=FALSE, na.rm=TRUE),
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
           key==1 ~ "#f0f0f0", key==2 ~ "#a0dcdd", key==3 ~ "#00cfc1",
           key==4 ~ "#ffa2aa", key==5 ~ "#afa7b7", key==6 ~ "#44b4cb",
           key==7 ~ "#ff3968", key==8 ~ "#c066b2", TRUE ~ "#6d87cc"),
         GEOID=if_else(str_length(FIPS)==4, 
                       as.character(paste0("0", FIPS)), as.character(FIPS)))

#generate dataframe for key
keydata <- plotdata %>%
  filter(!is.na(fillcolour)) %>%
  group_by(votetert, vaxtert) %>%
  summarise(RGB=unique(fillcolour))

#Set up legend
key <- ggplot(keydata)+
  geom_tile(aes(x=votetert, y=vaxtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More Biden votes" %->%  ""),
       y = expression("Higher vaccination rates" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank(),
    text=element_text(family="Lato", colour="White"),
    plot.background=element_rect(fill="Grey10", colour="Grey10"))+
  # quadratic tiles
  coord_fixed()

#Pull in county-level map
shapefile <- counties(resolution="500k")

stateshapes <- states(resolution="500k")

mapdata <- left_join(shapefile, plotdata)

map <- ggplot(mapdata, aes(geometry=geometry, fill=fillcolour))+
  geom_sf(colour="white", size=0.1)+
  geom_sf(data=stateshapes, aes(geometry=geometry), fill=NA, )
  xlim(-125, -66)+
  ylim(26, 46)+
  scale_fill_identity()+
  theme_void()+
  labs(title="US Counties with high vaccination rates are overwhelmingly Democrat-leaning",
       subtitle="Proportion of votes case for Joe Biden in the 2020 US presidential election compared to the proportion of the population who have received\nat least 1 dose of COVID vaccine in US counties. Counties with missing vaccination data appear in black.\n",
       caption="Vaccination data from CDC | Voting data from Tony McGovern | Plot by @VictimOfMaths")+
  theme(text=element_text(family="Lato", colour="White"), plot.title.position="plot",
        plot.caption.position="plot", plot.title=element_text(face="bold", size=rel(1.6)),
        plot.background=element_rect(fill="Grey10", colour="Grey10"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))+
#These boxes need moving about, but even just adding one of them in increases the rendering time of the plot *massively* for reasons I don't understand
  geom_textbox(aes(x=-120, y=35, label="<span style='color:#ff3968;'>Red<span style='color:white;'> areas are GOP-leaning and have high vaccine coverage"), 
              width=grid::unit(0.1, "npc"), hjust=0, vjust=1, halign=0.5, size=rel(2.5),
               fill="Grey10", colour="Grey10", box.padding = grid::unit(rep(0, 4), "pt"))+
  geom_textbox(aes(x=-100, y=29, label="<span style='color:#6d87cc;'>Blue<span style='color:white;'> areas are Dem-leaning and have high vaccine coverage"), 
               width=grid::unit(0.1, "npc"), hjust=0, vjust=1, halign=0.5, size=rel(2.5),
               fill="Grey10", colour="Grey10", box.padding = grid::unit(rep(0, 4), "pt"))+
  geom_textbox(aes(x=-85, y=45, label="<span style='color:#f0f0f0;'>White<span style='color:white;'> areas are GOP-leaning and have low vaccine coverage"), 
               width=grid::unit(0.1, "npc"), hjust=0, vjust=1, halign=0.5, size=rel(2.5),
               fill="Grey10", colour="Grey10", box.padding = grid::unit(rep(0, 4), "pt"))+
  geom_textbox(aes(x=-80, y=32, label="<span style='color:#00cfc1;'>Turquoise<span style='color:white;'> areas are Dem-leaning and have low vaccine coverage"), 
               width=grid::unit(0.1, "npc"), hjust=0, vjust=1, halign=0.5, size=rel(2.5),
               fill="Grey10", colour="Grey10", box.padding = grid::unit(rep(0, 4), "pt"))

  
agg_tiff("Outputs/COVIDVaxvsBidenMap.tiff", units="in", width=10, height=5.4, res=200)
ggdraw()+
  draw_plot(map, 0,0,1,1)+
  draw_plot(key, 0,0.01,0.3,0.3)
dev.off()
