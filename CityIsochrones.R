#devtools::install_github("rCarto/osrm")
library(osrm)
library(sf)
library(tidyverse)
library(ggmap)
library(ragg)
library(extrafont)
library(paletteer)

#Get the isochrones. At this resolution this takes *ages*, res=100 is much more manageable
#(though obviously not as detailed)
London <- osrmIsochrone(loc = c(-0.128059, 51.507725), breaks = seq(from=5, to=60, by=5),
                     returnclass="sf", res=200, osrm.profile="car")
Birmingham <- osrmIsochrone(loc = c(-1.900583, 52.477234), breaks = seq(from=5, to=60, by=5),
                      returnclass="sf", res=200, osrm.profile="car")
Liverpool <- osrmIsochrone(loc = c(-2.980741, 53.404897), breaks = seq(from=5, to=60, by=5),
                      returnclass="sf", res=200, osrm.profile="car")
Nottingham <- osrmIsochrone(loc = c(-1.145328, 52.948077), breaks = seq(from=5, to=60, by=5),
                        returnclass="sf", res=200, osrm.profile="car")
Sheffield <- osrmIsochrone(loc = c(-1.468472, 53.380476), breaks = seq(from=5, to=60, by=5),
                            returnclass="sf", res=200, osrm.profile="car")
Bristol <- osrmIsochrone(loc = c(-2.597515, 51.453532), breaks = seq(from=5, to=60, by=5),
                           returnclass="sf", res=200, osrm.profile="car")
Glasgow <- osrmIsochrone(loc = c(-4.258316, 55.858505), breaks = seq(from=5, to=60, by=5),
                        returnclass="sf", res=200, osrm.profile="car")
Leicester <- osrmIsochrone(loc = c(-1.133487, 52.635145), breaks = seq(from=5, to=60, by=5),
                            returnclass="sf", res=200, osrm.profile="car")
Edinburgh <- osrmIsochrone(loc = c(-3.194443, 55.949118), breaks = seq(from=5, to=60, by=5),
                           returnclass="sf", res=200, osrm.profile="car")
Leeds <- osrmIsochrone(loc = c(-1.547136, 53.795208), breaks = seq(from=5, to=60, by=5),
                        returnclass="sf", res=200, osrm.profile="car")
Cardiff <- osrmIsochrone(loc = c(-3.180315, 51.476967), breaks = seq(from=5, to=60, by=5),
                            returnclass="sf", res=200, osrm.profile="car")
Manchester <- osrmIsochrone(loc = c(-2.236261, 53.480062), breaks = seq(from=5, to=60, by=5),
                           returnclass="sf", res=200, osrm.profile="car")

maps <- bind_rows(Birmingham %>% mutate(city="Birmingham"),
                  Bristol %>% mutate(city="Bristol"), 
                  Cardiff %>% mutate(city="Cardiff"), 
                  Edinburgh %>% mutate(city="Edinburgh"), 
                  Glasgow %>% mutate(city="Glasgow"), 
                  Leeds %>% mutate(city="Leeds"), 
                  Leicester %>% mutate(city="Leicester"), 
                  Liverpool %>% mutate(city="Liverpool"),
                  London %>% mutate(city="London"), 
                  Manchester %>% mutate(city="Manchester"), 
                  Nottingham %>% mutate(city="Nottingham"), 
                  Sheffield %>% mutate(city="Sheffield"))

#Save the isochrones so you don't have to do that ^^^ again
st_write(maps, "Data/UKCityIsochrones.shp")

#Plot
agg_png("IsochroneCities.png", units="in", width=7.3, height=10, res=800, background="Grey10")
ggplot(maps, aes(geometry=geometry, alpha=65-max, fill=city))+
  geom_sf(colour=NA, show.legend=FALSE)+
  scale_fill_paletteer_d("rcartocolor::Bold")+
  lims(x=c(-6,2), y=c(50.8,56.5))+
  theme_void()+
  theme(plot.background=element_rect(fill="Grey10", colour="Grey10"),
        panel.background=element_rect(fill="Grey10", colour="Grey10"),
        text=element_text(colour="cornsilk", family="High Alpine"),
        plot.title.position="plot", plot.caption.position="plot",
        plot.title=element_text(size=rel(6)),
        plot.subtitle=element_text(size=rel(1), family="Belltopo Sans"),
        plot.caption=element_text(size=rel(0.9), family="Belltopo Sans"))+
  labs(title="The Great Escape",
       subtitle="How far can you get by car in an hour from the centre of major British cities?",
       caption="Routing data from OpenStreetMap | Fonts from @sarahbellmaps | Map by @VictimOfMaths")
dev.off()

agg_png("IsochroneCitiesEdges.png", units="in", width=7.3, height=10, res=800, background="Grey10")
ggplot(maps %>% filter(max==60), aes(geometry=geometry, colour=city))+
  geom_sf(fill=NA, show.legend=FALSE, alpha=0.8)+
  scale_colour_paletteer_d("rcartocolor::Bold")+
  lims(x=c(-6,2), y=c(50.8,56.5))+
  theme_void()+
  theme(plot.background=element_rect(fill="Grey10", colour="Grey10"),
        panel.background=element_rect(fill="Grey10", colour="Grey10"),
        text=element_text(colour="cornsilk", family="High Alpine"),
        plot.title.position="plot", plot.caption.position="plot",
        plot.title=element_text(size=rel(6)),
        plot.subtitle=element_text(size=rel(1), family="Belltopo Sans"),
        plot.caption=element_text(size=rel(0.9), family="Belltopo Sans"))+
  labs(title="The Great Escape",
       subtitle="How far can you get by car in an hour from the centre of major British cities?",
       caption="Routing data from OpenStreetMap | Fonts from @sarahbellmaps | Map by @VictimOfMaths")

dev.off()

library(purrr)
library(cowplot)

g <- map(unique(maps$city), function(x) {
  ggplot(maps %>% filter(city==x), aes(geometry=geometry, alpha=65-max))+
    geom_sf(colour=NA, show.legend=FALSE, fill="#CF1C90FF")+
    theme_void()+
    theme(plot.background=element_rect(fill="Grey10", colour="Grey10"),
          panel.background=element_rect(fill="Grey10", colour="Grey10"),
          text=element_text(colour="cornsilk", family="Belltopo Sans"),
          plot.title=element_text(size=rel(1)))+
    labs(title=x)
})

agg_png("IsochroneCitiesFacets.png", units="in", width=8.5, height=10, res=800, background="Grey10")
plot_grid(plotlist=g)+
  theme(plot.background=element_rect(fill="Grey10", colour="Grey10"),
        panel.background=element_rect(fill="Grey10", colour="Grey10"),
        text=element_text(colour="cornsilk", family="High Alpine"),
        plot.title.position="plot", plot.caption.position="plot",
        plot.title=element_text(size=rel(4)),
        plot.subtitle=element_text(size=rel(1), family="Belltopo Sans"),
        plot.caption=element_text(size=rel(0.7), family="Belltopo Sans"))+
  labs(title="The Great Escape",
       subtitle="\nHow far can you get by car in an hour from the centre of major British cities?",
       caption="Routing data from OpenStreetMap | Fonts from @sarahbellmaps | Map by @VictimOfMaths")

dev.off()
