setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/panels")

dat<-read.csv("2020panelkim.csv", header = TRUE)
dat_sum<-read.csv("panelsum.csv", header = TRUE)

library(ggplot2)

dat_sum$year(as.factor(dat_sum$year))

graph<-ggplot(dat_sum)+
  geom_bar(aes(x = as.factor(year), y = reported), stat = "identity", color = "black", fill = "white")+
  geom_bar(aes(x = as.factor(year), y = total), stat = "identity", fill = "#89BD9E")+
  geom_text(aes(x = as.factor(year), y = total, label=total), position=position_dodge(width=0.9), vjust=-0.25)+
  geom_text(aes(x = as.factor(year), y = reported, label=reported), position=position_dodge(width=0.9), vjust=-0.25)+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
                )+
  labs(x = "Year", 
       y = "Total")

graph

ggsave("2020panelsum.png", dpi = 500, height = 12, width = 28, units = "cm")


#map now

library(maps)

worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot


library(ggmap)
library(stringr)
library(tidyverse)
map.world <- map_data("world")

dat_c<-read.csv("panelsum1.csv", header = TRUE)

map.world_joined <- full_join(map.world, dat_c, by = c('region' = 'country'))

map1<-ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = europ_map)) +
 # geom_point(data = df.country_points, aes(x = lon, y = lat), color = "#e60000") +
#  scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  labs(title = '2020 PT Panel Grading'
       ,subtitle = "PMDDL") +
  theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 30)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )

map1


europ_map <- map_data("world", region = c(
  "Germany", 
  "Austria",
  "Belgium",
  "Bulgaria",
  "Chipre",
  "Croacia",
  "Denmark",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Estonia",
  "Finland",
  "France",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Norway",
  "Netherlands",
  "Poland",
  "Portugal",
  "UK",
  "Czech Republic",
  "Romania",
  "Sweden"))
map.world_joined <- full_join(world, europ_map, by = c('region' = 'region'))




library(rgeos)
library(geosphere)
library(maptools)
library(countrycode)

data(wrld_simpl)
shape <- wrld_simpl
shape_df <- fortify(shape, region = 'ISO3')
shape_df <- shape_df %>% mutate(continent = countrycode(id, 'iso3c', 'continent')) %>%
  subset(., !is.na(continent))
#capital <- df %>% subset(., capital == 1)

contcolors<-c("Africa" = "grey",
                "Americas" = "grey20",
                "Asia" = "grey",
                "Europe" = "#0065D1", 
                "Oceania" = "#84cff4")

map1<-ggplot() +
  coord_fixed(1.3)+
  geom_polygon(data = shape_df , aes(x = long, y = lat, group = group, fill = continent), color = 'black') +
  #geom_point(data = capital, aes(x = long, y = lat), fill = 'red', color = 'red', size = 0.5)
  scale_fill_manual(values = contcolors)+
  theme(text = element_text(family = "Gill Sans", color = "#000000")
        ,panel.background = element_rect(fill = "#ffffff")
        ,plot.background = element_rect(fill = "#ffffff")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 30)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        #,legend.position = "none"
  )
map1
