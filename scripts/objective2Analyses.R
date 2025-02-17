library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)
library(viridis)
library(tidyverse)

### Wrangle objective 2 data ###
dat <- read_csv("./data/allResults.csv") %>% 
  filter(obj == 2) %>% 
  mutate(net = str_sub(sample, start = -1, end = -1),
         sample = str_sub(sample, start = 1, end = -2)) %>% 
  mutate(direction = str_sub(sample, start = -2, end = -1)) %>% 
  mutate(direction = case_when(direction %in% c("0E", "0N", "0S", "0W") ~ str_sub(sample, start = -1, end = -1),
                               .default = direction)) %>% 
  mutate(direction = case_when(direction %in% c("mE", "mN", "mS", "mW") ~ str_sub(sample, start = -1, end = -1),
                               .default = direction)) %>% 
  mutate(distance = str_sub(sample, start = 1, end = 3)) %>% 
  mutate(distance = case_when(distance %in% c("20W", "20S", "20N", "20E") ~ "20",
                              distance == "1km" ~ "1000",
                              distance == "2km" ~ "2000",
                              .default = distance)) %>% 
  mutate(net = case_when(net == "a" ~ "west",
                              net == "b" ~ "east",
                              net == "c" ~ "central"))

  
### Histogram of eDNA concentrations around nets ###
dat %>% 
  ggplot(aes(x = pg_L)) +
  geom_histogram(binwidth = 5) +
  geom_vline(xintercept = 11.9, ## mean concentrion
             color = "darkred",
             size = 1)+
  geom_vline(xintercept = 32.2, ## 90th percentile concentration
             color = "darkred",
             size = 1,
             linetype = "dashed") +
  xlab("Lake whitefish eDNA concentration (picograms/liter)") +
  ylab("Number of eDNA samples") +
  ggtitle("Objective 2: Trap net eDNA concentrations") +
  theme_bw()
#ggsave("./outputs/obj2ConcentrationRelativeToAmbient.png", width = 6, height = 4)


### Make maps
spatialData <- st_as_sf(dat, coords = c("long", "lat"), crs = 4326)
wisconsin <- read_sf("../Wisconsin_State_Boundary_24K/Wisconsin_State_Boundary_24K.shp")
wisconsin <- st_transform(wisconsin, 4326) # Transform to WGS 84

# WestNet
spatialData.West <- spatialData %>% filter(net == "west")
spatialData.West %>% 
  ggplot() +
  geom_sf(data = wisconsin) +
  geom_sf(aes(color = sqrt(pg_L)), size = 3) +
  scale_color_gradientn(colors = c("blue", "red")) +
  coord_sf(xlim = c(-87.61, -87.55), ylim = c(44.85, 44.89), expand = FALSE) +
  ggtitle("West net") + 
  theme_minimal() +
  theme(panel.grid=element_blank(), axis.ticks.length = unit(0, "pt"))

# CentralNet
spatialData.Central <- spatialData %>% filter(net == "central")
spatialData.Central %>% 
  ggplot() +
  geom_sf(data = wisconsin) +
  geom_sf(aes(color = sqrt(pg_L)), size = 2.5) +
  scale_color_gradientn(colors = c("blue", "red")) +
  coord_sf(xlim = c(-87.54, -87.46), ylim = c(44.87, 44.92), expand = FALSE) +
  facet_wrap(~date) +
  ggtitle("Central net") + 
  theme_minimal() +
  theme(panel.grid=element_blank(), axis.ticks.length = unit(0, "pt"))

# EastNet
spatialData.East <- spatialData %>% filter(net == "east")
spatialData.East %>% 
  ggplot() +
  geom_sf(data = wisconsin) +
  geom_sf(aes(color = pg_L), size = 3) +
  scale_color_gradientn(colors = c("blue", "red")) +
  coord_sf(xlim = c(-87.44, -87.38), ylim = c(44.895, 44.94), expand = FALSE) +
  facet_wrap(~date) +
  ggtitle("East net") + 
  theme_minimal() +
  theme(panel.grid=element_blank(), axis.ticks.length = unit(0, "pt"))




