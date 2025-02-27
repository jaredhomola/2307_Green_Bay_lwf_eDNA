library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)
library(patchwork)
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

  
# ### Modeling data for Zach
# harvest <- read_csv("data/harvest_records.csv")
# dat %>% 
#   mutate(distance = as.double(distance)) %>% 
#   mutate(distanceEW = case_when(direction %in% c("N", "S") ~ 0,
#                                 direction == "W" ~ distance*-1,
#                                 direction == "E" ~ distance,
#                                 direction %in% c("NE", "SE") ~ distance*cos(45*pi/180),
#                                 direction %in% c("NW", "SW") ~ (distance*cos(45*pi/180))*-1)) %>% 
#   mutate(distanceNS = case_when(direction %in% c("E", "W") ~ 0,
#                                 direction == "S" ~ distance*-1,
#                                 direction == "N" ~ distance,
#                                 direction %in% c("NE", "NW") ~ distance*cos(45*pi/180),
#                                 direction %in% c("SW", "SE") ~ (distance*cos(45*pi/180))*-1)) %>% 
#   select(date, pg_L, net, distanceEW, distanceNS) %>% 
#   left_join(harvest, join_by(net, date == eDNA_sampling_date)) %>% 
#   select(-Nearest_net_lift_date) %>% 
#   drop_na(Catch_lbs) %>% 
#   write.csv("./data/modelingData.csv")

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


### Make maps ###
spatialData <- st_as_sf(dat, coords = c("long", "lat"), crs = 4326)
wisconsin <- read_sf("../Wisconsin_State_Boundary_24K/Wisconsin_State_Boundary_24K.shp")
wisconsin <- st_transform(wisconsin, 4326) # Transform to WGS 84

# WestNet
spatialData.West <- spatialData %>% filter(net == "west")
west.plot <- spatialData.West %>% 
  ggplot() +
  geom_sf(data = wisconsin) +
  geom_sf(aes(color = log10(pg_L+1)), size = 3) +
  scale_color_gradientn(colors = c("blue", "red")) +
  coord_sf(xlim = c(-87.61, -87.55), ylim = c(44.85, 44.89), expand = FALSE) +
  ggtitle("West net") + 
  theme_minimal() +
  theme(panel.grid=element_blank(), axis.ticks.length = unit(0, "pt")) + 
  labs(color = expression(Log[10](pg/L)))

# CentralNet
spatialData.Central <- spatialData %>% filter(net == "central")
central.plot <- spatialData.Central %>% 
  ggplot() +
  geom_sf(data = wisconsin) +
  geom_sf(aes(color = log10(pg_L+1)), size = 2.5) +
  scale_color_gradientn(colors = c("blue", "red")) +
  coord_sf(xlim = c(-87.54, -87.46), ylim = c(44.87, 44.92), expand = FALSE) +
  facet_wrap(~date, ncol = 2) +
  ggtitle("Central net") + 
  theme_minimal() +
  theme(panel.grid=element_blank(), axis.ticks.length = unit(0, "pt")) + 
  labs(color = expression(Log[10](pg/L)))

# EastNet
spatialData.East <- spatialData %>% filter(net == "east")
east.plot <- spatialData.East %>% 
  ggplot() +
  geom_sf(data = wisconsin) +
  geom_sf(aes(color = log10(pg_L+1)), size = 3) +
  scale_color_gradientn(colors = c("blue", "red")) +
  coord_sf(xlim = c(-87.44, -87.395), ylim = c(44.895, 44.94), expand = FALSE) +
  facet_wrap(~date) +
  ggtitle("East net") + 
  theme_minimal() +
  theme(panel.grid=element_blank(), axis.ticks.length = unit(0, "pt")) + 
  labs(color = expression(Log[10](pg/L)))


central.plot | (west.plot / east.plot)
