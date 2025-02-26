library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)
library(viridis)
library(tidyverse)

dat <- read_csv("./data/allResults.csv") %>% 
  filter(obj == 1)

### Variation by  latitude? ###
## Note: excluding 1 outlier (3000 pg/L)
dat %>% 
  filter(pg_L < 1000) %>% 
  ggplot(aes(x = pg_L, y = long)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_bw()


### Histogram of ambient eDNA concentrations ###
## Note: excluding 1 outlier (3000 pg/L)
dat %>% 
  filter(pg_L < 1000) %>% 
  ggplot(aes(x = pg_L)) +
  geom_histogram() +
  xlab("Lake whitefish eDNA concentration (picograms/liter)") +
  ylab("Number of eDNA samples") +
  ggtitle("Objective 1: Ambient Green Bay eDNA concentration") +
  theme_bw()
#ggsave("./outputs/obj1ConcentrationHistogram.png", width = 6, height = 4)


### Ambient eDNA concentrations by month ###
## Note: excluding 1 outlier (3000 pg/L)
dat %>% 
  mutate(date = mdy(date)) %>% 
  mutate(month = month(date, label = TRUE)) %>% 
  filter(pg_L < 1000) %>% 
  ggplot(aes(x = month, y = pg_L, group = month)) +
  geom_boxplot() +
  ylab("Lake whitefish eDNA concentration (picograms/liter)") +
  xlab("Month") +
  ggtitle("Objective 1: Ambient Green Bay eDNA concentration") +
  theme_bw()
ggsave("./outputs/obj1ConcentrationByMonth.png", width = 6, height = 4)


### Mean and variance of ambient concentration ###
## Note: excluding 1 outlier (3000 pg/L)
dat %>%
  filter(pg_L < 1000) %>% 
  summarize(meanConc = mean(pg_L),
            n90percentile = quantile(pg_L, probs = 0.9))


#### Map making ####
spatialData <- st_as_sf(dat, coords = c("long", "lat"), crs = 4326)
wisconsin <- read_sf("../Wisconsin_State_Boundary_24K/Wisconsin_State_Boundary_24K.shp")
wisconsin <- st_transform(wisconsin, 4326) # Transform to WGS 84

spatialData %>% 
  ggplot() +
  geom_sf(data = wisconsin) +
  geom_sf(aes(color = log10(pg_L+1)), size = 3) +
  scale_color_gradientn(colors = c("blue", "red")) +# Apply the gradient
  coord_sf(xlim = c(-88.1, -87.3), ylim = c(44.5, 45.2), expand = FALSE) +
  annotation_scale(location = "tl", bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "br") +
  theme_minimal() +
  theme(panel.grid=element_blank(), axis.ticks.length = unit(0, "pt"))
