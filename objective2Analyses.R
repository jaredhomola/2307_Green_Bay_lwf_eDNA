library(sf)
library(ggspatial)
library(tidyverse)

dat <- read_csv("./allResults.csv") %>% 
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
                              .default = distance))
  
# A tibble: 1 × 2
# meanConc  n90percentile
# 11.9      32.2  

dat %>% 
  ggplot(aes(x = pg_L)) +
  geom_histogram(binwidth = 5) +
  geom_vline(xintercept = 11.9,
             color = "darkred",
             size = 1)+
  geom_vline(xintercept = 32.2,
             color = "darkred",
             size = 1,
             linetype = "dashed") +
  xlab("Lake whitefish eDNA concentration (picograms/liter)") +
  ylab("Number of eDNA samples") +
  ggtitle("Objective 2: Trap net eDNA concentrations") +
  theme_bw()
#ggsave("./outputs/obj2ConcentrationRelativeToAmbient.png", width = 6, height = 4)


#### Map making ####
spatialData <- st_as_sf(dat, coords = c("long", "lat"), crs = 4326)
  
  
  
 