library(tidyverse)

dat <- read_csv("./allResults.csv") %>% 
  filter(obj == 1)

dat %>% 
  filter(pg_L < 1000) %>% 
  ggplot(aes(x = pg_L, y = long)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_bw()

dat %>% 
  filter(pg_L < 1000) %>% 
  ggplot(aes(x = pg_L)) +
  geom_histogram() +
  xlab("Lake whitefish eDNA concentration (picograms/liter)") +
  ylab("Number of eDNA samples") +
  ggtitle("Objective 1: Ambient Green Bay eDNA concentration") +
  theme_bw()
#ggsave("./outputs/obj1ConcentrationHistogram.png", width = 6, height = 4)

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


dat %>% 
  mutate(date = mdy(date)) %>% 
  mutate(month = month(date, label = TRUE)) %>% 
  filter(pg_L < 1000) %>% 
  count(month)


### Mean and variance of ambient concentration ###
## Note: excluding 1 outlier (3000 pg/L)
dat %>%
  filter(pg_L < 1000) %>% 
  summarize(meanConc = mean(pg_L),
            n90percentile = quantile(pg_L, probs = 0.9))



