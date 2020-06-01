#THIS IS A SAMPLE COMMENT
#By Adam Staveski

library(tidyverse)
library(readxl)
weo <- read_excel("data/input/WEO-2018.xlsx")

#Question 3.1 --------------------------------------------------------
weo %>%
  arrange(rgdp2017) %>%
  select(country, rgdp2017)

#Question 3.2 --------------------------------------------------------
weo %>%
  arrange(desc(rgdp2017)) %>%
  select(country, rgdp2017)

#Question 3.3 --------------------------------------------------------
weo %>%
  arrange(continent, desc(rgdp2017)) %>%
  select(country, continent, rgdp2017)

#Question 3.4 --------------------------------------------------------
weo %>%
  arrange(continent, desc(rgdp2017)) %>%
  filter(continent=="Africa") %>%
  select(country, rgdp2017)

#Question 4.1 --------------------------------------------------------
weo_percap <- weo %>%
  mutate(gdp_percap_2017 = rgdp2017/pop2017) %>%
  mutate(gdp_percap_1992 = rgdp1992/pop1992) %>%
  mutate(growth_2017_1992 = (gdp_percap_2017-gdp_percap_1992)/gdp_percap_1992)

#Question 5.1 --------------------------------------------------------
ggplot(data = weo_percap) +
  geom_point(mapping = aes(x = gdp_percap_1992, y = gdp_percap_2017))

#Question 5.2 --------------------------------------------------------
ggplot(data = weo_percap) +
  geom_point(mapping = aes(x = gdp_percap_1992, y = gdp_percap_2017, color=continent))

#Question 5.3 --------------------------------------------------------
ggplot(data = weo_percap) +
  geom_point(mapping = aes(x = gdp_percap_1992, y = gdp_percap_2017, shape=continent), color="navy")

#Question 6.1 --------------------------------------------------------
weo_percap %>%
  summarise(mean_pcgdp_17 = mean(gdp_percap_2017), median_pcgdp_17 = median(gdp_percap_2017))

#Question 6.2 --------------------------------------------------------
for(i in 1:length(weo_percap$gdp_percap_1992)) {
  if(is.na(weo_percap$gdp_percap_1992[i])) {
    print(weo_percap$country[i])
  }
}

for(i in 1:length(weo_percap$gdp_percap_2017)) {
  if(is.na(weo_percap$gdp_percap_2017[i])) {
    print(weo_percap$country[i])
  }
}

#Question 6.3 --------------------------------------------------------
weo_percap %>%
  summarise(mean_pcgdp_92 = mean(gdp_percap_1992, na.rm = TRUE), median_pcgdp_92 = median(gdp_percap_1992, na.rm = TRUE))

#Question 7.1 --------------------------------------------------------
top_three <- weo_percap %>%
  arrange(desc(gdp_percap_2017)) %>%
  select(country, gdp_percap_2017) %>%
  slice(gdp_percap_2017, 1:3)

bot_three <- weo_percap %>%
  arrange(gdp_percap_2017) %>%
  select(country, gdp_percap_2017) %>%
  slice(gdp_percap_2017, 1:3)
