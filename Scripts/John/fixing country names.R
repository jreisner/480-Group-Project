library(tidyverse)
library(lubridate)
library(viridis)
library(Hmisc)
land <- read.csv("Global_Landslide_Catalog_Export.csv")

land$date <- mdy_hms(land$event_date)
land$year <- year(land$date)
land$quarter <- as.factor(quarter(land$date))

states <- map_data("state")


land %>%
  filter(country_name == "United States") %>%
  group_by(admin_division_name) %>%
  tally() %>%
  mutate(admin_division_name = tolower(admin_division_name)) %>%
  right_join(states, by = c("admin_division_name" = "region")) %>%
  ggplot(aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = n), colour = "black", size = 0.25) +
  scale_fill_distiller("Landslides", palette = "Spectral", trans = "log10", na.value = "white") +
  theme_void() +
  coord_map() +
  theme(legend.position = "bottom")
ggsave("us map - note where mountains are.pdf")


states %>%
  filter(long > -130 & between(lat, 25, 50)) %>%
  ggplot(aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), size = 0.5) +
  geom_point(aes(x = longitude, y = latitude, colour = landslide_size),
             size = 0.5,
             data = land %>% filter(country_name == "United States") %>%
               filter(longitude > -130 & between(latitude, 25, 50))) +
  coord_map() +
  theme_void()


world <- map_data("world")
wor <- world
w_reg <- world$region %>% unique()
l_reg <- land$country_name %>% unique()
which(!(l_reg %in% w_reg))
l_reg[which(!(l_reg %in% w_reg))]
# 9 country names don't match up

wor <- wor %>%
  mutate(region = ifelse(region == "USA", "United States", region)) %>%
  mutate(
    region = ifelse(region == "Trinidad", "Trinidad and Tobago", region)
  ) %>%
  mutate(
    region = ifelse(region == "Tobago", "Trinidad and Tobago", region)
  ) %>%
  mutate(
    region = ifelse(region == "Myanmar", "Myanmar [Burma]", region)
  ) %>%
  mutate(
    region = ifelse(region == "Grenadines", "Saint Vincent and the Grenadines", 
                    region)
  ) %>%
  mutate(
    region = ifelse(region == "Saint Vincent", "Saint Vincent and the Grenadines", 
                    region)
  ) %>%
  mutate(
    region = ifelse(region == "Virgin Islands", 
                    "U.S. Virgin Islands", region)
  ) %>%
  mutate(
    region = ifelse(region == "Timor-Leste",  "East Timor", region)
  ) %>%
  mutate(
    region = ifelse(region == "Saint Kitts",  "Saint Kitts and Nevis", region)
  ) %>%
  mutate(
    region = ifelse(region == "Nevis",  "Saint Kitts and Nevis", region)
  ) %>%
  mutate(
    region = ifelse(region == "UK",  "United Kingdom", region)
  )

land$country_name <- as.character(land$country_name)
land <- land %>%
  mutate(country_name = ifelse(country_name == "Hong Kong", "China", country_name)) %>%
  mutate(country_name = ifelse(country_name == "Czechia", "Czech Republic", country_name)) %>%
  mutate(country_name = ifelse(country_name == "Republic of the Congo", 
                               "Democratic Republic of the Congo", country_name))

w_reg <- wor$region %>% unique()
l_reg <- land$country_name %>% unique()
which(!(l_reg %in% w_reg))
l_reg[which(!(l_reg %in% w_reg))]
# we're good to go

land %>%
  group_by(country_name) %>%
  tally() %>%
  right_join(wor, by = c("country_name" = "region")) %>%
  ggplot(aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = n), colour = "black", size = 0.25) +
  scale_fill_distiller("Landslides", palette = "Spectral", trans = "log10", na.value = "white") +
  theme_void() +
  coord_map() +
  scale_x_continuous(limits = c(-200, 200)) +
  scale_y_continuous(limits = c(-57.5, NA)) +
  theme(legend.position = "bottom")
ggsave("trial plotv2.pdf")


land %>%
  group_by(country_name) %>%
  summarise(landslides = n(), 
            num_yrs = length(unique(year))) %>%
  mutate(Landslides_Year = landslides / num_yrs) %>%
  right_join(wor, by = c("country_name" = "region")) %>%
  ggplot(aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = Landslides_Year), colour = "black", size = 0.25) +
  scale_fill_distiller("Landslides per Year", palette = "Spectral", trans = "log10", na.value = "white") +
  theme_void() +
  coord_map() +
  scale_x_continuous(limits = c(-200, 200)) +
  scale_y_continuous(limits = c(-55, NA)) +
  theme(legend.position = "bottom")
ggsave("trial plot2v2.pdf")

land %>%
  group_by(country_name, quarter) %>%
  tally() %>%
  ungroup() %>%
  group_by(country_name) %>%
  summarise(most_freq_quart = quarter[which.max(n)]) %>%
  right_join(wor, by = c("country_name" = "region")) %>%
  ggplot(aes(x = long, y = lat)) + 
  geom_hline(yintercept = 0, colour = "grey50", size = 0.25) +
  geom_polygon(aes(group = group, fill = most_freq_quart), 
               colour = "black", size = 0.25) +
  scale_fill_manual("Most Frequent Quarter", 
                    values = c("#d01c8b", "#f1b6da", "#b8e186", "#4dac26", "#f7f7f7"),
                    na.value = "white") +
  theme_void() +
  theme(legend.position = "bottom") +
  coord_map() +
  scale_x_continuous(limits = c(-200, 200)) +
  scale_y_continuous(limits = c(-55, NA))


land %>%
  mutate(half = as.factor(semester(date))) %>%
  group_by(country_name, half) %>%
  tally() %>%
  ungroup() %>%
  group_by(country_name) %>%
  summarise(most_freq_half = half[which.max(n)]) %>%
  right_join(wor, by = c("country_name" = "region")) %>%
  ggplot(aes(x = long, y = lat)) + 
  geom_hline(yintercept = 0, colour = "black", size = 0.25) +
  geom_polygon(aes(group = group, fill = most_freq_half), 
               colour = "black", size = 0.25) +
  scale_fill_manual("Most Frequent Half of Year", 
                    values = c("#d01c8b", "#4dac26", "#f7f7f7"),
                    na.value = "white") +
  theme_void() +
  theme(legend.position = "bottom") +
  coord_map() +
  scale_x_continuous(limits = c(-200, 200)) +
  scale_y_continuous(limits = c(-55, NA))




