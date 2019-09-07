df %>% group_by(DAY) %>%
  filter(HOUR == 12 & DAY == 1) %>%
  ggplot(aes(x = ((TEMP/10)-272.15), y= (CALCGPH/3.28084))) +
  geom_point(aes(col=MONTH)) +
  facet_grid(ID ~ .)

library("sf")
library("rnaturalearthdata")
library("ggspatial")

world <- ne_countries(scale = "medium", returnclass = "sf")
currentStatNum <- stations %>% filter(LSTYEAR == 2019) %>% tally()


#sf, rgeos, rnaturalearth, rnaturalearthdata, ggspatial

plot_stations <- st_as_sf(stations %>% filter(LSTYEAR == 2019, ELEVATION != -998.8), coords = c("LONGITUDE","LATITUDE"), crs = 4326, agr = "constant")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = plot_stations, size = 1, shape = 23, fill = "darkred")