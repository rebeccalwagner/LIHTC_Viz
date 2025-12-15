library(dplyr)
library(ggplot2)
library(gganimate)
library(sf)
library(usmap)

##############################
# import data

data <- read.csv("./data/LIHTCPUB.csv")

##############################
# Filter valid coordinates and years

data <- data %>%
  filter(!is.na(latitude), 
         !is.na(longitude), 
         yr_pis!=8888, 
         yr_pis!=9999,
         !proj_st %in% c("AS", "GU", "MP", "PR", "VI"))

##############################
# transform long/lat data for map projection

data_transformed <- usmap_transform(data,
                                    input_names = c("longitude","latitude"))

coords <- st_coordinates(data_transformed)
data_transformed$x <- coords[, "X"]
data_transformed$y <- coords[, "Y"]

##############################

major_cities <- data.frame(
  city = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", 
           "Philadelphia", "San Antonio", "San Diego", "Dallas", "San Jose",
           "Austin", "Jacksonville", "Fort Worth", "Columbus", "Charlotte",
           "San Francisco", "Indianapolis", "Seattle", "Denver", "Boston",
           "Portland", "Detroit", "Memphis", "Nashville", "Baltimore",
           "Milwaukee", "Albuquerque", "Tucson", "Fresno", "Sacramento",
           "Kansas City", "Atlanta", "Miami", "Tampa", "New Orleans",
           "Cleveland", "Minneapolis", "Omaha", "Raleigh", "Las Vegas"),
  latitude = c(40.7128, 34.0522, 41.8781, 29.7604, 33.4484,
               39.9526, 29.4241, 32.7157, 32.7767, 37.3382,
               30.2672, 30.3322, 32.7555, 39.9612, 35.2271,
               37.7749, 39.7684, 47.6062, 39.7392, 42.3601,
               45.5152, 42.3314, 35.1495, 36.1627, 39.2904,
               43.0389, 35.0844, 32.2226, 36.7378, 38.5816,
               39.0997, 33.7490, 25.7617, 27.9506, 29.9511,
               41.4993, 44.9778, 41.2565, 35.7796, 36.1699),
  longitude = c(-74.0060, -118.2437, -87.6298, -95.3698, -112.0740,
                -75.1652, -98.4936, -117.1611, -96.7970, -121.8863,
                -97.7431, -81.6557, -97.3308, -82.9988, -80.8431,
                -122.4194, -86.1581, -122.3321, -104.9903, -71.0589,
                -122.6784, -83.0458, -90.0490, -86.7816, -76.6122,
                -87.9065, -106.6504, -110.9747, -119.7871, -121.4944,
                -94.5786, -84.3880, -80.1918, -82.4572, -90.0715,
                -81.6944, -93.2650, -95.9345, -78.6382, -115.1398)
)

# Transform city coordinates to match map projection
cities_transformed <- usmap_transform(major_cities,
                                      input_names = c("longitude", "latitude"))
cities_coords <- st_coordinates(cities_transformed)
cities_transformed$x <- cities_coords[, "X"]
cities_transformed$y <- cities_coords[, "Y"]

##############################

# Get the map data
map_data <- us_map()

##############################
# Create animation

map_anim <- ggplot() +
  geom_sf(data = map_data,           
          fill = "white", 
          color = "#143642",
          size = 0.3) +
  geom_point(data = cities_transformed,
             aes(x = x, y = y),
             color = "#0F8B8D",
             size = 4,
             shape = 17) +
  geom_point(data = data_transformed,
             aes(x = x, y = y, group = seq_along(y)), 
             color = "#6D2E46",
             size = 1.5,
             alpha = 0.8) +
  labs(title = 'LIHTC Projects Placed in Service: {frame_time}',
       subtitle = "37 years of affordable housing development across the United States",
       caption = "Source: HUD LIHTC Database (1987-2023)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#143642", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#143642", size = 11),
    plot.caption = element_text(color = "#143642", size = 9, hjust = 0),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  transition_time(yr_pis) +
  shadow_wake(wake_length = 0.1, alpha = TRUE, size = 0.5) +
  enter_fade() +
  exit_fade()

anim <- animate(map_anim, 
        nframes = 150, 
        duration = 20,
        width = 1200, 
        height = 800)

anim_save("./docs/assets/map_anim.gif", animation = anim)