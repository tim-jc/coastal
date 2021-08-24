# libraries
library(extrafont)
library(tidyverse)
library(leaflet)
library(mapdata)
library(magrittr)
library(cowplot)
library(lubridate)

loadfonts()

# clear the memory
rm(list=ls(all = TRUE))

# source functions
source("coastal_vis_functions.R")

# Notes / resources -------------------------------------------------------

# Print in A2 (420mm x 594mm), but to fit 40cm x 50cm frame. IKEA Lomviken

# gradients - https://uigradients.com/
# colour palette - https://flatuicolors.com/palette/gb
# ideas https://themassifcentral.co.uk/pages/bespoke

# Get data ----------------------------------------------------------------

load_gps_data(file_location = "gps_data/csv_files/")

rides_index <- create_summary()

uk_outline_map <-  map_data(map = "worldHires", region = c("UK", "Isle of Man", "Isle of Wight", "Wales:Anglesey"), xlim=c(-11,3), ylim=c(49.9,58.5))

rev_geo <- read_csv("reverse_geocoding.csv")

towns <- read_csv("uk_towns_by_population.csv")

postcode_elevation <- read_csv("open_postcode_elevation.csv")

# Processing --------------------------------------------------------------

# Order rides
ride_levels <-  c("washford_bristol","tintagel_washford","penzance_tintagel","penzance_looe","looe_exmouth","exmouth_bournemouth",
                  "folkestone_bognor", "london_folkestone", "maldon_battlesbridge", "maldon_clacton", "clacton_manningtree",
                  "woodbridge_manningtree", "orford_woodbridge", "snape_orford","southwold_snape", "hunstanton_southwold",
                  "boston_hunstaton", "boston_hull", "hull_staithes", "staithes_newcastle")

# Add reverse geocoding and elevation data
full_dataset <- full_dataset %>%
  left_join(rev_geo, by = c("lon", "lat")) %>% 
  mutate(postcode = str_extract(location_string, "[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}"),
         town = str_remove(location_string, ", [A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}.*") %>% str_extract("([^,]+$)") %>% str_trim(),
         ride = factor(ride, levels = ride_levels, ordered = T)) %>% 
  left_join(postcode_elevation, by = "postcode") %>% 
  left_join(towns, by = "town") %>% 
  arrange(ride) %>% 
  mutate(id = seq(1,nrow(.),1))

# Write out list of lat / longs that need reverse geocoding
full_dataset %>% 
  filter(is.na(postcode)) %>% 
  select(lon, lat) %>% 
  write_csv("locations_to_map.csv")
  
# 10 most populous towns on the route
top_10_towns <- full_dataset %>% 
  select(town, Population, town_lat, town_lon) %>% 
  unique() %>% 
  arrange(desc(Population)) %>% 
  head(10) %>% 
  arrange(desc(town_lat)) %>% 
  mutate(top_10_town = TRUE) %>% 
  select(town, top_10_town)

# Add back to main dataset and flag start / finish towns
full_dataset <- full_dataset %>%
  left_join(top_10_towns, by = "town") %>% 
  mutate(is_bookend = if_else(id == 1 | id == nrow(.), T, F),
         elevation_plot_lbl = if_else(is_bookend, town, NA_character_))

# View route in leaflet ---------------------------------------------------

leaflet() %>% 
  addTiles() %>% 
  add_track(full_dataset)

# Plot --------------------------------------------------------------------

map_track <- ggplot() + 
  geom_polygon(data = uk_outline_map, aes(x = long, y = lat, group = group), fill = "#192a56") +
  geom_point(data = full_dataset, aes(x = lon, y = lat),  colour = "#ffffff", size = 0.1, alpha = 0.9) +
  coord_map() +
  scale_colour_gradient2(low = "#2980B9", mid = "#6DD5FA", high = "#ffffff") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#273c75"),
        plot.background = element_rect(fill = "#273c75", colour = NA))

elevation_profile <- full_dataset %>% 
  ggplot(aes(x = id, y = -ele)) +
  ggformula::geom_spline(colour = "#FFFFFF", spar = c(.2)) +
  geom_text(aes(label = elevation_plot_lbl), angle = 0, y = -100, colour = "#FFFFFF", family = "Avenir", fontface = "italic", size = 5) +
  coord_flip() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA))
elevation_profile

summary_data <- rides_index %>% 
  mutate(yr = as.character(yr)) %>% 
  group_by(yr) %>% 
  summarise(total_miles = sum(distance_miles) %>% as.integer(),
            total_climb = sum(elevation_metres) %>% as.integer()) %>% 
  bind_rows(data.frame(yr = "",
                       total_miles = NA_real_,
                       total_climb = NA_real_))

max_miles <- max(summary_data$total_miles, na.rm = T) %>% as.integer()
max_miles_lbl <- str_c(max_miles, "\nmiles")
max_climb <- max(summary_data$total_climb, na.rm = T) %>% as.integer()
max_climb_lbl <- str_c(max_climb, "\nmeters")

n_yrs <- nrow(summary_data)

lbl_df <- summary_data %>% 
  filter(yr == "") %>% 
  mutate(total_miles = max_miles,
         miles_lbl = max_miles_lbl,
         total_climb = max_climb,
         climb_lbl = max_climb_lbl)

miles <- summary_data %>% 
  ggplot(aes(x = yr, y = total_miles)) +
  geom_segment(x = 2, xend = n_yrs, y = max_miles, yend = max_miles, linetype = "dashed", colour = "#FFFFFF") +
  geom_text(label = utf8::utf8_print("\u2699"), size=10, colour = "#FFFFFF", family = "Apple Symbols") +
  geom_text(data = lbl_df, aes(x= yr, y = total_miles, label = miles_lbl), colour = "#FFFFFF", fontface = "italic", family = "Avenir", size = 6, hjust = 1) +
  coord_flip() +
  expand_limits(y = c(50,max_miles * 1.1)) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#FFFFFF", size = 20, family = "Avenir"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#273c75"),
        plot.background = element_rect(fill = "#273c75", colour = NA),
        plot.margin = margin(t = 0, l = 0, b = 2, r = 2, "cm"))

climb <- summary_data %>% 
  ggplot(aes(x = yr, y = total_climb)) +
  geom_segment(x = 2, xend = n_yrs, y = max_climb, yend = max_climb, linetype = "dashed", colour = "#FFFFFF") +
  geom_text(label = utf8::utf8_print("\u2699"), size=10, colour = "#FFFFFF", family = "Apple Symbols") +
  geom_text(data = lbl_df, aes(x= yr, y = total_climb, label = climb_lbl), colour = "#FFFFFF", fontface = "italic", family = "Avenir", size = 6, hjust = 1) +
  coord_flip() +
  expand_limits(y = c(50,max_climb * 1.1)) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#FFFFFF", size = 20, family = "Avenir"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#273c75"),
        plot.background = element_rect(fill = "#273c75", colour = NA),
        plot.margin = margin(t = 0, l = 0, b = 2, r = 2, "cm"))


background <- ggplot() +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "#273c75", colour = NA))


# Annotations -------------------------------------------------------------

title_string <- "COASTING"
sub_title_string <- str_glue("{min(rides_index$start_datetime) %>% as_date() %>% format('%B %Y')} to {max(rides_index$start_datetime) %>% as_date() %>% format('%B %Y')}
                             {sum(summary_data$total_miles, na.rm = T) %>% as.integer()} miles ridden
                             {sum(summary_data$total_climb, na.rm = T) %>% as.integer()} metres climbed") 

# Assemble visualisation --------------------------------------------------

# Sizes of print and frame. x_val and y_val to draw a rectangle
# showing the aperture of the picture frame so all contents can fit inside

a2_width <- 420
a2_height <- 594

ikea_frame_width <- 400
ikea_frame_height <- 500

x_val <- (1 - (ikea_frame_width / a2_width))/2
y_val <- (1 - (ikea_frame_height / a2_height))/2

gg_out <- ggdraw(background) +
  draw_plot(map_track, width = 0.8, height = 0.8, x = 0.1, y = 0.07) +
  draw_plot(miles, x = x_val+0.05, y = 0.1, width = 0.275, height = 0.2) +
  draw_plot(climb, x = x_val+0.65, y = 0.6, width = 0.275, height = 0.2) +
  draw_text(title_string, x = x_val+0.01, y = (1-y_val) * 0.97, size = 80, family = "Avenir", colour = "#FFFFFF", fontface = "bold", hjust = 0) +
  draw_line(
    x = c(x_val, 1-x_val, 1-x_val, x_val, x_val),
    y = c(y_val, y_val, 1-y_val, 1-y_val, y_val),
    color = "white", size = 1
  ) +
  draw_text(sub_title_string, x = x_val+0.01, y = (1-y_val) * 0.915, size = 20, family = "Avenir", colour = "#FFFFFF", hjust = 0)


ggsave(filename = "coastal_vis.png", device = "png", width = a2_width, height = a2_height, units = "mm",  dpi = 300)

