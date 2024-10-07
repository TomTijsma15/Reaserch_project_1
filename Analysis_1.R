
# loading libraries -------------------------------------------------------
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(lubridate)
library(igraph)
library(ggraph)
library(ggmap)
library(sf)
library(ggrepel)
library(htmlwidgets)

## set wroking directory
setwd("~/Desktop/Research_project_1/Analysis")

## clearing workspace
rm(list = ls())




# databases ---------------------------------------------------------------

#koeienrustplaats
KR <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=874617147&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Kapvlakte_II
K2 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=0&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Kapvlakte_I
K1 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=14694863&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Landbouwenclave
LBE <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=447384624&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#wilgenbosje
WB <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=1892290163&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Verjongingsplek 
VJ <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=359353175&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Rustplaats_2
R2 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=855755444&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Hereveldje
HV <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=267291890&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Drinkplaats
DP <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=281469642&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Stierenkuil
SK <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=1653327107&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Drinkplaats_2
DP2 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=1070015457&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Rustplaats_3
R3 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=124710809&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Dode_boom
DB <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=769361918&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Kapvlakte_3
K3 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=1908906115&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Hek_laarweg
HEK <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=142699725&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Picknickbank
Pick <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=1191089659&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#Fietspad
FP <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRY2ImBL4fkK_TjjX-oG-c6QfJa7nZNrxwaOWCwkRDvbhCQc2Unz4ZYHWFbU_Q_BkK9Y4P_L524F6dx/pub?gid=287156853&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S")
  )

#total dataframe
Hart_data <- bind_rows(DB, DP, DP2, FP, HEK, HV, K1, K2, K3, KR, LBE, Pick, R2, R3, SK, VJ, WB)



# Eartag Analysis NO MAP ---------------------------------------------------------

Eartag_Dat <- Hart_data %>%
  filter(Location !='Wilgenbosje') %>%
  filter(Location !='Landbouwenclave') %>%
  filter(!is.na(Eartag_N))

# Create a bipartite graph
edges <- data.frame(from = Eartag_Dat$Location, to = Eartag_Dat$Eartag_N)
graph <- graph_from_data_frame(edges, directed = FALSE)

# Assign types for bipartite graph
V(graph)$type <- bipartite_mapping(graph)$type

# Plotting the graph with "Wilgenbosje
p3 <- ggraph(graph, layout = 'fr') + 
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) + 
  geom_node_point(aes(color = V(graph)$type), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Network of Galloway  Ear Tags",
       color = "Type") +
  scale_color_manual(values = c('blue', 'red'), labels = c('Location', 'Ear Tag'))
p3


# map of camera locations -------------------------------------------------

map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = Hart_data,
    ~Lon, ~Lat,
    popup = ~Location,
    radius = 5,  # Adjust the size of the circle
    color = "blue",  # Customize the color
    fillOpacity = 0.5  # Adjust the fill opacity
  )
map


# Eartag Analysis WITH MAP --------------------------------------------------

# Prepare spatial data for mapping
locations <- Eartag_Dat %>%
  select(Location, Lat, Lon) %>%
  distinct()

# Assuming ear tags have the same location coordinates, add slight offset
ear_tags <- Eartag_Dat %>%
  distinct(Eartag_N, .keep_all = TRUE) %>%
  mutate(Lat = Lat + runif(n(), -0.002, 0.002),  # Slight random offset for visibility
         Lon = Lon + runif(n(), -0.002, 0.002)) %>%
  select(Eartag_N, Lat, Lon) %>%
  distinct()

# Create an sf object for locations and ear tags
locations_sf <- st_as_sf(locations, coords = c("Lon", "Lat"), crs = 4326)
ear_tags_sf <- st_as_sf(ear_tags, coords = c("Lon", "Lat"), crs = 4326)

# Combine the graph with the map
map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = locations_sf,
    label = ~Location,
    color = "blue",
    radius = 6,
    fillOpacity = 0.5
  ) %>%
  addCircleMarkers(
    data = ear_tags_sf,
    label = ~Eartag_N,
    color = "red",
    radius = 6,
    fillOpacity = 0.5
  )

# Add edges as polylines on the map
for (i in 1:nrow(edges)) {
  from <- edges$from[i]
  to <- edges$to[i]
  from_coords <- locations %>% filter(Location == from) %>% select(Lon, Lat)
  to_coords <- ear_tags %>% filter(Eartag_N == to) %>% select(Lon, Lat)
  map <- map %>%
    addPolylines(
      lng = c(from_coords$Lon, to_coords$Lon),
      lat = c(from_coords$Lat, to_coords$Lat),
      color = "gray",
      weight = 1,
      opacity = 0.5
    )
}

# Print the map
map

# Eartag analysis by month WITH MAP ------------------------------------------------

# Function to filter data by months and create map
create_map <- function(data, month_range) {
  filtered_data <- data %>%
    filter(month(Date) %in% month_range)
  
  # Remove duplicates for ear tags and create sf object
  ear_tags_sf <- filtered_data %>%
    distinct(Eartag_N, .keep_all = TRUE) %>%
    mutate(Lat = Lat + runif(n(), -0.002, 0.002),  # Slight random offset for visibility
           Lon = Lon + runif(n(), -0.002, 0.002)) %>%
    select(Eartag_N, Lat, Lon) %>%
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  
  # Create sf object for locations
  locations_sf <- filtered_data %>%
    select(Location, Lat, Lon) %>%
    distinct() %>%
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  
  # Create the map
  map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      data = locations_sf,
      label = ~Location,
      color = "blue",
      radius = 6,
      fillOpacity = 0.5
    ) %>%
    addCircleMarkers(
      data = ear_tags_sf,
      label = ~Eartag_N,
      color = "red",
      radius = 6,
      fillOpacity = 0.5
    )
  
  # Add edges as polylines on the map
  for (i in 1:nrow(filtered_data)) {
    from <- filtered_data$Location[i]
    to <- filtered_data$Eartag_N[i]
    from_coords <- filtered_data %>% filter(Location == from) %>% select(Lon, Lat)
    to_coords <- filtered_data %>% filter(Eartag_N == to) %>% select(Lon, Lat) %>% distinct()
    map <- map %>%
      addPolylines(
        lng = c(from_coords$Lon, to_coords$Lon),
        lat = c(from_coords$Lat, to_coords$Lat),
        color = "gray",
        weight = 1,
        opacity = 0.5
      )
  }
  
  return(map)
}

# Define the month ranges for each period
winter_months <- c(12, 1, 2)
spring_months <- c(3, 4, 5)
summer_months <- c(6, 7, 8)
fall_months <- c(9, 10, 11)

# Create maps for each period
winter_map <- create_map(Eartag_Dat, winter_months)
spring_map <- create_map(Eartag_Dat, spring_months)
summer_map <- create_map(Eartag_Dat, summer_months)
fall_map <- create_map(Eartag_Dat, fall_months)

# Display the maps
winter_map
spring_map
summer_map
fall_map

# Tabel of averages per month per location-------------------------------------------------------

# Extract month and year from date
data <- Hart_data %>%
  mutate(month = format(Date, "%Y-%m"))

# Filter data for Galloway cattle
galloway_data <- data %>%
  filter(Animal == "Galloway")

#create new dataframe for plotting
summary_data <- galloway_data %>%
  group_by(Location) %>%
  summarise(
    mean_galloway = mean(N_Animals, na.rm = TRUE),
    sd_galloway = sd(N_Animals, na.rm = TRUE),
    mean_bulls = mean(N_Bulls, na.rm = TRUE),
    sd_bulls = sd(N_Bulls, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    lower_galloway = mean_galloway - sd_galloway,
    upper_galloway = mean_galloway + sd_galloway,
    lower_bulls = mean_bulls - sd_bulls,
    upper_bulls = mean_bulls + sd_bulls
  )

# Create a long format dataframe for plotting
plot_data <- summary_data %>%
  pivot_longer(
    cols = c(mean_galloway, mean_bulls),
    names_to = "animal_type",
    values_to = "mean_count"
  ) %>%
  mutate(
    animal_type = ifelse(animal_type == "mean_galloway", "Galloway", "Bulls"),
    lower = ifelse(animal_type == "Galloway", lower_galloway, lower_bulls),
    upper = ifelse(animal_type == "Galloway", upper_galloway, upper_bulls)
  )

# Plot the data with error bars
ggplot(plot_data, aes(x = reorder(Location, mean_count), y = mean_count, fill = animal_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(0.7), width = 0.25) +
  labs(
    title = "Average Count of Galloway Cattle and Bulls per Location",
    x = "Location",
    y = "Average Count",
    fill = "Animal Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Combine mean and SD of galloway and bulls into a single string
summary_data <- summary_data %>%
  mutate(
    summary = paste0("Galloway: ", mean_galloway, " ± ", sd_galloway, ", Bulls: ", mean_bulls, " ± ", sd_bulls)
  ) %>%
  select(Location, month, summary)

# Sort data by month
summary_data <- summary_data %>%
  arrange(month)

# Reshape data to wide format
summary_wide <- summary_data %>%
  pivot_wider(
    names_from = Location,
    values_from = summary,
    names_sep = "_"
  )

# Create a separate dataframe for the summary table
Gal_mean_dat <- as.data.frame(summary_wide)

# Print the summary table
print(summary_wide)

# Save the summary table to a CSV file
write.csv(summary_wide, "summary_galloway_cattle.csv", row.names = FALSE)


