
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
library(igraph)
library(leafsync)

## set wroking directory
#setwd("~/Desktop/Research_project_1/Analysis")

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
  #filter(Location !='Wilgenbosje') %>%
  #filter(Location !='Landbouwenclave') %>%
  filter(!is.na(Eartag_N))

# Create a bipartite graph
edges <- data.frame(from = Eartag_Dat$Location, to = Eartag_Dat$Eartag_N)
graph <- graph_from_data_frame(edges, directed = FALSE)

# Assign types for bipartite graph
V(graph)$type <- bipartite_mapping(graph)$type

# Plotting the graph with "Wilgenbosje
p1 <- ggraph(graph, layout = 'fr') + 
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) + 
  geom_node_point(aes(color = V(graph)$type), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Network of Galloway  Ear Tags",
       color = "Type") +
  scale_color_manual(values = c('blue', 'red'), labels = c('Location', 'Ear Tag'))
p1


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
## with distinct lines

# Prepare spatial data for mapping (locations without offsets)
locations <- Eartag_Dat %>%
  dplyr::select(Location, Lat, Lon) %>%
  distinct()

# Apply random offsets to the ear tag locations for visibility
ear_tags_offset <- Eartag_Dat %>%
  distinct(Eartag_N, Location, .keep_all = TRUE) %>%
  dplyr::mutate(Lat = Lat + runif(n(), -0.0011, 0.0011),  # Slight random offset for visibility
                Lon = Lon + runif(n(), -0.0011, 0.0011)) %>%
  dplyr::select(Eartag_N, Lat, Lon) %>%
  distinct()

# Create an sf object for locations (without offsets) and ear tags (with offsets)
locations_sf <- st_as_sf(locations, coords = c("Lon", "Lat"), crs = 4326)
ear_tags_sf <- st_as_sf(ear_tags_offset, coords = c("Lon", "Lat"), crs = 4326)

# Create edges dataframe that will store pairs of locations visited by the same ear tag
edges <- Eartag_Dat %>%
  dplyr::group_by(Eartag_N) %>%
  dplyr::arrange(Eartag_N, Location) %>%
  dplyr::mutate(Next_Location = lead(Location),
                Next_Lat = lead(Lat),
                Next_Lon = lead(Lon)) %>%
  dplyr::filter(!is.na(Next_Location)) %>%
  dplyr::select(Eartag_N, Location, Lat, Lon, Next_Location, Next_Lat, Next_Lon)

# Create the leaflet map
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

# Add polylines using real (non-offset) coordinates to show movements between locations
for (i in 1:nrow(edges)) {
  from_coords <- c(edges$Lon[i], edges$Lat[i])  # Actual coordinates of first location
  to_coords <- c(edges$Next_Lon[i], edges$Next_Lat[i])  # Actual coordinates of second location
  
  map <- map %>%
    addPolylines(
      lng = c(from_coords[1], to_coords[1]),
      lat = c(from_coords[2], to_coords[2]),
      color = "gray",
      weight = 2,
      opacity = 0.7
    )
  
  
}

# Add the legend to the map
map <- map %>%
  addLegend("bottomright",
            colors = c("blue", "red", "gray"),
            labels = c("Locations", "Ear Tags", "Connections"),
            title = "Legend",
            opacity = 0.7)
# Print the map
map



# Connectivity test -------------------------------------------------------
# Create edge list: pairs of locations visited by the same individual (ear tags)
edges <- Eartag_Dat %>%
  dplyr::group_by(Eartag_N) %>%
  dplyr::arrange(Eartag_N, Location) %>%
  dplyr::mutate(Next_Location = lead(Location)) %>%
  dplyr::filter(!is.na(Next_Location)) %>%
  dplyr::ungroup() %>%   # Ungroup to remove the Eartag_N grouping
  dplyr::select(Location, Next_Location) %>%
  distinct()

# Convert to a matrix with two columns
edge_list <- as.matrix(edges)

# Create the graph object using the edge list
g <- graph_from_edgelist(edge_list, directed = FALSE)

# Plot the graph to visualize the connectivity
plot(g, vertex.label = V(g)$name, vertex.color = "lightblue", edge.color = "gray")

is_connected(g)
# Returns TRUE if all locations are connected, FALSE if there are disconnected components.
components <- components(g)
# Print the membership of each component (which locations belong to which group)
components$membership
degree(g)
# Shows the number of connections each location has.
#shortest_paths(g, from = "Koeienrustplaats", to = "Kapvlakte_II")
transitivity(g, type = "global") # Gives the overall clustering coefficient of the graph.
transitivity(g, type = "local") # Gives the overall clustering coefficient of the graph.
transitivity(g, type = "average") # Gives the overall clustering coefficient of the graph.



## get the number of eartags per location
# Summarize unique ear tags per location
location_eartag_counts <- Eartag_Dat %>%
  group_by(Location) %>%
  summarise(Unique_Eartag_Count = n_distinct(Eartag_N)) %>%
  ungroup()


# Plot the number of unique ear tags per location
P4 <- ggplot(data= location_eartag_counts, aes(x=Location, y=Unique_Eartag_Count)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title = "Number of unique ear tag observations per Location",
       x = "Location",
       y = "Number of unique Tags") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

P4


# unique ear tags per location --------------------------------------------
# Step 1: Calculate unique ear tags count
total_unique_ear_tags <- Eartag_Dat %>%
  dplyr::summarize(total_unique_ear_tags = n_distinct(Eartag_N)) %>%
  dplyr::pull(total_unique_ear_tags)

# Step 2: Summarize ear tag locations
ear_tag_locations <- Eartag_Dat %>%
  dplyr::group_by(Eartag_N) %>%
  dplyr::summarize(
    n_locations = n_distinct(Location),
    locations = paste(unique(Location), collapse = ", ")
  )

# Step 3: Separate tags found in only one location and in more than one
tags_one_location <- ear_tag_locations %>%
  dplyr::filter(n_locations == 1)

tags_multiple_locations <- ear_tag_locations %>%
  dplyr::filter(n_locations > 1)

# Print the total count of unique ear tags
total_unique_ear_tags

# Display results
print("Tags found in only one location:")
print(tags_one_location)

print("Tags found in multiple locations:")
print(tags_multiple_locations)

# Combine the results for plotting
plot_data <- ear_tag_locations %>%
  dplyr::mutate(tag_category = ifelse(n_locations == 1, "One Location", "Multiple Locations"))

# Plot
ggplot(plot_data, aes(x = reorder(Eartag_N, -n_locations), y = n_locations, fill = tag_category)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flip coordinates for better readability
  labs(
    title = "Number of Locations for Each Eartag",
    x = "Eartag Number",
    y = "Number of Locations",
    fill = "Tag Category"
  ) +
  theme_minimal()



# Eartag analysis by month WITH MAP ------------------------------------------------

# Function to filter data by months and create map
create_map <- function(data, month_range) {
  # Filter data by the specified month range
  filtered_data <- data %>%
    dplyr::filter(month(Date) %in% month_range)
  
  # Remove duplicates for ear tags and create sf object with slight offsets
  ear_tags_sf <- filtered_data %>%
    distinct(Eartag_N, Location, .keep_all = TRUE) %>%
    dplyr::mutate(Lat = Lat + runif(n(), -0.0011, 0.0011),  # Slight random offset for visibility
                  Lon = Lon + runif(n(), -0.0011, 0.0011)) %>%
    dplyr::select(Eartag_N, Lat, Lon) %>%
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  
  # Create sf object for locations without offsets
  locations_sf <- filtered_data %>%
    dplyr::select(Location, Lat, Lon) %>%
    distinct() %>%
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  
  # Create edges dataframe for the filtered data (pairs of locations visited by the same ear tag)
  edges <- filtered_data %>%
    dplyr::group_by(Eartag_N) %>%
    dplyr::arrange(Eartag_N, Location) %>%
    dplyr::mutate(Next_Location = lead(Location),
                  Next_Lat = lead(Lat),
                  Next_Lon = lead(Lon)) %>%
    dplyr::filter(!is.na(Next_Location)) %>%
    dplyr::select(Eartag_N, Location, Lat, Lon, Next_Location, Next_Lat, Next_Lon)
  
  # Create the leaflet map
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
  
  # Add polylines using real (non-offset) coordinates to show movements between locations
  for (i in 1:nrow(edges)) {
    from_coords <- c(edges$Lon[i], edges$Lat[i])  # Actual coordinates of first location
    to_coords <- c(edges$Next_Lon[i], edges$Next_Lat[i])  # Actual coordinates of second location
    
    map <- map %>%
      addPolylines(
        lng = c(from_coords[1], to_coords[1]),
        lat = c(from_coords[2], to_coords[2]),
        color = "gray",
        weight = 2,
        opacity = 0.7
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
# all maps togheter
sync(winter_map, spring_map, summer_map, fall_map)


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




# histogram of roe deer ---------------------------------------------------
# filter to remove Galloway and rename "other" to "unkown" 
Roe_data <- Hart_data %>%
  filter(Animal != "Galloway") %>%
  mutate(Animal = as.character(Animal),
         Animal = replace(Animal, Animal == "Other", "unknown"),
         Animal = as.factor(Animal)) %>%
  mutate(Proportion_Roe_Deer = sum(Animal == "Roe deer") / n())



P100 <- ggplot(data=Roe_data, aes(x=Animal)) +
  geom_bar(fill="blue") +
  labs(title = "Number of observations per Animal",
       x = "Animal",
       y = "Number of observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P100
