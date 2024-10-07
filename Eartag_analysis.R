## loading packages
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(igraph)
library(ggraph)
library(ggmap)
library(sf)
library(ggrepel)

## clean up workspace
rm(list = ls())

## loading databases
Ear_dat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRAMDPMh68__uTtNI4sT0JTg763dWAyKaudVJQPzOYaxLg0EY-nOzd7gSGWW55FKURS2MWxIFQAvn9n/pub?gid=1269775945&single=true&output=csv") %>%
  mutate(Eartag_N = factor(Eartag_N),
         Location = factor(Location),
         Date=lubridate::dmy(Date),
         Time = format(Time, format = "%H:%M:%S")
         )


Ear_dat2 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRAMDPMh68__uTtNI4sT0JTg763dWAyKaudVJQPzOYaxLg0EY-nOzd7gSGWW55FKURS2MWxIFQAvn9n/pub?gid=1269775945&single=true&output=csv") %>%
  dplyr::filter(Location != "Wilgenbosje" ) %>%
  dplyr::filter(Location != "LandbouwEnclave") %>%
  dplyr::mutate(Eartag_N = factor(Eartag_N),
         Location = factor(Location),
         Date = lubridate::dmy(Date),
         Time = format(Time, format = "%H:%M:%S"))


# first plots NO BUENO ----------------------------------------------------

## first attempts of plots, unreadable
p1 <- ggplot(Ear_dat2, aes(x = Location, y = Eartag_N)) +
  geom_jitter(width = 0.0, height = 0) +  # Add jitter to avoid overplotting
  labs(title = "Unique Ear Tag Numbers at Each Location",
       x = "Location",
       y = "Ear Tag Number") +
  scale_x_discrete(expand = c(0.5,0.5)) +   # Adjust the spacing between x-axis categories
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1





# Function to generate HSL colors
generate_same_palette_colors <- function(n) {
  set.seed(42)  # Set seed for reproducibility
  qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual', ]
  col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  sample(col_vector, n)
  }

# Generate 63 distinct colors
  colors <- generate_same_palette_colors(63)
  
p2 <- ggplot(Ear_dat, aes(x = Location, y = Eartag_N, fill = as.factor(Eartag_N))) +
  geom_bar(stat = "identity", position = position_dodge()) +  # Grouped bar plot
  labs(title = "Unique Ear Tag Numbers at Each Location",
       x = "Location",
       y = "Ear Tag Number",
       fill = "Ear Tag Number") +
  scale_fill_manual(values = colors) +  # Apply a custom discrete color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
p2




# chatGPT graph -----------------------------------------------------------

# Create a bipartite graph
edges <- data.frame(from = Ear_dat$Location, to = Ear_dat$Eartag_N)
graph <- graph_from_data_frame(edges, directed = FALSE)

# Assign types for bipartite graph
V(graph)$type <- bipartite_mapping(graph)$type

# Plotting the graph with "Wilgenbosje
p3 <- ggraph(graph, layout = 'fr') + 
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) + 
  geom_node_point(aes(color = V(graph)$type), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Network of Galloway Cattle Ear Tags across Locations",
       color = "Type") +
  scale_color_manual(values = c('blue', 'red'), labels = c('Location', 'Ear Tag'))
p3


#### without wilgenbosje
# Create a bipartite graph
edges <- data.frame(from = Ear_dat2$Location, to = Ear_dat2$Eartag_N)
graph <- graph_from_data_frame(edges, directed = FALSE)

# Assign types for bipartite graph
V(graph)$type <- bipartite_mapping(graph)$type

# Plotting the graph with "Wilgenbosje, LandbouwEnclave" 
p4 <- ggraph(graph, layout = 'fr') + 
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) + 
  geom_node_point(aes(color = V(graph)$type), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Network of Galloway Cattle Ear Tags across Locations",
       color = "Type") +
  scale_color_manual(values = c('blue', 'red'), labels = c('Location', 'Ear Tag'))
p4


