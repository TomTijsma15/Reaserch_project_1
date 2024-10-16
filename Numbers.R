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
library(Hmisc)
library(multcomp)
library(multcompView)

## set wroking directory
#setwd("~/Desktop/Research_project_1/Analysis")

## clearing workspace
rm(list = ls())




# databases ---------------------------------------------------------------

Bef_af <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSYAQWAQaV-ihNGmewHNrIMyAD4qG2P_7M1p0gCClkyLp9ro-OeV7C2y_JkkDuh6N22mrMeohMSQFgf/pub?gid=0&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Event = factor(Event, levels=c("Before", "During", "After")),
                Location = factor(Location),
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S"),
                N_Bulls = ifelse(is.na(N_Bulls), 0, N_Bulls)
  ) %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))

# filter data for only Koeienrustplaats
bef_af1 <- Bef_af %>%
  dplyr::filter(Location == "Koeienrustplaats")

# Calculate sample sizes for each Event
sample_sizes <- bef_af1 %>%
  group_by(Event) %>%
  summarise(N = n())

# mean number of animals --------------------------------------------------


# boxplot number of animals
P1 <- ggplot(bef_af1, aes(x = Event, y = N_Animals)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.5) +
  labs(title = "Number of Galloway in frame/video for different wolf events",
       x = " Wolf event",
       y = " Maximum number of Galloway in frame/video",
       fill = "Location") +  # Changed color to fill for legend
  theme_minimal() +                 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P1

# nuber of animals analysis
m1 <- glm(N_Animals ~ Event, family = 'poisson', data = bef_af1)
summary(m1)
posthoc <- glht(m1, linfct = mcp(Event = "Tukey"))
summary(posthoc)
# Extract pairwise comparison p-values
posthoc_summary <- summary(posthoc)$test$pvalues
# Create names in the format "Before-During", "Before-After", "During-After"
names(posthoc_summary) <- c("Before-During", "Before-After", "During-After")
# Get the significance letters
letters <- multcompLetters(posthoc_summary)
signif_letters <- letters$Letters
# Create a data frame to match Event levels with significance letters
signif_data <- data.frame(
  Event = c("Before", "During", "After"),
  Letters = signif_letters,
  y_position = c(13, 13, 13)
)

P1.1 <- P1 + 
  geom_text(data = signif_data, aes(x = Event, y = y_position, label = Letters), 
            vjust = -0.5, color = "red", size = 5)
P1.1

# Add sample sizes (N) to the plot
P1.2 <- P1.1 +
  geom_text(data = sample_sizes, aes(x = Event, y = 0, label = paste("n =", N)), 
            vjust = -0.5, color = "black", size = 3)
P1.2
