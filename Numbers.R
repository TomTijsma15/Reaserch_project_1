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

## set wroking directory
setwd("~/Desktop/Research_project_1/Analysis")

## clearing workspace
rm(list = ls())




# databases ---------------------------------------------------------------

Bef_af <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSYAQWAQaV-ihNGmewHNrIMyAD4qG2P_7M1p0gCClkyLp9ro-OeV7C2y_JkkDuh6N22mrMeohMSQFgf/pub?gid=0&single=true&output=csv") %>%
  dplyr::mutate(Animal=factor(Animal), 
                Event = factor(Event, levels=c("Before", "After")),
                Location = factor(Location),
                Film_Mode=factor(Film_Mode),
                Eartag_N=factor(Eartag_N),
                Date=lubridate::dmy(Date),
                Time = format(Time, format = "%H:%M:%S"),
                N_Bulls = ifelse(is.na(N_Bulls), 0, N_Bulls)
  ) %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))




# mean number of animals --------------------------------------------------

# Calculate mean and standard deviation
summary_stats <- Bef_af %>%
  group_by(Event, Location) %>%
  dplyr::summarize(mean_N_Animals = mean(N_Animals, na.rm = TRUE),
            sd_N_Animals = sd(N_Animals, na.rm = TRUE),
            .groups = 'drop') 

# boxplot number of animals
P1 <- ggplot(Bef_af, aes(x = Event, y = N_Animals, fill = Location)) +
  geom_violin() +
  labs(title = "number of animals per frame",
       x = "Event",
       y = "Number of Galloway",
       fill = "Location") +  # Changed color to fill for legend
  theme_minimal() +                 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylim(0,13)

P1

m1 <- glm(N_Animals ~ Event, family="poisson", data=Bef_af)
summary(m1)

m2 <- lm(mean_N_Animals ~ Event, data=summary_stats)
summary(m2)



# mean number of bulls ----------------------------------------------------

P2 <- ggplot(Bef_af, aes(x = Event, y = N_Bulls, fill = Location)) +
  geom_violin() + 
  labs(title = "number of Bulls per video",
       x = "Event",
       y = "Number of Bulls",
       fill = "Location") +  # Changed color to fill for legend
  theme_minimal() +                 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

P2

P3 <- ggplot(Bef_af, aes(x = Event, y = N_Bulls, fill = Location)) +
  stat_summary(fun = "mean", geom = "col", position = "dodge") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean Number of Bulls",
       x = "Event",
       y = "Mean Number of Bulls",
       fill = "Location") +
  theme_minimal() +                 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

P3
