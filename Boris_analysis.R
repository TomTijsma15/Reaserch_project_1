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
library(patchwork)
library(FSA)
library(multcompView)
library(rcompanion)

## set wroking directory
#setwd("~/Users/tom/Documents/Masters/_Github/Research_project_1")

## clearing workspace
rm(list = ls())



# loading database --------------------------------------------------------
## 9 april
April_9 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRn-t03PCU_LCd1BL_feEim7-sVrR_DzDLDTKvCXwtLoIrJ3Qn1Mc_kblAjOQj9fU21F92azfRf9Gmy/pub?output=csv") %>%
  dplyr::mutate(Moment = factor(Moment),
                Observation_id = factor(Observation_id),
                Behavior = factor(Behavior),
                Date=lubridate::dmy(Date),
                Total_duration_s = if_else(Moment == "B", Total_duration_s / 21, Total_duration_s),
                Total_duration_s = if_else(Moment == "A", Total_duration_s / 21, Total_duration_s),
                Duration_mean_s = if_else(Moment == "B", Duration_mean_s / 21, Duration_mean_s),
                Duration_mean_s = if_else(Moment == "A", Duration_mean_s / 21, Duration_mean_s),
                #Total_duration_s = ceiling(Total_duration_s),
                ) %>%
  dplyr::select(-Description, -Subject, -Modifiers, - Time_budget_start, -Time_budget_stop, -Time_budget_duration) %>%
  mutate(Observation_id = recode(Observation_id, 
                                 "9-4-23_Na" = "After", 
                                 "9-4-23_Tijdens" = "During", 
                                 "9-4-23_Voor" = "Before"),
         Total_duration_s = if_else(is.na(Total_duration_s), 10, Total_duration_s),
         Duration_mean_s = if_else(is.na(Duration_mean_s), 10, Duration_mean_s),
         P_of_total_length = if_else(is.na(P_of_total_length), 10, P_of_total_length)
         ) %>%
  mutate(Observation_id = factor(Observation_id, levels = c("Before", "During", "After")))
## create dataframe containing the median responses
April_9_median <- April_9 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_Total_duration = median(Total_duration_s, na.rm = TRUE),
            #Total_number_of_occurences = sum(Total_number_of_occurences, na.rm = TRUE)
            )



## 9 maart
Maart_9 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRn-t03PCU_LCd1BL_feEim7-sVrR_DzDLDTKvCXwtLoIrJ3Qn1Mc_kblAjOQj9fU21F92azfRf9Gmy/pub?gid=1033223028&single=true&output=csv") %>%
  dplyr::mutate(Moment = factor(Moment),
                Observation_id = factor(Observation_id),
                Behavior = factor(Behavior),
                Date=lubridate::dmy(Date),
                Total_duration_s = if_else(Moment == "B", Total_duration_s / 15, Total_duration_s),
                Total_duration_s = if_else(Moment == "A", Total_duration_s / 27, Total_duration_s),
                Duration_mean_s = if_else(Moment == "B", Duration_mean_s / 15, Duration_mean_s),
                Duration_mean_s = if_else(Moment == "A", Duration_mean_s / 27, Duration_mean_s),
                Total_duration_s = if_else(Moment == "D", Total_duration_s / 5, Total_duration_s),
                Duration_mean_s = if_else(Moment == "D", Duration_mean_s / 5, Duration_mean_s)
  ) %>%
  dplyr::select(-Description, -Subject, -Modifiers, - Time_budget_start, -Time_budget_stop, -Time_budget_duration) %>%
  mutate(Observation_id = recode(Observation_id, 
                                 "9-3-24_Na" = "After", 
                                 "9-3-24_Tijdens" = "During", 
                                 "9-3-24_Voor" = "Before"),
         Total_duration_s = if_else(is.na(Total_duration_s), 10, Total_duration_s),
         Duration_mean_s = if_else(is.na(Duration_mean_s), 10, Duration_mean_s),
         P_of_total_length = if_else(is.na(P_of_total_length), 10, P_of_total_length)) %>%
  mutate(Observation_id = factor(Observation_id, levels = c("Before", "During", "After")))
## create dataframe containing the median responses
Maart_9_median <- Maart_9 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_Total_duration = median(Total_duration_s, na.rm = TRUE))
            

## 4 april
April_4 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRn-t03PCU_LCd1BL_feEim7-sVrR_DzDLDTKvCXwtLoIrJ3Qn1Mc_kblAjOQj9fU21F92azfRf9Gmy/pub?gid=1476679565&single=true&output=csv") %>% 
  dplyr::mutate(Moment = factor(Moment),
                Observation_id = factor(Observation_id),
                Behavior = factor(Behavior),
                Date=lubridate::dmy(Date),
                Total_duration_s = if_else(Moment == "B", Total_duration_s / 10, Total_duration_s),
                Total_duration_s = if_else(Moment == "A", Total_duration_s / 21, Total_duration_s),
                Duration_mean_s = if_else(Moment == "B", Duration_mean_s / 10, Duration_mean_s),
                Duration_mean_s = if_else(Moment == "A", Duration_mean_s / 21, Duration_mean_s),
                Total_duration_s = if_else(Moment == "D", Total_duration_s / 4, Total_duration_s),
                Duration_mean_s = if_else(Moment == "D", Duration_mean_s / 4, Duration_mean_s)
  ) %>%
  dplyr::select(-Description, -Subject, -Modifiers, - Time_budget_start, -Time_budget_stop, -Time_budget_duration) %>%
  mutate(Observation_id = recode(Observation_id, 
                                 "4-4-23_Na" = "After", 
                                 "4-4-23_Tijdens" = "During", 
                                 "4-4-23_Voor" = "Before"),
         Total_duration_s = if_else(is.na(Total_duration_s), 10, Total_duration_s),
         Duration_mean_s = if_else(is.na(Duration_mean_s), 10, Duration_mean_s),
         P_of_total_length = if_else(is.na(P_of_total_length), 10, P_of_total_length)) %>%
  mutate(Observation_id = factor(Observation_id, levels = c("Before", "During", "After")))
## create dataframe containing the median responses
April_4_median <- April_4 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_Total_duration = median(Total_duration_s, na.rm = TRUE))


## 7 juni
Juni_7 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRn-t03PCU_LCd1BL_feEim7-sVrR_DzDLDTKvCXwtLoIrJ3Qn1Mc_kblAjOQj9fU21F92azfRf9Gmy/pub?gid=2002311890&single=true&output=csv") %>%
  dplyr::mutate(Moment = factor(Moment),
                Observation_id = factor(Observation_id),
                Behavior = factor(Behavior),
                Date=lubridate::dmy(Date),
                Total_duration_s = if_else(Moment == "B", Total_duration_s / 21, Total_duration_s),
                Total_duration_s = if_else(Moment == "A", Total_duration_s / 5, Total_duration_s),
                Duration_mean_s = if_else(Moment == "B", Duration_mean_s / 21, Duration_mean_s),
                Duration_mean_s = if_else(Moment == "A", Duration_mean_s / 5, Duration_mean_s),
                Total_duration_s = if_else(Moment == "D", Total_duration_s / 1, Total_duration_s),
                Duration_mean_s = if_else(Moment == "D", Duration_mean_s / 1, Duration_mean_s)
  ) %>%
  dplyr::select(-Description, -Subject, -Modifiers, - Time_budget_start, -Time_budget_stop, -Time_budget_duration) %>%
  mutate(Observation_id = recode(Observation_id, 
                                 "7-6-23_Na" = "After", 
                                 "7-6-23_Tijdens" = "During", 
                                 "7-6-23_Voor" = "Before"),
         Total_duration_s = if_else(is.na(Total_duration_s), 10, Total_duration_s),
         Duration_mean_s = if_else(is.na(Duration_mean_s), 10, Duration_mean_s),
         P_of_total_length = if_else(is.na(P_of_total_length), 10, P_of_total_length)) %>%
  mutate(Observation_id = factor(Observation_id, levels = c("Before", "During", "After")))
## create dataframe containing the median responses
Juni_7_median <- Juni_7 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_Total_duration = median(Total_duration_s, na.rm = TRUE))


## 23 september
Sep_23 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRn-t03PCU_LCd1BL_feEim7-sVrR_DzDLDTKvCXwtLoIrJ3Qn1Mc_kblAjOQj9fU21F92azfRf9Gmy/pub?gid=2104007755&single=true&output=csv") %>%
  dplyr::mutate(Moment = factor(Moment),
                Observation_id = factor(Observation_id),
                Behavior = factor(Behavior),
                Date=lubridate::dmy(Date),
                Total_duration_s = if_else(Moment == "B", Total_duration_s / 7, Total_duration_s),
                Total_duration_s = if_else(Moment == "A", Total_duration_s / 9, Total_duration_s),
                Duration_mean_s = if_else(Moment == "B", Duration_mean_s / 7, Duration_mean_s),
                Duration_mean_s = if_else(Moment == "A", Duration_mean_s / 9, Duration_mean_s),
                Total_duration_s = if_else(Moment == "D", Total_duration_s / 1, Total_duration_s),
                Duration_mean_s = if_else(Moment == "D", Duration_mean_s / 1, Duration_mean_s)
  ) %>%
  dplyr::select(-Description, -Subject, -Modifiers, - Time_budget_start, -Time_budget_stop, -Time_budget_duration) %>%
  mutate(Observation_id = recode(Observation_id, 
                                 "23-9-23_Na" = "After", 
                                 "23-9-23_Tijdens" = "During", 
                                 "23-9-23_Voor" = "Before"),
         Total_duration_s = if_else(is.na(Total_duration_s), 10, Total_duration_s),
         Duration_mean_s = if_else(is.na(Duration_mean_s), 10, Duration_mean_s),
         P_of_total_length = if_else(is.na(P_of_total_length), 10, P_of_total_length)) %>%
  mutate(Observation_id = factor(Observation_id, levels = c("Before", "During", "After")))
## create dataframe containing the median responses
Sep_23_median <- Sep_23 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_Total_duration = median(Total_duration_s, na.rm = TRUE))

## combine it all toghether in one da## combine it all together in one da## combine it all toghether in one dataframe
all_dat <- bind_rows(
  April_4, April_9, Juni_7, Maart_9, Sep_23, new_rows)

## combine the median into one dataframe
all_dat_median <- bind_rows(
  April_4_median, April_9_median, Juni_7_median, Maart_9_median, Sep_23_median)

## add missing values in the dataframe
new_rows <- data.frame(
  Observation_id = c("Before", "During", "Before", "During", "Before", "After", "During", "During", "Before", "After", "Before", "During", "After", "Before", "After", "During", "After", "Before", "After", "Before", "During", "During", "After", "Before", "During", "Before", "After", "During", "Before", "After", "Before", "After", "Before", "During", "During", "Before", "During", "During", "After", "Before", "During", "Before", "During", "During", "After", "Before", "During", "Before", "After", "During", "After"),
  Behavior = c("chasing females", "chasing females", "grazing", "grazing", "Hoof scraping", "Hoof scraping", "laying", "ruminating", "running", "running", "walking", "grazing", "grazing", "grouping", "grouping", "headbutting", "headbutting", "Hoof scraping", "Hoof scraping", "laying", "laying", "other", "other", "ruminating", "ruminating", "running", "running", "standing", "vocalizing", "vocalizing", "Hoof scraping", "Hoof scraping", "laying", "laying", "other", "ruminating", "ruminating", "grazing", "grazing", "running", "running", "scanning", "scanning", "standing", "standing", "vocalizing", "walking", "scanning", "scanning", "standing", "standing"),
  Date = as.Date(c("2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-09-23", "2023-09-23", "2023-09-23", "2023-09-23")),
  median_Total_duration = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Setting the duration to 0 for the missing events
  )

all_dat_median1 <- rbind(all_dat_median, new_rows) %>%
  mutate(Observation_id = factor(Observation_id, levels = c("Before", "During", "After")),
         Behavior = factor(Behavior)
  )



# Plots -------------------------------------------------------------------
P1 <- ggplot(data=all_dat, aes(x=Behavior, y=Total_duration_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  theme_minimal() +
  ggtitle("total time of  behaviors") + 
  labs(x = "Behavior", y = "Total time (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
facet_wrap(~ Date, scales = "free_x")  # Facet by Date, with separate scales for x-axis
P1


# total behavior of all dates ---------------------------------------------
#plot
P01 <- ggplot(data=all_dat_median1, aes(x=Behavior, y=median_Total_duration, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  theme_minimal() +
  ggtitle("Median time of  behaviors") + 
  labs(x = "Behavior", y = "median total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Date, scales = "free_x")  # Facet by Date, with separate scales for x-axis
P01

# non-parametric test for median duration of behavior ---------------------
# kruskal wallis test (non parametric ANOVA)
kruskal.test(median_Total_duration ~ Observation_id, data = all_dat_median1)

# post hoc test, Dunns test
dunnTest(median_Total_duration ~ Observation_id, data = all_dat_median1, method = "bonferroni")

## schreider ray hare test for behavior and event cause it is two way
scheirerRayHare(median_Total_duration ~ Observation_id + Behavior + Date, data = all_dat_median1)
# post hoc, dunns test EVENT
dunnTest(median_Total_duration ~ Observation_id, data = all_dat_median1, method = "bonferroni")
# post hoc, dunns test BEHAVIOR
dunnTest(median_Total_duration ~ Behavior, data = all_dat_median1, method = "bonferroni")





