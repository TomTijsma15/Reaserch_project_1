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
## create dataframe containing the median percentage of time
April_9_median_P <- April_9 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_P = median(P_of_total_length, na.rm = TRUE),
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
## create dataframe containing the median percentage of time
Maart_9_median_P <- Maart_9 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_P = median(P_of_total_length, na.rm = TRUE),
  )
            

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
## create dataframe containing the median percentage of time
April_4_median_P <- April_4 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_P = median(P_of_total_length, na.rm = TRUE),
  )


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
## create dataframe containing the median percentage of time
Juni_7_median_P <- Juni_7 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_P = median(P_of_total_length, na.rm = TRUE),
  )


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
## create dataframe containing the median percentage of time
Sep_23_median_P <- Sep_23 %>%
  group_by(Observation_id, Behavior, Date) %>%
  summarise(median_P = median(P_of_total_length, na.rm = TRUE),
  )

## combine it all toghether in one da## combine it all together in one da## combine it all toghether in one dataframe
all_dat <- bind_rows(
  April_4, April_9, Juni_7, Maart_9, Sep_23)

## combine the median into one dataframe
all_dat_median <- bind_rows(
  April_4_median, April_9_median, Juni_7_median, Maart_9_median, Sep_23_median)

## combine the median percentage of time into one dataframe
all_dat_median_P <- bind_rows(
  April_4_median_P, April_9_median_P, Juni_7_median_P, Maart_9_median_P, Sep_23_median_P)

## add missing values in the dataframe medians
new_rows <- data.frame(
  Observation_id = c("Before", "During", "Before", "During", "Before", "After", "During", "During", "Before", "After", "Before", "During", "After", "Before", "After", "During", "After", "Before", "After", "Before", "During", "During", "After", "Before", "During", "Before", "After", "During", "Before", "After", "Before", "After", "Before", "During", "During", "Before", "During", "During", "After", "Before", "During", "Before", "During", "During", "After", "Before", "During", "Before", "After", "During", "After"),
  Behavior = c("chasing females", "chasing females", "grazing", "grazing", "Hoof scraping", "Hoof scraping", "laying", "ruminating", "running", "running", "walking", "grazing", "grazing", "grouping", "grouping", "headbutting", "headbutting", "Hoof scraping", "Hoof scraping", "laying", "laying", "other", "other", "ruminating", "ruminating", "running", "running", "standing", "vocalizing", "vocalizing", "Hoof scraping", "Hoof scraping", "laying", "laying", "other", "ruminating", "ruminating", "grazing", "grazing", "running", "running", "scanning", "scanning", "standing", "standing", "vocalizing", "walking", "scanning", "scanning", "standing", "standing"),
  Date = as.Date(c("2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-09-23", "2023-09-23", "2023-09-23", "2023-09-23")),
  median_Total_duration = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Setting the duration to 0 for the missing events
  )
## add missing rows dataframe Percentage of time
new_rows_P <- data.frame(
  Observation_id = c("Before", "During", "Before", "During", "Before", "After", "During", "During", "Before", "After", "Before", "During", "After", "Before", "After", "During", "After", "Before", "After", "Before", "During", "During", "After", "Before", "During", "Before", "After", "During", "Before", "After", "Before", "After", "Before", "During", "During", "Before", "During", "During", "After", "Before", "During", "Before", "During", "During", "After", "Before", "During", "Before", "After", "During", "After"),
  Behavior = c("chasing females", "chasing females", "grazing", "grazing", "Hoof scraping", "Hoof scraping", "laying", "ruminating", "running", "running", "walking", "grazing", "grazing", "grouping", "grouping", "headbutting", "headbutting", "Hoof scraping", "Hoof scraping", "laying", "laying", "other", "other", "ruminating", "ruminating", "running", "running", "standing", "vocalizing", "vocalizing", "Hoof scraping", "Hoof scraping", "laying", "laying", "other", "ruminating", "ruminating", "grazing", "grazing", "running", "running", "scanning", "scanning", "standing", "standing", "vocalizing", "walking", "scanning", "scanning", "standing", "standing"),
  Date = as.Date(c("2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-09", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2023-04-04", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2024-03-09", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-06-07", "2023-09-23", "2023-09-23", "2023-09-23", "2023-09-23")),
  median_P = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # Setting the duration to 0 for the missing events
)

## add new rows with 0's to the median dataframe
all_dat_median1 <- rbind(all_dat_median, new_rows) %>%
  mutate(Observation_id = factor(Observation_id, levels = c("Before", "During", "After")),
         Behavior = factor(Behavior)
  )%>%
  dplyr::filter(Behavior != "other")

## add the new rows with 0's to the percentage of time dataframe
all_dat_median_P1 <- rbind(all_dat_median_P, new_rows_P) %>%
  mutate(Observation_id = factor(Observation_id, levels = c("Before", "During", "After")),
         Behavior = factor(Behavior)
  ) %>%
  dplyr::filter(Behavior != "other")



# Create the new dataframe by summarizing behaviors median percantage of time
all_dat_median2 <- all_dat_median1 %>%
  dplyr::mutate(Behavior_group = case_when(
    Behavior %in% c("Hoof scraping", "headbutting", "chasing females") ~ "agitated",
    Behavior %in% c("scanning", "vocalizing", "grouping") ~ "vigilant",
    Behavior %in% c("laying", "ruminating", "grazing", "standing") ~ "non-vigilant",
    TRUE ~ Behavior  # walking, running, and other remain the same
  )) %>%
  dplyr::group_by(Observation_id, Date, Behavior_group) %>%
  dplyr::summarize(median_Total_duration = sum(median_Total_duration, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Behavior_group = factor(Behavior_group, levels = c("vigilant", "non-vigilant", "agitated", "walking", "running", "other"))) %>%
  tidyr::complete(Observation_id, Date, Behavior_group, fill = list(median_Total_duration = 0)) %>%
  dplyr::filter(Behavior_group != "other")
  


# Create the new dataframe by summarizing behaviors median percantage of time
all_dat_median_P2 <- all_dat_median_P1 %>%
  dplyr::mutate(Behavior_group = case_when(
    Behavior %in% c("Hoof scraping", "headbutting", "chasing females") ~ "agitated",
    Behavior %in% c("scanning", "vocalizing", "grouping") ~ "vigilant",
    Behavior %in% c("laying", "ruminating", "grazing", "standing") ~ "non-vigilant",
    TRUE ~ Behavior  # walking, running, and other remain the same
  )) %>%
  dplyr::group_by(Observation_id, Date, Behavior_group) %>%
  dplyr::summarize(median_P = sum(median_P, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Behavior_group = factor(Behavior_group, levels = c("vigilant", "non-vigilant", "agitated", "walking", "running", "Other")))


# all data plot -------------------------------------------------------------------
P1 <- ggplot(data=all_dat, aes(x=Behavior, y=Total_duration_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  theme_minimal() +
  ggtitle("total time of  behaviors") + 
  labs(x = "Behavior", y = "Total time (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
facet_wrap(~ Date, scales = "free_x")  # Facet by Date, with separate scales for x-axis
P1


# median plots  ---------------------------------------------
# median behavior total time grouped
P2 <- ggplot(data=all_dat_median2, aes(x=Behavior_group, y=median_Total_duration, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  theme_minimal() +
  ggtitle("Time spend per behavior, grouped") + 
  labs(x = "Behavior", y = "Median total time (sec)", fill = "Observation period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_grid(rows = vars(Date), switch = "both", scales = "free_x") +  # Use facet_grid for better axis label control
  geom_vline(xintercept = seq(1.5, length(unique(all_dat_median2$Behavior_group)) - 0.5, by = 1), 
             linetype = "dotted", color = "black", linewidth = 0.5) + 
  theme(axis.ticks.x = element_line(linewidth = 0.8),   # Add thicker x-axis ticks
        axis.ticks.length.x = unit(0.25, "cm"),
        strip.placement = "outside") +  # Place facet labels outside the plot
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) + # Add space on the sides of x-axis
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                    labels = c("Before (-20 videos)", "During", "After (+20 videos)"))  # Colorblind-friendly palette


P2
# median behavior total time ungrouped

P2.5 <- ggplot(data=all_dat_median1, aes(x=Behavior, y=median_Total_duration, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  theme_minimal() +
  ggtitle("Time spend per behavior, ungrouped") + 
  labs(x = "Behavior", y = "Median total time (sec)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_grid(rows = vars(Date), switch = "both", scales = "free_x") +  # Use facet_grid for better axis label control
  geom_vline(xintercept = seq(1.5, length(unique(all_dat_median1$Behavior)) - 0.5, by = 1), 
             linetype = "dotted", color = "black", linewidth = 0.5) + 
  theme(axis.ticks.x = element_line(linewidth = 0.8),   # Add thicker x-axis ticks
        axis.ticks.length.x = unit(0.25, "cm"),
        strip.placement = "outside") +  # Place facet labels outside the plot
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +  # Add space on the sides of x-axis
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                    labels = c("Before (-20 videos)", "During", "After (+20 videos)"))  # Colorblind-friendly palette

P2.5

# non-parametric test for median duration of behavior ---------------------
# kruskal wallis test (non parametric ANOVA)
kruskal.test(median_Total_duration ~ Observation_id, data = all_dat_median1)

# post hoc test, Dunns test
dunnTest(median_Total_duration ~ Observation_id, data = all_dat_median2, method = "bonferroni")


## use only vigilant and non-vigilant for better comparison
vigilant_nonvigilant_df <- all_dat_median2 %>%
  dplyr::filter(Behavior_group %in% c("vigilant", "non-vigilant"))

kruskal.test(median_Total_duration ~ Observation_id, data = vigilant_nonvigilant_df)


