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
  group_by(Observation_id, Behavior) %>%
  summarise(median_Total_duration = median(Total_duration_s, na.rm = TRUE),
            Total_number_of_occurences = sum(Total_number_of_occurences, na.rm = TRUE))



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
  group_by(Observation_id, Behavior) %>%
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
  group_by(Observation_id, Behavior) %>%
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
  group_by(Observation_id, Behavior) %>%
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
  group_by(Observation_id, Behavior) %>%
  summarise(median_Total_duration = median(Total_duration_s, na.rm = TRUE))

## combine it all toghether in one dataframe
all_dat <- bind_rows(
  April_4, April_9, Juni_7, Maart_9, Sep_23)

# Ethogram Plot mean duration-----------------------------------------------------------
# 9 april
P1 <- ggplot(data=April_9, aes(x=Behavior, y=Duration_mean_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() +
  ggtitle("9 April") + 
  labs(x = "Behavior", y = "Mean Duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P1

# 4 april
P2 <- ggplot(data=April_4, aes(x=Behavior, y=Duration_mean_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() +
  ggtitle("4 April") + 
  labs(x = "Behavior", y = "Mean Duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P2

# 9 maart
P3 <- ggplot(data=Maart_9, aes(x=Behavior, y=Duration_mean_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() + 
  ggtitle("9 Maart") + 
  labs(x = "Behavior", y = "Mean Duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P3

# 7 juni
P4 <- ggplot(data=Juni_7, aes(x=Behavior, y=Duration_mean_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() + 
  ggtitle("7 Juni") + 
  labs(x = "Behavior", y = "Mean Duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P4

# 23 september
P5 <- ggplot(data=Sep_23, aes(x=Behavior, y=Duration_mean_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() + 
  ggtitle("23 September") + 
  labs(x = "Behavior", y = "Mean Duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P5

## combi plot
P_combi <- (P1 + P2 + P3 + P4 + P5) + 
  plot_layout(ncol = 2, nrow = 3)
P_combi



# Ethogram Plot total duration --------------------------------------------

# 9 april
P6 <- ggplot(data=April_9, aes(x=Behavior, y=Total_duration_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() +
  ggtitle("9 April") + 
  labs(x = "Behavior", y = "Total duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P6

# 4 april
P7 <- ggplot(data=April_4, aes(x=Behavior, y=Total_duration_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() +
  ggtitle("4 April") + 
  labs(x = "Behavior", y = "Total duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P7

# 9 maart
P8 <- ggplot(data=Maart_9, aes(x=Behavior, y=Total_duration_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() + 
  ggtitle("9 Maart") + 
  labs(x = "Behavior", y = "Total duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P8

# 7 juni
P9 <- ggplot(data=Juni_7, aes(x=Behavior, y=Total_duration_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() + 
  ggtitle("7 Juni") + 
  labs(x = "Behavior", y = "Total duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P9

# 23 september
P10 <- ggplot(data=Sep_23, aes(x=Behavior, y=Total_duration_s, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() + 
  ggtitle("23 September") + 
  labs(x = "Behavior", y = "Total duration (s)", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P10

## combi plot
P_combi_2 <- (P6 + P7 + P8 + P9 + P10) + 
  plot_layout(ncol = 2, nrow = 3)
P_combi_2



# Ethogram Plot percentage of time ----------------------------------------
# 9 april
P11 <- ggplot(data=April_9, aes(x=Behavior, y=P_of_total_length, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() +
  ggtitle("9 April") + 
  labs(x = "Behavior", y = "Percentage of total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P11

# 4 april
P12 <- ggplot(data=April_4, aes(x=Behavior, y=P_of_total_length, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() +
  ggtitle("4 April") + 
  labs(x = "Behavior", y = "Percentage of total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P12

# 9 maart
P13 <- ggplot(data=Maart_9, aes(x=Behavior, y=P_of_total_length, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() + 
  ggtitle("9 Maart") +  labs(x = "Behavior", y = "Percentage of total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P13

# 7 juni
P14 <- ggplot(data=Juni_7, aes(x=Behavior, y=P_of_total_length, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() + 
  ggtitle("7 Juni") + 
  labs(x = "Behavior", y = "Percentage of total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P14

# 23 september
P15 <- ggplot(data=Sep_23, aes(x=Behavior, y=P_of_total_length, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_errorbar(aes(ymin=Duration_mean_s - Duration_std_dev, ymax=Duration_mean_s + Duration_std_dev),  position=position_dodge(0.9), width=0.25) + 
  theme_minimal() + 
  ggtitle("23 September") + 
  labs(x = "Behavior", y = "Percentage of total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P15

## combi plot
P_combi_3 <- (P11 + P12 + P13 + P14 + P15) + 
  plot_layout(ncol = 2, nrow = 3)
P_combi_3



P_combi
P_combi_2
P_combi_3



# Plot of percentage of time with error bars ------------------------------
# Grouping the data by Behavior and Observation_id to calculate mean and SD for percentage of time
April_9_summary <- all_dat %>%
  filter(Date == "2023-04-09") %>%  # Make sure to filter by the correct date column
  group_by(Behavior, Observation_id) %>%
  summarize(
    Mean_Percentage_Time = mean(P_of_total_length, na.rm = TRUE), 
    SD_Percentage_Time = sd(P_of_total_length, na.rm = TRUE)
  ) %>%
  ungroup()


# Plotting with error bars
P0 <- ggplot(data = April_9_summary, aes(x = Behavior, y = Mean_Percentage_Time, fill = Observation_id)) + 
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = pmax(0, Mean_Percentage_Time - SD_Percentage_Time), 
                    ymax = pmin(100, Mean_Percentage_Time + SD_Percentage_Time)), 
                position = position_dodge(0.9), width = 0.25) + 
  theme_minimal() +
  ggtitle("Percentage of Total Time per Behavior") +
  labs(x = "Behavior", y = "Mean Percentage of Total Time (%)", fill = "Observation Period") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P0



# total behavior of all dates ---------------------------------------------

all_dat_summary <- all_dat %>%
  group_by(Behavior, Observation_id) %>%
  dplyr::summarize(
    Mean_Percentage_Time = mean(P_of_total_length, na.rm = TRUE), 
    SD_Percentage_Time = sd(P_of_total_length, na.rm = TRUE),
    Total_number_of_occurences = sum(Total_number_of_occurences, na.rm = TRUE)
  ) %>%
  ungroup()



# Plotting with error bars
P100 <- ggplot(data = all_dat_summary, aes(x = Behavior, y = Mean_Percentage_Time, fill = Observation_id)) + 
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = pmax(0, Mean_Percentage_Time - SD_Percentage_Time), 
                    ymax = pmin(100, Mean_Percentage_Time + SD_Percentage_Time)), 
                position = position_dodge(0.9), width = 0.25) + 
  geom_text(aes(label = paste("n =", Total_number_of_occurences), y= 0),
            position = position_dodge(0.9), 
            vjust = -0.5, size = 2.5) +  
  theme_minimal() +
  ggtitle("Total Percentage of Total Time per Behavior") +
  labs(x = "Behavior", y = "Mean Percentage of Total Time (%)", fill = "Observation Period") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylim(0,100)
P100

# median plots of total time --------------------------------------

# 9 april
P20 <- ggplot(data=April_9_median, aes(x=Behavior, y=median_Total_duration, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  #geom_text(aes(label = paste("n =", Total_number_of_occurences), y= 0),
            #position = position_dodge(0.9), 
            #vjust = -0.5, size = 2.5) +
  theme_minimal() +
  ggtitle("9 April") + 
  labs(x = "Behavior", y = "median total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P20


# 4 april
P21 <- ggplot(data=April_4_median, aes(x=Behavior, y=median_Total_duration, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  theme_minimal() +
  ggtitle("4 April") + 
  labs(x = "Behavior", y = "median total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P21

# 9 maart
P22 <- ggplot(data=Maart_9_median, aes(x=Behavior, y=median_Total_duration, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  theme_minimal() +
  ggtitle("9 maart") + 
  labs(x = "Behavior", y = "median total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P22

# 7 juni
P23 <- ggplot(data=Juni_7_median, aes(x=Behavior, y=median_Total_duration, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  theme_minimal() +
  ggtitle("7 juni") + 
  labs(x = "Behavior", y = "median total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P23

# 23 eptember
P24 <- ggplot(data=Sep_23_median, aes(x=Behavior, y=median_Total_duration, fill=Observation_id)) + 
  geom_col(position="dodge") + 
  theme_minimal() +
  ggtitle("23 september") + 
  labs(x = "Behavior", y = "median total time", fill = "Observation Period") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P24

P_combi_4 <- (P20 + P21 + P22 + P23 + P24) + 
  plot_layout(ncol = 2, nrow = 3)
P_combi_4

