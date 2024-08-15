library(lubridate)
library(tidyverse)
library(ggthemes)

morphs_full <- read_csv("data/Morphs_US_Crocker_RoDBLS8.csv", 
                        na = c("Pt.Reyes", "NA"))

#Trim to include only metadata, straight length, ax girth, and actual mass
morphs_trim <- morphs_full %>% 
  select(1:7, standard_length, actual_mass, ax_girth)

#Creating a df where each seal has one row for deployment AND recovery (if applicable)

#First create a df in which I filter out seals that were deployed AND recovered (RoD2022-n = 22, RoD2023-n = 21 * 2 for each season [deployment + recovery] = 86 rows)
morphs_recd <- morphs_trim %>%
  group_by(topp_id) %>%
  filter(n() > 1)

# Pivot wider
morphs_pivot <- morphs_recd %>%
  pivot_wider(names_from = tagging_procedure, values_from = 
                c(date, standard_length, ax_girth, actual_mass))

#Let's create a new df that has information for animals that were NOT recovered, (RoD2022-n = 10, RoD2023-n = 11 = 21 rows) & format it to match the pivoted df 
morphs_NOTrecd <- morphs_trim %>%
  group_by(topp_id) %>%
  filter(n() == 1) %>% 
  #delete the procedure type column as that was "used up" when pivoting 
  select(-tagging_procedure) %>% 
  #move the date column to match the pivoted df
  relocate(date, .before = standard_length) %>% 
  #rename the date, lengths, and mass colums to reflect deployment measurements
  rename(date_Deployment = date, 
         standard_length_Deployment = standard_length,
         ax_girth_Deployment = ax_girth,
         actual_mass_Deployment = actual_mass) %>% 
  #create empty date, lengths, and mass columns and populate them with NAs to match     the structure of the pivoted df
  add_column(date_Recovery = NA, .after = "date_Deployment") %>% 
  add_column(standard_length_Recovery = NA, .after = "standard_length_Deployment") %>%
  add_column(ax_girth_Recovery = NA, .after = "ax_girth_Deployment") %>%
  add_column(actual_mass_Recovery = NA, .after = "actual_mass_Deployment") %>% 
  relocate(ax_girth_Deployment, .after = standard_length_Recovery) %>%
  relocate(ax_girth_Recovery, .before = actual_mass_Deployment)
  

# Now combined the recovered and unrecovered seals dfs + make dates into date objects
morphs_combined <- bind_rows(morphs_pivot, morphs_NOTrecd) %>% 
  arrange(topp_id) %>% 
  mutate(date_Deployment = dmy(date_Deployment), 
         date_Recovery = dmy(date_Recovery))

# Now let's calculate some changes in body size thruout the deployment duration
morphs_change <- morphs_combined %>% 
  mutate(deployment_Dur = date_Recovery - date_Deployment, 
         change_SL = standard_length_Recovery - standard_length_Deployment, 
         change_AxGirth = ax_girth_Recovery - ax_girth_Deployment, 
         change_Mass = actual_mass_Recovery - actual_mass_Deployment) %>% 
  #Move these columns next to their original counterparts to more easily read
  relocate(deployment_Dur, .after = date_Recovery) %>% 
  relocate(change_SL, .after = standard_length_Recovery) %>%
  relocate(change_AxGirth, .after = ax_girth_Recovery) %>% 
  relocate(change_Mass, .after = actual_mass_Recovery)

#save as an RDS to retain date-time formatting
saveRDS(morphs_change, "data/outputs/morphs_change.RDS")

#Now let's bring in the startstop spreadsheet to calculate actual trip duration and join with the morphs data
startstop <- read_csv("data/startstop.csv")

#Trim out some redundant/irrelevant columns to make it less of a nightmare to read
startstop_t <- startstop %>% 
  select(TOPPID, 
         DepartDate, DepartLat, DepartLon, 
         ArriveDate, ArriveLat, ArriveLon, ArriveLoc,
         LastDateObs) %>% 
  #Renaming date fields and calling them DateTime fields to better reflect the data they contain
  rename(DepartDateTime = DepartDate) %>% 
  rename(ArriveDateTime = ArriveDate) %>% 
  rename(LastObsDateTime = LastDateObs) %>% 
  #Separating dates and times into their own columns
  separate(DepartDateTime, c("DepartDate","DepartTime"),sep = " ", remove = FALSE) %>% 
  separate(ArriveDateTime, c("ArriveDate", "ArriveTime"), sep = " ", remove = FALSE) %>% 
  separate(LastObsDateTime, c("LastObsDate", "LastObsTime"), sep = " ", remove = FALSE) %>% 
  #Remove DateTime and Time cols as they are not needed for this analysis
  select(-matches("Time")) %>% 
  #Convert dates into date objects %>% 
  mutate(DepartDate = mdy(DepartDate), 
         ArriveDate = mdy(ArriveDate), 
         LastObsDate = mdy(LastObsDate))

#Now let's calculate actual trip durations based on the departure and arrival dates
trip_Dur <- startstop_t %>% 
  mutate(trip_Dur = case_when(
    !is.na(ArriveDate) ~ ArriveDate - DepartDate,
    !is.na(LastObsDate) ~ LastObsDate - DepartDate))

#save as an RDS to retain date-time formatting
saveRDS(trip_Dur, "data/outputs/trip_Dur.RDS")

#Now let's join the morphs change and trip duration dfs 
trip_Summary <- left_join(trip_Dur, morphs_change, by= c("TOPPID" = "topp_id")) %>% 
  relocate(animalid:date_Deployment, .after = TOPPID) %>% 
  relocate(date_Recovery, .after = ArriveDate) %>% 
  #Lets calculate how long each animal was on the beach after deployment and before recovery
  mutate(dep_Diff = DepartDate - date_Deployment, 
         rec_Diff = date_Recovery - ArriveDate, 
         trip_Diff = deployment_Dur - trip_Dur) %>% 
  relocate(dep_Diff, .after = DepartDate) %>% 
  relocate(rec_Diff, .after = date_Recovery) %>% 
  relocate(trip_Diff, .after = deployment_Dur)

#save as an RDS to retain date-time formatting
saveRDS(trip_Summary, "data/outputs/trip_Summary.RDS")
#Also saving as a .csv for logistics + planning
write_csv(trip_Summary, "data/outputs/trip_Summary.csv")


#let's plot some body size
BodySize_Dep <- ggplot(trip_Summary, aes(x = actual_mass_Deployment, 
                                         y = standard_length_Deployment)) +
  geom_point(aes(fill = Sex, shape = factor(age_class)), 
             size = 7, stroke = 2.5, alpha = 0.65) +
  geom_smooth(method = "lm", color = "black") +
  scale_fill_manual(values =c("#FD7DD0","deepskyblue"),
                    labels = c("Female", "Male")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Deployment Mass (kgs)", 
       y = "Standard Length (cm)", 
       fill = "Sex", shape = "Age") +
  ggtitle("Standard Length ~ Deployment Mass") +
  theme_clean()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.x = element_line(color = "gray30", linetype = "dotted"),
    plot.title = element_text(face = "bold", color = "black", size = 24),
    axis.text = element_text(face = "bold", color = "black", size = 24),
    axis.title = element_text(face = "bold", color = "black", size = 24),
    legend.key.height = unit(1.5, "cm"))

BodySize_Dep
ggsave("figures/BodySize_Dep.png", BodySize_Dep, dpi = 600,
       width = 17, height = 11, units = "in")


BodySize_Rec <- ggplot(trip_Summary, aes(x = actual_mass_Recovery, 
                                         y = standard_length_Recovery)) +
  geom_point(aes(fill = Sex, shape = factor(age_class)), 
             size = 7, stroke = 2.5, alpha = 0.65) +
  geom_smooth(method = "lm", color = "black") +
  scale_fill_manual(values =c("#FD7DD0","deepskyblue"),
                    labels = c("Female", "Male")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Recovery Mass (kgs)", 
       y = "Standard Length (cm)", 
       fill = "Sex", shape = "Age") +
  ggtitle("Standard Length ~ Recovery Mass") +
  theme_clean()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.x = element_line(color = "gray30", linetype = "dotted"),
    plot.title = element_text(face = "bold", color = "black", size = 24),
    axis.text = element_text(face = "bold", color = "black", size = 24),
    axis.title = element_text(face = "bold", color = "black", size = 24),
    legend.key.height = unit(1.5, "cm"))

BodySize_Rec
ggsave("figures/BodySize_Rec.png", BodySize_Rec, dpi = 600,
       width = 17, height = 11, units = "in")
