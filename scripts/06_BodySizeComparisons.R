library(tidyverse)

morphDat <- readRDS("data/outputs/trip_Summary.RDS")

#Let's just plot each seal's deployment mass, ax girth, SL

#Deployment Mass
perSeal_depMass <- ggplot(morphDat, aes(x = factor(age_class), y = actual_mass_Deployment)) +
  geom_point(aes(fill = Sex), shape = 21, stroke = 1.5, size = 3, alpha = 0.75)+
  scale_fill_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  labs(x = "Age (years)", y = "Mass (kg)", 
       title = "Comparison of Deployment Mass by Age and Sex",
       caption = "Figure Credit: S. Abdel-Raheem") +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.text.align = 0,
    legend.title.align = 0.5)
perSeal_depMass
ggsave("figures/allSeal_depMass.png", perSeal_depMass)

#Deployment SL
perSeal_depSL <- ggplot(morphDat, aes(x = factor(age_class), y = standard_length_Deployment)) +
  geom_point(aes(fill = Sex), shape = 21, stroke = 1.5, size = 3, alpha = 0.75)+
  scale_fill_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  labs(x = "Age (years)", y = "Standard Length (cm)", 
       title = "Comparison of Deployment Standard Length by Age and Sex",
       caption = "Figure Credit: S. Abdel-Raheem") +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.text.align = 0,
    legend.title.align = 0.5)
perSeal_depSL
ggsave("figures/allSeal_depSL.png", perSeal_depSL)

#Deployment Ax Girth
perSeal_depAxGirth <- ggplot(morphDat, aes(x = factor(age_class), y = ax_girth_Deployment)) +
  geom_point(aes(fill = Sex), shape = 21, stroke = 1.5, size = 3, alpha = 0.75)+
  scale_fill_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  labs(x = "Age (years)", y = "Axillary Girth (cm)", 
       title = "Comparison of Deployment Ax Girth by Age and Sex",
       caption = "Figure Credit: S. Abdel-Raheem") +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.text.align = 0,
    legend.title.align = 0.5)
perSeal_depAxGirth
ggsave("figures/allSeal_depAxGirth.png", perSeal_depAxGirth)

#Trip distributions
tripDur_dist <- ggplot(morphDat, aes(x = factor(age_class), 
                                     y = trip_Dur, 
                                     fill = Sex)) +
  geom_dotplot(aes (fill = Sex), binaxis="y", stackdir="center", 
               dotsize=0.5, color="black", alpha = 0.9, show.legend = FALSE) +
  geom_violin(position = position_dodge(width = 0.5), alpha = 0.75, color = "gray20") +
  scale_fill_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  labs(x = "Age (years)", y = "Trip Duration (days)", fill = "Sex", 
       title = "Foraging Trip Duration",
       caption = str_wrap(
         "Foraging trip distributions across age and sex. Trip durations were 
         calculated by subtracting the departure date from the arrival or last 
         observed date. For animals from which instruments were recovered (n = 43), 
         departure and arrival dates were determined from archival data. For 
         animals from which instruments were not recovered (n = 21), the first 
         and last Argos transmisison dates were used as the departure and last 
         observed dates.", 150)) +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, r = 0, b = 0, l = 0), hjust = 0),
    plot.caption.position = "plot",
    legend.text.align = 0,
    legend.title.align = 0.5)
tripDur_dist
ggsave("figures/TripDurationDistributions.png", tripDur_dist)

#Change in mass distributions
#Filter just the weenie recoveries to plot their single points below.
recWeanies <- morphDat %>% 
  filter(age_class == 0 & !is.na(change_Mass))

massChange_dist <- ggplot(morphDat, aes(x = factor(age_class), y = change_Mass, fill = Sex)) +
  geom_violin(position = position_dodge(width = 0.5), alpha = 0.9, color = "gray20") +
  geom_point(data = recWeanies, aes(x = factor(age_class), y = change_Mass, fill = Sex), 
             shape = 21, stroke = 1.5, size = 3, alpha = 0.75, show.legend = FALSE) +
  scale_fill_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  labs(x = "Age (years)", y = "Mass Change (kgs)", fill = "Sex", 
       title = "Mass Change",
       caption = str_wrap(
         "Mass change across age and sex. Mass change was calculated by 
         subtracting the deployment mass from the recovery mass for animals from 
         which instruments were recovered (n = 43).", 150)) +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, r = 0, b = 0, l = 0), hjust = 0),
    plot.caption.position = "plot",
    legend.text.align = 0,
    legend.title.align = 0.5)
massChange_dist
ggsave("figures/MassChangeDistributions.png", massChange_dist)

#Change in SL 
SLchange_dist <- ggplot(morphDat, aes(x = factor(age_class), y = change_SL, fill = Sex)) +
  geom_violin(position = position_dodge(width = 0.5), alpha = 0.9, color = "gray20") +
  geom_point(data = recWeanies, aes(x = factor(age_class), y = change_SL, fill = Sex), 
             shape = 21, stroke = 1.5, size = 3, alpha = 0.75, show.legend = FALSE) +
  scale_fill_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  labs(x = "Age (years)", y = "Standard Length Change (cm)", fill = "Sex", 
       title = "Standard Length Change",
       caption = str_wrap(
         "Change in standard (straight) length across age and sex. Body length 
         change was calculated by subtracting the deployment standard length 
         from the recovery length for animals from which instruments were 
         recovered (n = 43).", 150)) +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, r = 0, b = 0, l = 0), hjust = 0),
    plot.caption.position = "plot",
    legend.text.align = 0,
    legend.title.align = 0.5)
SLchange_dist
ggsave("figures/SLChangeDistributions.png", SLchange_dist)

#Change in Ax girth
AxGirth_change_dist <- ggplot(morphDat, aes(x = factor(age_class), y = change_AxGirth, fill = Sex)) +
  geom_violin(position = position_dodge(width = 0.5), alpha = 0.9, color = "gray20") +
  geom_point(data = recWeanies, aes(x = factor(age_class), y = change_AxGirth, fill = Sex), 
             shape = 21, stroke = 1.5, size = 3, alpha = 0.75, show.legend = FALSE) +
  scale_fill_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  labs(x = "Age (years)", y = "Ax Girth Change (cm)", fill = "Sex", 
       title = "Girth Change",
       caption = str_wrap(
         "Change in axillary girth across age and sex. Girth change was calculated 
         by subtracting the deployment axillary girth from the recovery girth 
         for animals from which instruments were recovered (n = 43).", 150)) +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, r = 0, b = 0, l = 0), hjust = 0),
    plot.caption.position = "plot",
    legend.text.align = 0,
    legend.title.align = 0.5)
AxGirth_change_dist
ggsave("figures/AxGirthChangeDistributions.png", AxGirth_change_dist)

#Let's look at the relationship between deployment duration and recovery mass
RecoveryMass_DeployDur <- ggplot(morphDat, aes(x = deployment_Dur, y = actual_mass_Recovery)) +
  # geom_crossbar(aes(ymin = Avg_MaxDdep - SE_MaxDdep, ymax = Avg_MaxDdep + SE_MaxDdep,
  #                   xmin = Avg_Ddur - SE_Ddur, xmax = Avg_Ddur + SE_Ddur))+
  geom_point(aes(fill = Sex, shape = factor(age_class)),
             size = 7, stroke = 1.5, alpha = 0.75) + 
  scale_fill_manual(values =c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Deployment Duration (days)", 
       y = "Mass at Recovery (kgs)", 
       fill = "Sex", shape = "Age",
       caption = "Figure Credit: S. Abdel-Raheem") +
  ggtitle("Mass ~ Trip") +
  theme_clean()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.key.height = unit(1.5, "cm"),
    legend.text.align = 0,
    legend.background = element_rect(color = "transparent"),
    legend.title.align = 0.5)
RecoveryMass_DeployDur
ggsave("figures/RecoveryMass_DeployDur.png", RecoveryMass_DeployDur)

#Mass Gain vs Deployment Duration?
MassGain_DeployDur <- ggplot(morphDat, aes(x = deployment_Dur, y = change_Mass)) +
# geom_crossbar(aes(ymin = Avg_MaxDdep - SE_MaxDdep, ymax = Avg_MaxDdep + SE_MaxDdep,
#                   xmin = Avg_Ddur - SE_Ddur, xmax = Avg_Ddur + SE_Ddur))+
geom_point(aes(fill = Sex, shape = factor(age_class)),
           size = 7, stroke = 1.5, alpha = 0.75) + 
  scale_fill_manual(values =c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Deployment Duration (days)", 
       y = "Mass Change (kgs)", 
       fill = "Sex", shape = "Age",
       caption = "Figure Credit: S. Abdel-Raheem") +
  ggtitle("Mass Gain ~ Deployment") +
  theme_clean()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.key.height = unit(1.5, "cm"),
    legend.text.align = 0,
    legend.background = element_rect(color = "transparent"),
    legend.title.align = 0.5)
MassGain_DeployDur
ggsave("figures/MassGaom_DeployDur.png", MassGain_DeployDur)

#SLchange vs Trip Duration?
SLGain_TripDur <- ggplot(morphDat, aes(x = trip_Dur, y = change_SL)) +
  # geom_crossbar(aes(ymin = Avg_MaxDdep - SE_MaxDdep, ymax = Avg_MaxDdep + SE_MaxDdep,
  #                   xmin = Avg_Ddur - SE_Ddur, xmax = Avg_Ddur + SE_Ddur))+
  geom_point(aes(fill = Sex, shape = factor(age_class)),
             size = 7, stroke = 1.5, alpha = 0.75) + 
  scale_fill_manual(values =c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Foraging Trip Duration (days)", 
       y = "Length Change (cm)", 
       fill = "Sex", shape = "Age",
       caption = "Figure Credit: S. Abdel-Raheem") +
  ggtitle("Standard Length Gain ~ Trip") +
  theme_clean()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.key.height = unit(1.5, "cm"),
    legend.text.align = 0,
    legend.background = element_rect(color = "transparent"),
    legend.title.align = 0.5)
SLGain_TripDur
ggsave("figures/SLGain_TripDur.png", SLGain_TripDur)

#AxGirth vs Trip Duration?
AxGgain_TripDur <- ggplot(morphDat, aes(x = trip_Dur, y = change_AxGirth)) +
  # geom_crossbar(aes(ymin = Avg_MaxDdep - SE_MaxDdep, ymax = Avg_MaxDdep + SE_MaxDdep,
  #                   xmin = Avg_Ddur - SE_Ddur, xmax = Avg_Ddur + SE_Ddur))+
  geom_point(aes(fill = Sex, shape = factor(age_class)),
             size = 7, stroke = 1.5, alpha = 0.75) + 
  scale_fill_manual(values =c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Foraging Trip Duration (days)", 
       y = "Ax Girth Change (cm)", 
       fill = "Sex", shape = "Age",
       caption = "Figure Credit: S. Abdel-Raheem") +
  ggtitle("Axillary Girth Gain ~ Trip") +
  theme_clean()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.key.height = unit(1.5, "cm"),
    legend.text.align = 0,
    legend.background = element_rect(color = "transparent"),
    legend.title.align = 0.5)
AxGgain_TripDur
ggsave("figures/AxGirth-Gain_TripDur.png", AxGgain_TripDur)



#Now let's look at some age-sex distributions
# Define a custom function to calculate SEM
calc_SEM <- function(x) {
  n <- sum(!is.na(x))
  if (n > 1) {
    sd_x <- sd(x, na.rm = TRUE)
    return(sd_x / sqrt(n))
  } else {
    return(NA)
  }
}

#group the data by age and sex
ageSex_morphs <- morphDat %>% 
  #Filter out columns that won't be used for calculations
  select(-matches("Season|Date|Lat|Lon|Loc")) %>% 
  #Group by age and sex
  group_by(age_class, Sex) %>% 
  summarise(
    #Calculating the number of individuals that were recovered
    Recd = sum(!is.na(rec_Diff)), 
    #Calculating the age-sex group means across all the columns
    across(dep_Diff:change_Mass, 
           list(mean = ~mean(., na.rm = TRUE), 
                #Calculating the age-sex group SEMs across all the columns
                SE = ~ calc_SEM(.)),
           #Assigning names to columns based on the function used to create them
           .names = "{.col}_{.fn}") 
    ) %>% 
  ungroup() %>% 
  mutate(
    #sigfigs bitch
    #Rounding to the nearest digit bc those values are whole number measurements 
    across(dep_Diff_mean:change_AxGirth_SE, ~ round(., digits = 0)), 
    #Rounding to the 0.1 bc that's how those measurements were OG collected
    across(actual_mass_Deployment_mean:change_Mass_SE, ~ round(., digits = 1)) 
    )
