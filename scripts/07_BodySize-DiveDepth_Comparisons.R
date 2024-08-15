library(tidyverse)
library(ggthemes)

#read in dive data 
diveDat <- read_rds("data/outputs/diveStat_distance.RDS")

#Let's group dive data for each seal
perSeal_DiveDepDur <- diveDat %>% 
  filter(animalid != "H049") %>% 
  group_by(animalid, Age, Sex) %>% 
  summarise(
    #Was the instrument recovered?
    Recovered = first(Recovered),
    #Total number of dives that a seal took throughout its dive record
    DiveNum_tot = max(DiveNum, na.rm = TRUE),
    #Average dive duration first
    Avg_Ddur = mean(DiveDur_min),
    SE_Ddur = sd(DiveDur_min)/sqrt(DiveNum_tot), 
    #Average dive depth second
    Avg_Ddep = mean(MaxDiveDepth),
    SE_Ddep = sd(MaxDiveDepth)/sqrt(DiveNum_tot)) %>% 
  ungroup()

#read in & body size data
bodySize <- read_rds("data/outputs/trip_Summary.RDS") %>% 
  #No need for all of the data columns
  select(-matches("Season|Date|Lat|Lon|Loc|diff")) %>% 
  rename(Age = age_class)

#Join dive and body size data
dives_size <- left_join(bodySize, perSeal_DiveDepDur, 
                        by = c("animalid", "Sex", "Age")) %>% 
  relocate(Recovered, .after = Sex)

#Body Size x Dive Duration
DepMass_dDur <- ggplot(dives_size, aes(x = actual_mass_Deployment, y = Avg_Ddur)) +
  geom_crossbar(aes(ymin = Avg_Ddur - SE_Ddur, ymax = Avg_Ddur + SE_Ddur))+
  geom_point(aes(fill = Sex, shape = factor(Age), color = Recovered),
             size = 7, stroke = 2.5, alpha = 0.65) + 
  scale_fill_manual(values =c("#FD7DD0","deepskyblue"), 
                    labels = c("Female", "Male")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  scale_color_manual(values = c("TRUE" = "#B2B2E1", "FALSE" = "#D52C2C"), #Saving this color in case #BCBCF8"
                     labels= c("TRUE" = "Survival", "FALSE" = "Mortality"),
                     name = "Status") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Deployment Mass (kgs)", 
       y = "Dive Duration (minutes; mean ± SE)", 
       fill = "Sex", shape = "Age") +
  ggtitle("Dive Duration ~ Deployment Mass") +
  theme_clean()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.x = element_line(color = "gray30", linetype = "dotted"),
    plot.background = element_rect(fill = "aliceblue"), 
    plot.title = element_text(face = "bold", color = "black", size = 24),
    axis.text = element_text(face = "bold", color = "black", size = 24),
    axis.title = element_text(face = "bold", color = "black", size = 24),
    legend.key.height = unit(1.5, "cm"),
    legend.key = element_rect(fill = "aliceblue"), 
    legend.text.align = 0,
    legend.background = element_rect(color = "transparent", fill = "aliceblue"),
    legend.title.align = 0.5)
DepMass_dDur
ggsave("figures/DiveDur_DepMass.png", DepMass_dDur, dpi = 600,
       width = 17, height = 11, units = "in")

#Body Size x Dive Depth
DepMass_dDep <- ggplot(dives_size, aes(x = actual_mass_Deployment, y = Avg_Ddep)) +
  geom_crossbar(aes(ymin = Avg_Ddep - SE_Ddep, ymax = Avg_Ddep + SE_Ddep))+
  geom_point(aes(fill = Sex, shape = factor(Age), color = Recovered),
             size = 7, stroke = 2.5, alpha = 0.65, show.legend = FALSE) +
  scale_y_reverse() +
  scale_fill_manual(values =c("M" = "deepskyblue", "F" = "#FD7DD0")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  scale_color_manual(values = c("TRUE" = "#B2B2E1", "FALSE" = "#D52C2C"), #Saving this color in case #BCBCF8"
                     labels= c("TRUE" = "Survival", "FALSE" = "Mortality"),
                     name = "Status") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Deployment Mass (kgs)", 
       y = "Dive Depth (meters; mean ± SE)", 
       fill = "Sex", shape = "Age") +
  ggtitle("Dive Depth ~ Deployment Mass") +
  theme_clean()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.x = element_line(color = "gray30", linetype = "dotted"),
    plot.background = element_rect(fill = "aliceblue"), 
    plot.title = element_text(face = "bold", color = "black", size = 24),
    axis.text = element_text(face = "bold", color = "black", size = 24),
    axis.title = element_text(face = "bold", color = "black", size = 24),
    legend.key.height = unit(1.5, "cm"),
    legend.key = element_rect(fill = "aliceblue"), 
    legend.text.align = 0,
    legend.background = element_rect(color = "transparent", fill = "aliceblue"),
    legend.title.align = 0.5)
DepMass_dDep
ggsave("figures/DiveDep_DepMass.png", DepMass_dDep, dpi = 600,
       width = 17, height = 11, units = "in")
