library(tidyverse)
perDive_dat <- readRDS("data/outputs/diveStat_distance.RDS")

#Let's get per seal summaries so we can look at some interesting distributions
#First let's look at dive duration
perSeal_dDur <- perDive_dat %>% 
  filter(animalid != "H049") %>% 
  group_by(animalid, Age, Sex) %>% 
  summarise(
    #Total number of dives that a seal took throughout its dive record
    DiveNum_tot = max(DiveNum, na.rm = TRUE),
    min_Ddur = min(DiveDur_min),
    max_Ddur = max(DiveDur_min),
    med_Ddur = median(DiveDur_min),
    Avg_Ddur = mean(DiveDur_min),
    SE_Ddur = sd(DiveDur_min)/sqrt(DiveNum_tot)) %>% 
  ungroup()

#Age-Sex grouping and calculating the mean of different dive metrics for each age-sex group
grouped_dDur <- perSeal_dDur %>% 
  group_by(Age, Sex) %>% 
  summarise(
    A_Min_Ddur = mean(min_Ddur), #Average minimum dive duration across all seals in an Age-Sex group
    B_Max_Ddur = mean(max_Ddur), #Average maximum dive duration across all seals in an Age-Sex group
    C_Median_Ddur = mean(med_Ddur), #Average median dive duration across all seals in an Age-Sex group
    D_Mean_Ddur = mean(Avg_Ddur),  #Grand Average dive duration across all seals in an Age-Sex group
    SE_Ddur = sd(Avg_Ddur) / sqrt(sum((DiveNum_tot)))
    ) %>%  # Standard Error for the mean dive duration across all seals in an Age-Sex group multiplied by 60 to get it into minutes
  ungroup()  # Remove grouping for plotting

# Pivot the data for plotting with ggplot2
dDur_pivot <- grouped_dDur %>%
  pivot_longer(cols = c("A_Min_Ddur", "B_Max_Ddur", "C_Median_Ddur", "D_Mean_Ddur"), 
               names_to = "Metric", values_to = "Duration")

dDur_labels <- c(`A_Min_Ddur` = "Minimum Dive Duration", 
                 `B_Max_Ddur` = "Maximum Dive Duration", 
                 `C_Median_Ddur` = "Median Dive Duration", 
                 `D_Mean_Ddur` = "Average Dive Duration ± SE")
# Plotting
dDur_plot <- ggplot(dDur_pivot, aes(x = Age, y = Duration, color = Sex)) +
  geom_line(aes(group = interaction(Sex, Metric))) +
  geom_point(aes(shape = Sex), size = 3, stroke = 1.5) +
  facet_wrap(~ Metric, # Separate facet for each metric with independent y scales
             labeller = as_labeller(dDur_labels)) + 
  geom_errorbar(aes(ymin = Duration - SE_Ddur, ymax = Duration + SE_Ddur), width = 0.2, 
                data = subset(dDur_pivot, Metric == 'D_Mean_Ddur')) + # Error bars only for mean
  scale_color_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  ggtitle("Dive Duration Summary Statistics Across Age and Sex") +
  labs(x = "Age (years)", y = "Dive Duration (minutes)", 
       caption = "Figure Credit: S. Abdel-Raheem") +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"),
    plot.title = element_text(face = "bold", colour = "gray20", hjust = 0.5, 
                              margin = margin(b=10, unit = "pt"), size = 16),
    legend.title = element_text(colour = "black", size = 14),
    legend.title.align = 0.5,
    legend.text = element_text(colour = "black", size = 12),
    legend.text.align = 0,
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10, 
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    plot.caption.position = "plot", 
    strip.text = element_text(colour = "black", size = 12, face = "italic"))
dDur_plot
ggsave("figures/DiveDuration.png", dDur_plot)

#Let's look at the relationship between dive depth and duration across age and sex
#Summarize data for each seal
perSeal_DepDur <- perDive_dat %>% 
  filter(animalid != "H049") %>% 
  group_by(animalid, Age, Sex) %>% 
  summarise(
    Tot_Dives = max(DiveNum),
    Avg_Ddur = mean(DiveDur_min),
    SE_Ddur = round(sd(DiveDur_min)/sqrt(max(DiveNum)),2), #Multiply by 60 to get SE in minutes?
    SD_Ddur = sd(DiveDur_min),
    Avg_MaxDdep = mean(MaxDiveDepth),
    SE_MaxDdep = round(sd(MaxDiveDepth)/sqrt(max(DiveNum)),2),
    SD_MaxDdep = sd(MaxDiveDepth)) %>% 
  ungroup() %>% 
  mutate(SexAge = paste0(Sex, Age))

#Let's plot dive depth ~ dive duration by seal
perSeal_DepxDur <- ggplot(perSeal_DepDur, aes(x = Avg_Ddur, y = Avg_MaxDdep)) +
  geom_crossbar(aes(ymin = Avg_MaxDdep - SE_MaxDdep, ymax = Avg_MaxDdep + SE_MaxDdep,
                    xmin = Avg_Ddur - SE_Ddur, xmax = Avg_Ddur + SE_Ddur))+
  geom_point(aes(fill = Sex, shape = factor(Age)),
             size = 7, stroke = 1.5, alpha = 0.75) + 
  scale_y_reverse()+
  scale_fill_manual(values =c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Dive Duration (minutes; mean ± SE)", 
       y = "Dive Depth (meters; mean ± SE)", 
       fill = "Sex", shape = "Age",
       caption = "Figure Credit: S. Abdel-Raheem") +
  ggtitle("Depth ~ Duration") +
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
perSeal_DepxDur
ggsave("figures/perSeal_Duration-Depth.png", perSeal_DepxDur)

#Now summarize across age-sex groups
grouped_DepDur <- perSeal_DepDur %>% 
  group_by(Age, Sex) %>% 
  summarise(
    DiveSum = sum(Tot_Dives),
    GrandMean_Ddur = mean(Avg_Ddur),
    SE_GrandMean_Ddur = sd(Avg_Ddur)/sqrt(sum(Tot_Dives)), 
    SD_GrandMean_Ddur = sd(Avg_Ddur),
    GrandMean_MaxDdep = mean(Avg_MaxDdep),
    SE_GrandMean_MaxDdep = sd(Avg_MaxDdep)/sqrt(sum(Tot_Dives)),
    SD_GrandMean_MaxDdep = sd(Avg_MaxDdep)) %>% 
  mutate(SexAge = paste0(Sex, Age)) %>% 
  ungroup()

# Plotting Maximum Dive Depth ~ Duration Across Sex and Age
grouped_DepxDur <- ggplot(grouped_DepDur, aes(x = GrandMean_Ddur, y = GrandMean_MaxDdep)) +
  geom_crossbar(aes(ymin = GrandMean_MaxDdep - SE_GrandMean_MaxDdep, 
                    ymax = GrandMean_MaxDdep + SE_GrandMean_MaxDdep,
                    xmin = GrandMean_Ddur - SE_GrandMean_Ddur, 
                    xmax = GrandMean_Ddur + SE_GrandMean_Ddur))+
  geom_point(aes(fill = Sex, shape = factor(Age)),
             size = 7, stroke = 1.5, alpha = 0.75) + 
  #geom_smooth(aes(color = Sex), method = "loess", se = FALSE, size = 1) +
  scale_y_reverse()+
  scale_fill_manual(values =c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(x = "Grand Mean Dive Duration (minutes; mean ± SE)", 
       y = "Grand Mean Maximum Dive Depth (meters; mean ± SE)", 
       fill = "Sex", shape = "Age",
       caption = "Figure Credit: S. Abdel-Raheem") +
  ggtitle("Age-Sex Grand Mean Dive Duration x Max Depth") +
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
grouped_DepxDur
ggsave("figures/grouped_Duratio-Depth_Trends.png", grouped_DepxDur)
 