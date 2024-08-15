library(tidyverse)
perDive_dat <- readRDS("data/outputs/diveStat_distance.RDS")

#Filtering out H049 because no transmitted ata from that animal
allDives<- perDive_dat %>% 
  filter(animalid != "H049") %>% 
  mutate(SurfaceInterval = round(SurfaceInterval/60, 0)) #Surface interval is in seconds, change to minutes and round to the nearest minute

age_Labels <- c(`0` = "Young-of-the-Year seals", 
                `1` = "1 year-old seals", 
                `2` = "2 year-old seals", 
                `3` = "3 year-old seals")

#Plot dive duration distributions across ages and between sexes
allDur_plot <- ggplot(allDives) +
  geom_histogram(aes(x = DiveDur_min, #y = ..density.., #This is so that your density plot (if you end up putting them in eventually) don't end up like squashed pancakes
                     fill = Sex), alpha = 0.75, bins = 60,
                 position = "identity", color = "gray80")+
  scale_fill_manual(values = c("#FD7DD0", "#7FBFFD"), 
                    labels = c("Female", "Male")) +
  # geom_density(aes(x = DiveDur_min, fill = Sex), #Tried to plot histograms and densities but you loose a lot of resolution in sample size when doing densities
  #              alpha = 0.5) + #So omitting for now but I like the look of it so we're just commenting out in case I'd like to come back to it in the future
  ggtitle("Dive Durations Across Age and Sex") +
  labs(x = "Dive Duration (minutes)", y = "Number of Dives",
       caption = "Figure Credit: S. Abdel-Raheem") +
  facet_wrap(~Age, labeller = as_labeller(age_Labels)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
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
allDur_plot
ggsave("figures/AllDives_DurDistribution.png", allDur_plot)

#Plot maximum dive depth distributions across ages and sexes 
allMax_plot <- ggplot(allDives) +
  geom_histogram(aes(y = MaxDiveDepth, #x = ..density.., #See notes above about plotting densities along with histograms
                     fill = Sex), alpha = 0.75, bins = 60,
                 position = "identity", color = "gray80") +
  #geom_density(aes(y = MaxDiveDepth, fill = Sex), alpha = 0.75) +
  scale_fill_manual(values = c("#FD7DD0", "#7FBFFD"), 
                    labels = c("Female", "Male")) +
  scale_y_reverse() +
  ggtitle("Maximum Dive Depths Across Age and Sex") +
  labs(x =  "Number of Dives", y ="Dive Depth (meters)",
       caption = "Figure Credit: S. Abdel-Raheem") +
  facet_wrap(~Age, labeller = as_labeller(age_Labels)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
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
allMax_plot
ggsave("figures/AllDives_MaxDepDistribution.png", allMax_plot)

#Plot surface interval distributions across ages and between sexes
allSI_plot <- ggplot(allDives) +
  geom_histogram(aes(x = SurfaceInterval, y = ..density.., #This is so that your density plot (if you end up putting them in eventually) don't end up like squashed pancakes
                     fill = Sex), alpha = 0.5, bins = 60,
                 position = "identity", color = "gray80")+
  scale_fill_manual(values = c("#FD7DD0", "#7FBFFD"), 
                    labels = c("Female", "Male")) +
  geom_density(aes(x = SurfaceInterval, fill = Sex), #Tried to plot histograms and densities but you loose a lot of resolution in sample size when doing densities
                alpha = 0.5) + #So omitting for now but I like the look of it so we're just commenting out in case I'd like to come back to it in the future
  ggtitle("Surface Interval Across Age and Sex") +
  labs(x = "Surface Interval (minutes)", y = "Number of Dives",
       caption = "Figure Credit: S. Abdel-Raheem") +
  facet_wrap(~Age, labeller = as_labeller(age_Labels)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
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
allSI_plot
ggsave("figures/SurfaceInterval_DistributionDensities.png", allSI_plot)


#Plot distance from the colony distributions across ages and sexes 
allDistColony_plot <- ggplot(allDives, na.rm = TRUE) +
  geom_histogram(aes(x = Dist_Colony,fill = Sex), 
                 alpha = 0.75, bins = 60,
                 position = "identity", color = "gray80") +
  scale_fill_manual(values = c("#FD7DD0", "#7FBFFD"), 
                    labels = c("Female", "Male")) +
  ggtitle("Distance Traveled from the Colony Across Age and Sex") +
  labs(x = "Distance Traveled (kilometers)"  , y ="Number of Transmissions",
       caption = "Figure Credit: S. Abdel-Raheem") +
  facet_wrap(~Age, labeller = as_labeller(age_Labels), 
             scales = "free_y") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
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
allDistColony_plot
ggsave("figures/DistanceFromColony_Distribution.png", allDistColony_plot)

#Plot latitudes across ages and sexes (spread in N-S direction) -	Northern-most latitude (max lat should be just back at Año)
Lat_Labels <- c(`0` = "YOY N-S Distribution", 
                `1` = "1y/o N-S Distribution", 
                `2` = "2y/o N-S Distribution", 
                `3` = "3y/o N-S Distribution")

allLat_plot <- ggplot(allDives, na.rm = TRUE) +
  geom_histogram(aes(y = Lat, x = ..density.., fill = Sex), 
                 alpha = 0.7, bins = 60, show.legend = FALSE,
                 position = "identity", color = "gray80") +
  geom_density(aes(y = Lat, fill = Sex), alpha = 0.9, 
               show.legend = FALSE) +
  scale_fill_manual(values = c("#FD7DD0", "deepskyblue")) +
  facet_wrap(~Age, labeller = as_labeller(Lat_Labels), 
             scales = "free") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    #panel.grid.minor = element_line(color = "gray", linetype = "dotted"),
    plot.title = element_text(face = "bold", colour = "gray20", hjust = 0.5, 
                              margin = margin(b=10, unit = "pt"), size = 16),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(color = "gray15", size = 12, face = "bold"),
    strip.text = element_blank(),  #element_text(colour = "gray15", size = 12, face = "bold"),
    strip.background = element_blank()) #rect(fill = "#9FCEF2"))
allLat_plot
ggsave("figures/N-S_Distribution_Densities.png", allLat_plot, 
       height = 11, width = 17, units = "in")

#Plot longitudes across ages and sexes (spread in E-W direction) -	Western-most longitude (max long should be just back at Año)
allLong_plot <- ggplot(allDives, na.rm = TRUE) +
  geom_histogram(aes(x = Long, fill = Sex, y = ..density.., ), 
                 alpha = 0.5, bins = 60,
                 position = "identity", color = "gray80") +
  geom_density(aes(x = Long, fill = Sex), alpha = 0.75) +
  scale_fill_manual(values = c("#FD7DD0", "#7FBFFD"), 
                    labels = c("Female", "Male")) +
  ggtitle("East-West Spread Across Age and Sex") +
  labs(x = "Longitude (decimal degrees)"  , y ="Density of Transmissions",
       caption = "Figure Credit: S. Abdel-Raheem") +
  facet_wrap(~Age, labeller = as_labeller(age_Labels), 
             scales = "free_y") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
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
allLong_plot
ggsave("figures/E-W_Distribution_Densities.png", allLong_plot)


#Plot prey capture distributions across ages and between sexes
allPCA_plot <- ggplot(allDives) +
  geom_histogram(aes(x = TotalPreyCap, y = ..density.., #This is so that your density plot (if you end up putting them in eventually) don't end up like squashed pancakes
                     fill = Sex), alpha = 0.5, bins = 60,
                 position = "identity", color = "gray80")+
  scale_fill_manual(values = c("#FD7DD0", "#7FBFFD"), 
                    labels = c("Female", "Male")) +
  geom_density(aes(x = TotalPreyCap, fill = Sex), #Tried to plot histograms and densities but you loose a lot of resolution in sample size when doing densities
               alpha = 0.75) + #So omitting for now but I like the look of it so we're just commenting out in case I'd like to come back to it in the future
  ggtitle("Prey Capture Attempts Across Age and Sex") +
  labs(x = "Activity Count (events)", y = "Number of Dives",
       caption = "Figure Credit: S. Abdel-Raheem") +
  facet_wrap(~Age, labeller = as_labeller(age_Labels)) +
  coord_cartesian(xlim = c(0, 120))+
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
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
allPCA_plot
ggsave("figures/PreyCap_DistributionDensities.png", allPCA_plot)


#Plot average swim effort distributions across ages and between sexes
allAvgSE_plot <- ggplot(allDives) +
  geom_histogram(aes(x = AvgSwimEffort, y = ..density.., #This is so that your density plot (if you end up putting them in eventually) don't end up like squashed pancakes
                     fill = Sex), alpha = 0.5, bins = 60,
                 position = "identity", color = "gray80")+
  scale_fill_manual(values = c("#FD7DD0", "#7FBFFD"), 
                    labels = c("Female", "Male")) +
  geom_density(aes(x = AvgSwimEffort, fill = Sex), #Tried to plot histograms and densities but you loose a lot of resolution in sample size when doing densities
               alpha = 0.75) + #So omitting for now but I like the look of it so we're just commenting out in case I'd like to come back to it in the future
  ggtitle("Average Swim Effort Across Age and Sex") +
  labs(x = "Avgerage Swim Effort Per Dive (???)", y = "Number of Dives",
       caption = "Figure Credit: S. Abdel-Raheem") +
  facet_wrap(~Age, labeller = as_labeller(age_Labels)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
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
allAvgSE_plot
ggsave("figures/AvgDiveSE_DistributionDensities.png", allAvgSE_plot)

