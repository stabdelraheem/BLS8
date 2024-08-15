library(tidyverse)
library(ggthemes)

distColony_perDive <- readRDS("data/outputs/diveStat_distance.RDS") 

#Let's first look at mortality across age and sex
rec_Status <- distColony_perDive %>% 
  group_by(TOPPID, animalid, Age, Sex) %>% 
  summarise(Rec = first(Recovered)) %>% 
  arrange(TOPPID)

#Tallying up recovery vs. mortality across ages and sex
mortality <- rec_Status %>% 
  group_by(Age, Sex) %>% 
  summarise(Survived = sum(Rec == TRUE), 
            Mortality = sum(Rec == FALSE))

#Pivoting longer to better plot both survivorship & mortality
mortality_long <- mortality %>% 
  pivot_longer(cols = c(Survived, Mortality),
               names_to = "Status", values_to = "Count")

mort_labels <- c(`F` = "Females", 
                 `M` = "Males") 

#Mortality + Survivorship plot!
mort_plot <-  ggplot(mortality_long, aes(x = Age, y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.75, width = 0.5, 
           color = "gray20") +
  geom_text(data = subset(mortality_long, Count != 0), aes(label = Count), 
            position = position_stack(vjust = 0.5), color = "black", size = 4,
            family = "sans",  # Optional: set the font family
            fontface = "bold") + # Add text labels for non-zero counts
  labs(x = "Age", y = "Count", fill = "Status") +
  scale_fill_manual(values = c("#D52C2C", "#B2B2E1")) +
  #ggtitle("Survivorship and Mortality by Age") +
  facet_wrap(~ Sex, nrow = 1, # Facet by Sex to separate the bars for each sex
             labeller = as_labeller(mort_labels), 
             scales = "free_x") +  
  theme_classic() +
  theme(
    plot.background = element_rect(fill = "aliceblue"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_text(face = "bold", color = "black", size = 12),
    axis.title.x = element_text(face = "bold", color = "black", size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.direction  = "horizontal",
    legend.background = element_rect(color = "black", fill = "aliceblue"),
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.title.align = 0.5,
    legend.text = element_text(face = "bold", color = "black", size = 10),
    strip.text = element_text(colour = "black", size = 14, face = "bold"),
    strip.background = element_blank())
mort_plot
ggsave("figures/mortPlot_horizontal.png", mort_plot, dpi = 600, 
       width = 10, height = 6, units = "in")


#Let's first characterize the transmitted data we're working with for each seal
#How many dives and transmitted data points are we working with per seal
TxData_qty <- distColony_perDive %>% 
  #Select only the Tx and Dive Num data
  group_by(TOPPID, animalid) %>% 
  summarise(
    Age = first(Age),
    Sex = first(Sex),
    # of unique days for which data was transmitted
    TxDays = n_distinct(Date),
    #Total number of dives that a seal took throughout its dive record
    DiveNum_tot = max(DiveNum, na.rm = TRUE)) %>% 
  #Dealing with the problem child that H049
  mutate(TxDays = case_when(animalid == "H049" ~ 0, 
                            TRUE ~ TxDays), 
         DiveNum_tot = case_when(animalid == "H049" ~ 0, 
                                 TRUE ~ DiveNum_tot))

#Let's plot Tx data quality/quantity to get some trends, we'd like a scatter plot plz!
TxQty_colors <- c(F = "#FD7DD0", 
                  M = "#7FBFFD")


Tx_plot <-  ggplot(TxData_qty, aes(x = TxDays, y = DiveNum_tot, 
                                   shape = factor(Age))) +
  geom_point(aes(fill = Sex), size = 5, stroke = 1, alpha = 0.75) +
  scale_fill_manual(values = TxQty_colors, name = "Sex") +
  scale_shape_manual(values = c(21, 22, 23, 24),
                     labels = c("YOY", "1 y/o", "2 y/o", "3 y/o")) +
labs(x = "Number of Days with Transmissions", 
     y = "Total Number of Transmitted Dives", 
     fill = "Sex", shape = "Age",
     caption = "Figure Credit: S. Abdel-Raheem") +
  ggtitle("Data Transmissions") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  theme_clean()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.key.height = unit(1, "cm"), 
    legend.text.align = 0, 
    legend.background = element_rect(color = "transparent"),
    legend.title.align = 0.5,
    legend.position = c(0.95,0.23))
Tx_plot
ggsave("figures/Tx_plot.png", Tx_plot)
  
# # Txqty_facet <- ggplot(TxQty_long, aes(x = factor(Age), y = Value, fill = Sex)) +
#   geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
#   #geom_point(shape = 21, size = 7,  color = "black") +
#   labs(x = "Age", y = "Count", fill = "Sex", 
#        caption = "Figure Credit: S. Abdel-Raheem") +
#   scale_fill_manual(values = TxQty_colors) +
#   ggtitle("Data Transmissions by Sex and Age") +
#   facet_wrap(~ Metric, ncol = 1, # Facet by Sex to separate the bars for each sex
#              labeller = as_labeller(TxQty_labels)) +  
#   theme_clean()+
#   theme(
#     panel.border = element_rect(color = "black", fill = NA, size = 1),
#     strip.text = element_text(colour = "black", size = 14, face = "italic"), 
#     plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
#                                 margin = margin(t = 10, unit = "pt"), hjust = 0))


# Group data by age and sex, then summarize to calculate total dives + transmissions
data_dive_counts <- TxData_qty %>%
  group_by(Age, Sex) %>%
  summarise(Total_Dives = sum(DiveNum_tot, na.rm = TRUE), 
            Total_Transmissions = sum(TxDays, na.rm = TRUE)) %>%
  ungroup()  # Remove grouping

# Plotting dive counts by sex and age
dive_Count <- ggplot(data_dive_counts, aes(x = factor(Age), y = Total_Dives, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", color = "gray30") +
  geom_text(aes(label = Total_Dives), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black")+
  scale_fill_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  labs(x = "Age (years)", y = "Number of Transmitted Dives", 
       title = "Comparison of Dive Counts by Age and Sex",
       caption = "Figure Credit: S. Abdel-Raheem") +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.text.align = 0,
    legend.title.align = 0.5)
dive_Count
ggsave("figures/DiveCounts.png", dive_Count)



# Plotting data transmissions by sex and age
Tx_Count <- ggplot(data_dive_counts, aes(x = factor(Age), y = Total_Transmissions, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", color = "gray30") +
  geom_text(aes(label = Total_Transmissions), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black")+
  scale_fill_manual(values = c("M" = "#7FBFFD", "F" = "#FD7DD0")) +
  labs(x = "Age (years)", y = "Number of Transmission Days", 
       title = "Comparison of Data Transmissions by Age and Sex",
       caption = "Figure Credit: S. Abdel-Raheem") +
  theme_clean() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10,
                                margin = margin(t = 10, unit = "pt"), hjust = 0),
    legend.text.align = 0,
    legend.title.align = 0.5)
Tx_Count
ggsave("figures/TxCounts.png", Tx_Count)
 