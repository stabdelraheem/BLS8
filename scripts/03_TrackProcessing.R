library(lubridate)
library(tidyverse)

#Create a vector of all relevant data files and their paths
tracking_dat <- dir('data/TrackingData/', 
                       full.names = TRUE, 
                       pattern = ".*csv") %>% 
  read_csv(id = "file") %>% 
  rename(PointNum = ...1)

#Great! Now let's add some age and sex data to the tracking data to make things interesting
#Let's use the trip summary we made in the step 2 and use the "TOPPID" column to selectively join age and sex data to the tracking data
trip_Summary <- read_rds("data/outputs/trip_Summary.RDS")

tracking_f <- left_join(tracking_dat, 
                        select(trip_Summary, animalid, age_class, Sex, TOPPID), 
                        by = c("id" = "TOPPID")) %>% 
  relocate(animalid:Sex, .after = id) %>% 
  rename(Age = age_class) %>% 
  rename(TOPPID = id) %>% 
  select(-1) %>% 
  relocate(TOPPID:Sex, .before = PointNum) %>% 
  #Filter out outlier points
  filter(!lon > -110) %>% 
  filter(!lat < 0)



#Save output as RDS to retain date-time format
saveRDS(tracking_f, "data/outputs/tracks_all.RDS")

library(sf)
library(rnaturalearth)
library(ggspatial)
library(tmap)

#Making data points into spatial features object
tracking_sf <- st_as_sf(tracking_f, coords = c("lon", "lat"), crs = 4326) %>% 
  group_by(animalid, TOPPID, Age, Sex) %>% 
  arrange(date, .by_group = TRUE) %>% 
  ungroup()


#Making land and coastline base maps 
land <- ne_download(scale = 10, type = "land", category = "physical",
                    returnclass = "sf")

coast <- ne_coastline(scale = 10, returnclass = "sf")

seal_Lines <- tracking_sf %>% 
  # remove the land points using a logical vector applied to a list item
  mutate(land_intersects = st_intersects(., land), #Creates a col that IDs the points that cross land?
         overland = map_lgl(land_intersects, \(x) length(x) > 0)) %>% #Takes the values from the preceding col and makes them into a T/F vector
  filter(!overland) %>% #Filters out data points that do not intersect with the land shapefile 
  select(-land_intersects, -overland) %>% #Delete the two vectors initially created to ID and filter out land points, we like tidy data frames 
 # Convert points to lines
  group_by(TOPPID, animalid, Age, Sex) %>%
  summarize(.groups = "drop", 
            do_union = FALSE) %>% #Without this the points are connected in a weird order this keeps it so that st_cast mess up the order of 
  mutate(SexAge = paste0(Sex,Age)) %>% 
  st_cast("LINESTRING") 

#Map Legend Colors and Labels
map_colors <- c(F3 = "mediumorchid4",
                 F2 = "#C02883",
                 F1 = "#FE1EAC",
                 F0 = "#FD7DD0",
                 M3 = "dodgerblue4",
                 M2 = "deepskyblue4",
                 M1 = "#00B4D8",
                 M0 = "#7FBFFD")
map_labels <- c(F3 = "3 y/o Female",
            F2 = "2 y/o Female",
            F1 = "1 y/o Female",
            F0 = "YOY Female",
            M3 = "3 y/o Male",
            M2 = "2 y/o Male",
            M1 = "1 y/o Male",
            M0 = "YOY Male")


# Let's finally MAP! 
# First let's map all the tracks 
ALLseals <- ggplot(seal_Lines) +
  geom_sf(data = land, fill = "gray20") +
  geom_sf(aes(color = SexAge), alpha = 0.5) +
  scale_color_manual(name = "Sex and Age", 
                     values = map_colors, 
                     labels = map_labels)+
  coord_sf(xlim = c(-180, -110), ylim = c(30, 60)) +
  annotation_north_arrow(which_north = "true", location = "tl", 
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = "bl", style = "ticks", line_width = 1.5)+
  ggtitle("All Seal Tracks") +
  labs(caption = "Figure Credit: S. Abdel-Raheem") +
  theme_classic() +
  theme(
    legend.position = c(0.9,0.5),
    legend.title = element_text(face = "bold", colour = "black", size = 14),
    legend.text = element_text(colour = "black", size = 12),
    legend.text.align = 1,
    legend.key = element_rect(fill = "gray100", color = "transparent"),
    legend.background = element_rect(fill = "gray100", color = "black"),
    plot.title = element_text(face = "bold", colour = "gray20", hjust = 0.5, margin = margin(b=10, unit = "pt"), size = 24),
    plot.caption = element_text(face = "italic", colour = "gray20", size = 10, margin = margin(t = 10, unit = "pt"), hjust = 0),
    plot.caption.position = "plot")
ggsave("figures/AllSealTracks.png", ALLseals)

#Lets separate age and sex to better observe trends across ontogeny
#Lets make some custom facet panel labels first
facet_labels <- c(`0` = "Young-of-the-Year seals", 
                  `1` = "1 year-old seals", 
                  `2` = "2 year-old seals", 
                  `3` = "3 year-old seals")

#Now make a facet map
AGExSEX <- ggplot(seal_Lines) +
  geom_sf(data = land, fill = "gray20") +
  geom_sf(aes(color = Sex), size = 3) +
  scale_color_manual(values = c("#FE1EAC", "deepskyblue"))+
  facet_wrap(~Age, 
             labeller = as_labeller(facet_labels))+
  coord_sf(xlim = c(-180, -110), ylim = c(30, 60)) +
  annotation_north_arrow(which_north = "true", location = "tl", 
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = "bl", style = "ticks", line_width = 1.5)+
  theme_classic() +
  theme(
    plot.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "aliceblue",), 
    legend.position = c(0.95,0.75),
    legend.title = element_text(face = "bold", colour = "black", size = 14),
    legend.title.align = 0.5,
    legend.text = element_text(colour = "black", size = 12),
    legend.text.align = 0,
    legend.key = element_rect(fill = "gray100", color = "transparent"),
    legend.background = element_rect(fill = "gray100", color = "black"),
    axis.text = element_text(face = "bold", color = "black", size = 12),
    strip.text = element_text(colour = "black", size = 14, face = "bold"),
    strip.background = element_rect(fill = "#A4D3EC"))
AGExSEX
ggsave("figures/SexAgeTracks.png", AGExSEX, 
       width = 17, height =11 , units = "in", dpi = 600)



