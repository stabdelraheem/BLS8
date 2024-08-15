library(lubridate)
library(tidyverse)
library(hms)

transmitted_f <- read_rds("data/outputs/transmitted_f.RDS")

#Getting maximum dive depth, no minimum because that's just 0, and total dive
#duration for every dive in minutes
diveStat <- transmitted_f %>% 
  group_by(animalid, DiveNum) %>% 
  summarise(
    #The following values repeat for each dive segment in the raw data, so grabbing the first observation for each piece of data
    #Was the tag recovered
    Recovered = first(Recovered),
    #Date-time of dive start per dive per seal
    DateTime = first(DiveStart),
    Date = first(Date),
    Time = first(Time),
    #Location quality per dive per seal
    LocQual = first(LocationQuality),
    #Latitude per dive per seal
    Lat = first(Latitude),
    #Longitude per dive per seal
    Long = first(Longitude),
    #select the first row of a dive that has the total duration in it, and does
    #not have a segment assignment (hence, segment is.na) or you can do it as
    #Duration[1]
    #Dive duration divided by 60 to get minutes
    DiveDur_min = round(Duration[is.na(Segment)]/60, digits = 2), 
    #Maximum depth recorded during a dive
    MaxDiveDepth = max(InitialDepth, na.rm = TRUE), 
    #Average depth of a dive - ignore?
    MeanDiveDepth = mean(InitialDepth, na.rm = TRUE), 
    #Median depth of a dive - ignore?
    MedDiveDepth = median(InitialDepth, na.rm = TRUE),
    #Total prey capture events per dive for each seal
    TotalPreyCap = sum(ActivityCount, na.rm = TRUE),
    #Total swim effort per dive for each seal
    TotalSwimEffort = sum(SwimEffort, na.rm = TRUE),
    #Average swim effort per dive for each seal
    AvgSwimEffort = mean(SwimEffort, na.rm = TRUE),
    #Maximum swim effort per dive for each seal
    MaxSwimEffort = max(SwimEffort, na.rm = TRUE),
    #Grabbing the Surface Interval information for each dive
    SurfaceInterval = first(SurfaceInterval)) %>% 
  ungroup() %>% 
  relocate(Recovered, .after = animalid) %>% 
  mutate(Time = as_hms(Time))

#Great! Now let's add some age and sex data to the dive data to make things interesting
#Let's use the trip summary we made in the previous step and use the "animalid" column to selectively join age and sex data to the dive data
trip_Summary <- read_rds("data/outputs/trip_Summary.RDS")

diveStat_f <- left_join(diveStat, 
                        select(trip_Summary, animalid, age_class, Sex, TOPPID), 
                        by = "animalid") %>% 
  relocate(age_class:Sex, .after = animalid) %>% 
  relocate(TOPPID, .before = animalid) %>% 
  rename(Age = age_class) %>% 
  #Make a row for 2023229 (had no transmissions so does not have a DSA file)
  add_row(TOPPID = 2023229,
          animalid = "H049", 
          Age = 3, 
          Sex = "M", 
          Recovered = FALSE) %>% 
  arrange(TOPPID)

#Save output as RDS to retain date-time format
saveRDS(diveStat_f, "data/outputs/diveStat.RDS")

#Let's summarize distance from colony per dive each seal took 
library(geosphere)

perDive_stats <- read_rds("data/outputs/diveStat.RDS")

Ano <- c(-122.33, 37.11)

#Calculate the distance of each dive from Año Nuevo per seal.
#Divide by 1000 to get distance values in km not meters (default).
distColony_perDive <- perDive_stats %>% 
  mutate(Dist_Colony = round(distVincentySphere(Ano, cbind(Long,Lat))/1000, 
                             digits = 2)) %>% 
  relocate(Dist_Colony, .after = Long) 

#Save output as RDS 
saveRDS(distColony_perDive, "data/outputs/diveStat_distance.RDS")