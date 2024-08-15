library(lubridate)
library(tidyverse)

#Create a vector of all relevant data files and their paths
transmitted_dat <- dir('data/DSA', 
                       full.names = TRUE, 
                       pattern = ".*csv") %>% 
  read_csv(id = "file") 

#Cleaning up the imported data & adding Dive Number 
#separating out DeployID from age, sex
transmitted_f <- transmitted_dat %>% 
  #group by each individual seal
  group_by(DeployID) %>% 
  #create a column that starts counting at the start of each dive (segment == 0)
  mutate(test1 = case_when(Segment == 0 ~ 1), 
         #creates a column that counts and assigns a segment to a specific dive
         #based on the cumulative sum of the preceding value in the test1
         #counter & ignores NAs
         test2 = cumsum(replace_na(test1, 0)), 
         test3 = ifelse(is.na(Segment), NA, test2)) %>% 
  ungroup() %>% 
  #move those counter columns closer to the segment column to make it easier to
  #look at
  relocate(test1:test3, .after = Segment) %>% 
  #fill in the NA values with the dive number based on the value right below,
  #so it fills the data "up" instead of "down" which is the default
  fill(test3, .direction = "up") %>% 
  #renaming this counter column #commitment
  rename(DiveNum = test3) %>% 
  #deleting the counter columns and the OG DiveNumber column bc in this case it's
  #empty and is confusing to work with
  dplyr::select(-DiveNumber,-test1,-test2) %>%  
  #Separating animalID from age and sex and deleting the messy age_sex field
  separate(DeployID, c("animalid", "Age_Sex"), sep = " - ", remove = TRUE) %>% 
  select(-"Age_Sex") %>% 
  #Which seals were recovered -> survivorship metric
  mutate(Recovered = !grepl("-Offline", file),
         #Making Divestart into a Date-Time object
         DiveStart = parse_date_time(DiveStart, "%H:%M:%S %d-%b-%Y "),
         #Pulling data and time data into seperate columns
         Date = as.Date(DiveStart),
         Time = format(DiveStart, format = "%H:%M:%S"))%>% 
  #Tidying data frame to appear more intuitive
  relocate(Date:Time, .after = DiveStart) %>% 
  relocate(Recovered, .after = animalid) %>% 
  select(-file, -MobilityAvg, -MobilitySD, -Pitch:-TempDeep)

#save as an RDS to retain date-time formatting
saveRDS(transmitted_f, "data/outputs/transmitted_f.RDS")
