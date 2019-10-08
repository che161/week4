#installed.package("tidyverse")
library(tidyverse)
read_csv("data/BOM_data.csv")
BOM <- read_csv("data/BOM_data.csv")

#read_csv("data/BOM_stations.csv",col_names = FALSE)
read_csv("data/BOM_stations.csv")

#Questsion 1
BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% #separate Temp_min_max
  filter(Temp_min != "-", Temp_max != "-", Rainfall >= 0.) %>% #filter data 
  group_by(Station_number) %>% # group with Station_number
  summarise(num_rows = n()) %>% # counting number for each Station_number
  write.csv("res/Question1.csv")

#Questsion 2
BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% #separate Temp_min_max
  filter(Temp_min != "-", Temp_max != "-", Rainfall >= 0.) %>% #filter data 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% #calculate Temp_diff
  group_by(Month) %>%    # group with month
  summarise(mean_tem_diff = mean(Temp_diff)) %>% # calcuate mean temp diff for each month
  filter(mean_tem_diff == min(mean_tem_diff)) %>% # obtain the minmum mean temp diff
  write.csv("res/Question2.csv")

#Questsion 3
#BOM_Stations <- read_csv("data/BOM_stations.csv",col_names = FALSE)
BOM_Stations <- read_csv("data/BOM_stations.csv")
BOM_Stations
BOM_Stations_Tidy <- gather(BOM_Stations, Station_number, val, -info) %>% 
  spread(info, val) %>% # change row to column
  select(Station_number, state) # select Station_number, state column
BOM_Stations_Tidy
# change station number from string to numeric
BOM_Stations_Tidy2 <-   mutate(BOM_Stations_Tidy, station = as.numeric(Station_number)) %>% 
  select(station,state) %>% 
  rename(Station_number = station)

MinTemp <- BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% #separate Temp_min_max
  filter(Temp_min != "-", Temp_max != "-", Rainfall >= 0.) %>% #filter data 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% # calcuate mean temp diff for each month
  group_by(Station_number) %>% # group by station
  summarise(mean_tem_diff = mean(Temp_diff))  # obtain the mean temp diff for each station
MinTemp 

inner_join(MinTemp,BOM_Stations_Tidy2) %>%  # joint tables with total three columns
  filter(mean_tem_diff == min(mean_tem_diff))  %>% # find the station and state with the min temp diff
  write.csv("res/Question3.csv")

#Questsion 4
BOM_Stations <- read_csv("data/BOM_stations.csv")
BOM_Stations
Station_Lon <-   gather(BOM_Stations, Station_number, val, -info) %>% 
  spread(info, val) %>% # change row to column
  select(Station_number, lon) # select Station_number,longitude column
# change station number from string to numeric
Station_Lon2 <-   mutate(Station_Lon, station = as.numeric(Station_number)) %>% 
  select(station,lon) %>% 
  rename(Station_number = station)
Station_Lon2

#work out mean solar
MeanSol <- BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% #separate Temp_min_max
  filter(Solar_exposure != "-") %>% #filter data
  mutate(Solar = as.numeric(Solar_exposure)) %>% # change string to numeric
  group_by(Station_number) %>% #group by station
  summarise(mean_solar = mean(Solar))  #work out mean solar for each station
MeanSol 

inner_join(MeanSol,Station_Lon2) %>% # join two tables
  filter(lon == min(lon) | lon == max(lon)) %>% #work out the eastmost and westmost station
  write.csv("res/Question4.csv")