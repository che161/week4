#installed.package("tidyverse")
library(tidyverse)
read_csv("data/BOM_data.csv")
BOM <- read_csv("data/BOM_data.csv")
BOM
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
BOM_Stations_Tidy2
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

#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#Questsion 1
Qust1 <- BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% #separate Temp_min_max
  filter(Temp_min != "-", Temp_max != "-", Rainfall != "-", Solar_exposure != "-") %>% #filter data 
  filter(Station_number == 9225) %>% # filter with Station_number
  mutate(Temp_min = as.numeric(Temp_min), Temp_max = as.numeric(Temp_max), 
         Rainfall = as.numeric(Rainfall), Solar_exposure = as.numeric(Solar_exposure))

Qust1
summarise(Qust1, maxsol = max(Solar_exposure))
Qust1 %>% write.csv("res/Question1.csv")

# Using ggplot2
Qest1_plot1 <- ggplot(
  data = Qust1, 
  mapping = aes(x = Temp_max, y = Temp_min, colour = Temp_min)
) +
  geom_point(alpha = 0.2) 
Qest1_plot1
ggsave("fig/Quest1_plot1.png", plot = Qest1_plot1)

Qest1_plot2 <- ggplot(
  data = Qust1, 
  mapping = aes(x = Temp_max, y = Rainfall, colour = Rainfall)
) +
  geom_point(alpha = 0.2) 
Qest1_plot2
ggsave("fig/Quest1_plot2.png", plot = Qest1_plot2)

Qest1_plot3 <- ggplot(
  data = Qust1, 
  mapping = aes(x = Temp_max, y = Solar_exposure, colour = Solar_exposure)
) +
  geom_point(alpha = 0.2) 
Qest1_plot3
ggsave("fig/Quest1_plot3.png", plot = Qest1_plot3)


#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#Questsion 2
Qest2_plot1 <- ggplot(
  data = Qust1, 
  mapping = aes(x = Temp_max, y = Solar_exposure, colour = Temp_min, size = Rainfall)
) +
  geom_point(alpha = 0.2) +
  scale_colour_brewer(type = "seq", palette = 2, direction = 1,
                      aesthetics = "fill")
Qest2_plot1
ggsave("fig/Quest2_plot1.png", plot = Qest2_plot1)


#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#Questsion 3
#install.packages("cowplot")
library(cowplot)
# Combining them into one
Qest3_plot1 <- plot_grid(Qest1_plot1, Qest1_plot2, Qest1_plot3, Qest2_plot1)
Qest3_plot1
ggsave("fig/Quest3_plot1.png", plot = Qest3_plot1)

#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#Questsion 4
#install.packages("cowplot")
#library(cowplot)
# Combining them into one
BOM_Stations <- read_csv("data/BOM_stations.csv")
BOM_Stations
Station_Lon <-   gather(BOM_Stations, Station_number, val, -info) %>% 
  spread(info, val) %>% # change row to column
  select(state,Station_number) # select Station_number,longitude column
Station_Lon
# change station number from string to numeric
Station_Lon2 <-   mutate(Station_Lon, Station_number = as.numeric(Station_number))
Station_Lon2

BOM_clean <- BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% #separate Temp_min_max
  filter(Temp_min != "-", Temp_max != "-", Rainfall != "-", Solar_exposure != "-") %>% 
  mutate(Temp_min = as.numeric(Temp_min), Temp_max = as.numeric(Temp_max), 
         Rainfall = as.numeric(Rainfall), Solar_exposure = as.numeric(Solar_exposure))
BOM_clean

#work out mean solar
Meanrainfall <- BOM_clean %>% 
  group_by(Station_number,Month) %>% #group by station
  summarise(mean_rainf = mean(Rainfall))  #work out mean solar for each station
Meanrainfall 

Meanrainfallstate <- inner_join(Meanrainfall,Station_Lon2)
Meanrainfallstate %>%    write.csv("res/Question4.csv")
Meanrainfallstate

Qest4_plot1 <- ggplot(
  data = Meanrainfallstate, 
  mapping = aes(x = Month, y = mean_rainf, colour = state)
) +
  geom_line()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))

Qest4_plot1
ggsave("fig/Quest4_plot1.png", plot = Qest4_plot1)

