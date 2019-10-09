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
  geom_point(alpha = 0.2) +
  labs(
    title = "Question 1 Plot1",      # main title of figure
    x = "Temp_max (C)",              # x axis title
    y = "Temp_min (C)"   # y axis title
  )   
Qest1_plot1
ggsave("fig/Quest1_plot1.png", plot = Qest1_plot1)

Qest1_plot2 <- ggplot(
  data = Qust1, 
  mapping = aes(x = Temp_max, y = Rainfall, colour = Rainfall)
) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Question 1 Plot2",      # main title of figure
    x = "Temp_max (C)",              # x axis title
    y = "Rainfall (mm)"   # y axis title
  )   
Qest1_plot2
ggsave("fig/Quest1_plot2.png", plot = Qest1_plot2)

Qest1_plot3 <- ggplot(
  data = Qust1, 
  mapping = aes(x = Temp_max, y = Solar_exposure, colour = Solar_exposure)
) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Question 1 Plot3",      # main title of figure
    x = "Temp_max (C)",              # x axis title
    y = "Solar_exposure"   # y axis title
  )   
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
                      aesthetics = "fill") +
  labs(
    title = "Question 2",      # main title of figure
    x = "Temp_max (C)",              # x axis title
    y = "Solar_exposure"   # y axis title
  )   
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

#work out mean rainfall
Meanrainfall <- BOM_clean %>% 
  group_by(Station_number,Month) %>% #group by station
  summarise(mean_rainf = mean(Rainfall))  #work out mean solar for each station
Meanrainfall 

Meanrainfallstate <- inner_join(Meanrainfall,Station_Lon2)
Meanrainfallstate %>%    write.csv("res/Question4.csv")
Meanrainfallstate

Qest4_plot1 <- Meanrainfallstate %>% 
  ggplot(mapping = 
    aes(x = Month, y = mean_rainf, colour = state, group = Station_number)
) +
  geom_line()+
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(
    title = "Question 4",      # main title of figure
    x = "Month",              # x axis title
    y = "mean_rainf (mm)"   # y axis title
  )  

Qest4_plot1 <- Meanrainfallstate %>% 
  ggplot(mapping = 
           aes(x = Month, y = mean_rainf, colour = state, group = Station_number)
  ) +
  geom_line()+
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(
    title = "Question 4",      # main title of figure
    x = "Month",              # x axis title
    y = "mean_rainf (mm)"   # y axis title
  )  +
  facet_wrap( ~ state)

Qest4_plot1
ggsave("fig/Quest4_plot1.png", plot = Qest4_plot1)

