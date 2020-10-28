##### Thomas Fire Progression Data #####
# Christina Jimenez
# 10/20/2020

#### Load Packages ####

library(tidyverse)
library(dplyr)
library(readxl)
library(praise)
library(lubridate)

#### Load Dataset ####

excel_sheets("Input_Data/week1/Thomas_Fire_Progression.xlsx")

data <- read_excel("Input_Data/week1/Thomas_Fire_Progression.xlsx", sheet= "Data")


#### Inital Data Exploration ####

names(data)
# Date, Acres_Burned, Containment, PM10, PM25
dim(data)
class(data)
head(data)
tail(data)
str(data)
glimpse(data)
typeof(data$Acres_Burned)
max(data$Acres_Burned)


summary(data)


#### Piping ####

thomas.fire <- data %>% 
  arrange(desc(Acres_Burned)) %>%  
  mutate_at(vars("PM10"), replace_na, 0)

#### ggplot ####

ggplot(thomas.fire, aes(x = Date, y = Acres_Burned)) + 
  geom_point(aes(color = "")) +
  geom_line(color = "black") +
  ggtitle("Tomas Fire \n 12/05/17 - 01/12/18") +
  labs(x ="", y = "Acres Burned", color = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  
saveRDS(thomas.fire, file = "Output_Data/week1/Thomas_Fire_Data.rds")
write_csv(thomas.fire, "Output_Data/week1/Thomas_Fire_Data.csv")

ggsave(filename = "Thomas_Fire", thomas.fire, device = "jpeg", "Output_Data/week1/")  
  
  
  
  
  