
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)
library(ggplot2)
#library(maps) #for map data
#library(ggmap) #for mapping points on map
#library(ggthemes) #for more themes (including theme_map())

rm(list = ls())

setwd("~/Desktop/Project-Insect-Carnivore-main/Ladybug-Data-Group-Project/data")

df_sheet1 <- read_csv("Scan Ladybug Data.csv")

table1 <- df_sheet1 %>%
  select(kingdom, phylum, class, order, phylum, scientificName,recordedBy, year) %>%
  arrange(year, desc(year))

distance <- df_sheet1 %>%
  select(scientificName,year, stateProvince) %>% #selecting the columns needed
  dplyr::mutate(stateProvince = toupper(stateProvince)) %>% #changing everything to uppercase 
  arrange(year, desc(year)) %>%
  dplyr::mutate(stateProvince = ifelse(stateProvince == "IL", "ILLINOIS", stateProvince)) %>% #making all the state names coherent
  dplyr::mutate(stateProvince = ifelse(stateProvince == "IA", "IOWA", stateProvince))

recordByState <- distance %>%
  count(stateProvince)

recordByState$n <- as.numeric(recordByState$n)
#recordByState$stateProvince <- as.numeric(recordByState$stateProvince)

#hist(recordByState$n) #can't make histogram
ggplot(data = recordByState, aes(x=stateProvince, y=n)) +  #bar graph
  geom_bar(stat="identity", fill="steelblue")
yearCount <- df_sheet1 %>%
  dplyr::group_by(scientificName, year)
  
yearCount$year <- str_sub(yearCount$year, 1,3)

yearCount$year <- paste0(yearCount$year, "0's")

speciesByDecade <- yearCount %>%
  dplyr::summarise(count = n()) #creating a pivot table of the insect with the year found and counting the number of insect found each year 


recordByPerson <- df_sheet1 %>%
  dplyr::group_by(recordedBy,year) %>%
  dplyr::summarise(count = n())





 

  

  
 


  
  







