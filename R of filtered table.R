library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)

rm(list = ls())

setwd("~/Desktop/Project-Insect-Carnivore-main/Ladybug-Data-Group-Project/data")

table1 <- read_excel("Ladybug Data.xlsx", .name_repair = "universal") 

table1$plot <- str_sub(table1$plot, 4,5) #extracting the middle two letter 

nameChange <- table1 %>% #changing the names of every plot to it's full form 
  dplyr::mutate(plot = ifelse(plot == "AG", "Agriculture", plot)) %>%
  dplyr::mutate(plot = ifelse(plot == "GF", "Forested", plot)) %>%
  dplyr::mutate(plot = ifelse(plot == "GM", "Mowed Grass", plot)) %>%
  dplyr::mutate(plot = ifelse(plot == "GU", "Unmowed Grass", plot)) %>%
  dplyr::mutate(plot = ifelse(plot == "PR", "Praire", plot)) %>%
  dplyr::mutate(plot = ifelse(plot == "GA", "Garden", plot)) %>%
  dplyr::mutate(plot = ifelse(plot == "IC", "Industrial", plot)) %>%
  
  dplyr::mutate(identifier = ifelse(identifier == "Jack Hughes", "J. Hughes", identifier)) %>%
  dplyr::mutate(identifier = ifelse(identifier == "jack hughes", "J. Hughes", identifier)) %>%
  dplyr::mutate(identifier = ifelse(identifier == "Veronica Cervantes", "V. Cervantes", identifier)) %>%
  dplyr::mutate(identifier = ifelse(identifier == "v. cervantes", "V. Cervantes", identifier)) %>%
  dplyr::mutate(identifier = ifelse(identifier == "Olivia Ruffatto", "O. Ruffatto", identifier)) %>%
  dplyr::mutate(identifier = ifelse(identifier == "Marissa Gorsegner", "M. Gorsegner", identifier)) %>%
  dplyr::mutate(identifier = toupper(identifier)) %>%
  dplyr::mutate(identifier = ifelse(identifier == "O RUFFATTO", "O. RUFFATTO", identifier)) %>%
  
  dplyr::mutate(collector = toupper(collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "JACK HUGHES", "J. HUGHES", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "J HUGHES", "J. HUGHES", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "J HUGEES", "J. HUGHES", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "J. HUGHEES", "J. HUGHES", collector)) %>%
  
  dplyr::mutate(collector = ifelse(collector == "OLIVIA RUFFATTO", "O. RUFFATTO", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "O. RUFFATTTO", "O. RUFFATTO", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "OLIVIARUFFATTO", "O. RUFFATTO", collector)) %>%
  
  dplyr::mutate(collector = ifelse(collector == "VERONICA CERVANTES", "V. CERVANTES", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "V.CERVANTES", "V. CERVANTES", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "V CERVANTES", "V. CERVANTES", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "VERONICA CERVATNES", "V.CERVANTES", collector)) %>%
  
  dplyr::mutate(collector = ifelse(collector == "MARISSA GORSEGNER", "M. GORSEGNER", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "M.GORSEGNER", "M. GORSEGNER", collector)) %>%
  dplyr::mutate(collector = ifelse(collector == "M GORSEGNER", "M. GORSEGNER", collector)) 
 
SpeciesByPlot <- nameChange %>%
  dplyr::count(plot) #counting the sum of every species by plot

ggplot(data = SpeciesByPlot, aes(x = plot, y= n)) + 
  geom_bar(stat = "identity", fill = "steel blue") + 
  geom_text(aes(label =n), vjust = -0.3, size = 3.5) +
  theme_minimal()

speciesPlot <- nameChange %>%
  dplyr::group_by(plot, Species) %>%
  dplyr::summarise(count =n())

specificSpeciesCount <- nameChange %>%
  dplyr::select(Species, plot) %>%
  dplyr::filter(Species == "Coleomegilla maculata") %>%
  dplyr::group_by(Species, plot) %>%
  dplyr::summarise(count = n())

comparePlot <- nameChange %>%
  dplyr::select(Species, plot) %>%
  dplyr::filter(plot == "Mowed Grass") %>%
  dplyr::group_by(plot, Species) %>%
  dplyr::summarise(count = n())

Identifier <- nameChange %>%
  dplyr::count(identifier) 


#GM = mowed grass
#GU = unmowed grass
#AG = agriculture
#IC = I think this is industrial but im not 100%
#PR = prairie
#GA = garden
#GF = forested