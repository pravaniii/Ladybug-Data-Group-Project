library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)

rm(list = ls())

setwd("~/Desktop/Project-Insect-Carnivore-main/Ladybug-Data-Group-Project/data")

table1 <- read_excel("Ladybug Data.xlsx", .name_repair = "universal")
