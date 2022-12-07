library(tidyverse)
library(shiny)
library(party)
library(magrittr)
rm(list = ls())

df_clean_butterfly <- read.csv("Data/Clean/Butterfly.csv",) %>%
  mutate(date = as.Date(date))

options(scipen = 999)
