#### Analisis de datos climaticos proyecto modelacion QUINUA
# https://github.com/jrodriguez88/quinua_model
# Author: Rodriguez-Espinoza J.
# 2020

## Load packages

library(tidyverse)
library(lubridate)
library(readxl)

## Read Obs data
obs_data <- read_excel("data/Taraco_escenarios_Jeferson/Data observada.xlsx") %>%
  setNames(c("year", "month", "tmax", "tmin", "rain")) %>%
  nest(data = -c(year, month)) %>%
  mutate(data  = map(data, ~.x %>% mutate(day = row_number())))%>%
  unnest(data) %>%
  mutate(date = make_date(year, month, day), prec = rain) #%>% 
#  select(date, tmax, tmin, rain)


plot_clima_hist(obs_data, "Taraco")



         