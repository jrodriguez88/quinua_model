#### Analisis de datos climaticos proyecto modelacion QUINUA
# https://github.com/jrodriguez88/quinua_model
# Author: Rodriguez-Espinoza J.
# 2020

## Load packages

library(tidyverse)
library(lubridate)
library(readxl)

source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_weather_aquacrop.R", encoding = "UTF-8")


## Read Obxs data
taraco_wth_ref <- read_excel("data/Taraco_Puno_escenarios_homo/Data observada.xlsx") %>%
  setNames(c("year", "month", "tmax", "tmin", "rain")) %>%
  nest(data = -c(year, month)) %>%
  mutate(data  = map(data, ~.x %>% mutate(day = row_number())),
         data = map(data, ~.x %>% mutate_all(as.numeric))) %>%
  unnest(data) %>%
  mutate(date = make_date(year, month, day), prec = rain,
         localidad = "Taraco", 
         gcm = "Obs", 
         periodo = "Referencia",
         rcp = "Base") %>% 
  select(localidad, gcm, periodo, rcp, date, year, month, day, tmax, tmin, prec) %>% 
  filter(year != 2010)

lampa_wth_ref <- read_excel("data/Lampa_Puno_escenarios/Data observada_80-10.xlsx") %>%
  setNames(c("year", "month", "tmax", "tmin", "rain")) %>%
  nest(data = -c(year, month)) %>%
  mutate(data  = map(data, ~.x %>% mutate(day = row_number())),
data = map(data, ~.x %>% mutate_all(as.numeric))) %>%
  unnest(data) %>%
  mutate(date = make_date(year, month, day), prec = rain,
         localidad = "Lampa", 
         gcm = "Obs", 
         periodo = "Referencia",
         rcp = "Base") %>% 
  select(localidad, gcm, periodo, rcp, date, year, month, day, tmax, tmin, prec) %>% 
  filter(year != 2010)


plot_weather_series(lampa_wth_ref, "Lampa")
plot_weather_series(taraco_wth_ref, "Taraco")


## load GCM
dir_data <- list.dirs(path = paste0(getwd(), "/data/")) %>% str_subset("Periodo")
files_data <- map(dir_data, list.files, full.names = T) %>% as_vector() %>% str_subset("boxplot", negate = T)


read_excel_gcm <- function(file, header = "YYYY"){
  
  skip_by_xls <- file %>% read_excel(range = "A1:A8:") %>% 
    pull(1) %>% str_which(pattern = header) 
  
  read_excel(file, skip = skip_by_xls)
  
}

## read Raw Data 
gcm_raw_data <- files_data %>% map(read_excel_gcm) %>% 
  map(~.x %>% mutate_all(as.numeric)) %>% 
  set_names(files_data) %>% bind_rows(.id = "file")

## GCM data
gcm_data <- gcm_raw_data %>% 
  separate(file, into = c("v1", "v2"), sep = "//") %>%
  separate(v2, into = c("localidad", "gcm", "periodo", "rcp"), sep = "/") %>% 
  mutate(localidad = str_extract(localidad, pattern = "[^_]+"), 
         gcm = str_remove(gcm, "Modelo_"),
         periodo = str_remove(periodo, "Periodo "),
         rcp = str_remove(rcp, ".xlsx"),
         date = make_date(YYYY, MM, DD)) %>%
  rename(year = YYYY, month = MM, day = DD, tmax = TMAX, tmin = TMIN, prec = RAIN) %>% 
  select(localidad:rcp, date, year:prec)


#Set Names and labels  
var_name = c("rain", "prec", "srad", "tmin", "tmax", "rhum", "wvel")
var_label = paste(var_name, c('(mm)', '(mm)', '(MJ/m²d)', '(°C)', '(°C)', '(%)', '(m/s)'))
names(var_label) <- var_name

## Grafico lineas
bind_rows(gcm_data, lampa_wth_ref, taraco_wth_ref) %>% group_by(localidad, gcm, rcp, year, month) %>%
  summarise(tmax = mean(tmax, na.rm = T), tmin = mean(tmin, na.rm = T), prec = sum(prec)) %>%
  group_by(localidad, gcm, rcp, year) %>%
  summarise(tmax = median(tmax, na.rm = T), tmin = median(tmin, na.rm = T), prec = sum(prec)) %>% 
  pivot_longer(cols = tmax:prec, names_to = "var") %>% 
  ggplot(aes(year, value)) +
  geom_line(aes(color = gcm, linetype = rcp ), cex = 0.8) +
  facet_grid(var ~ localidad, scales = "free", labeller = labeller(var = var_label)) +
  labs(title = "Zona de Estudio: Climatologia y GCM",
       subtitle = "Valores Anuales",
       x = "",
       y =  NULL, 
       color = "Data: ",
       linetype = "Escenario: ") +
  theme_bw() +
  theme(legend.position="bottom",
        #    legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) + 
  scale_color_viridis_d(option = "D")

# Por Localidad y Decada
a <- bind_rows(gcm_data, lampa_wth_ref,taraco_wth_ref) %>% mutate(decade = floor(year/10)*10) %>% 
  group_by(localidad, gcm, rcp, periodo, decade, year) %>% 
  summarise(tmax = median(tmax, na.rm = T), tmin = median(tmin, na.rm = T), prec = sum(prec)) %>%
  pivot_longer(cols = tmax:prec, names_to = "var") %>% nest(-localidad) %>% #filter(localidad=="Lampa") %>%
#  ggplot(aes(factor(decade), value)) +
  mutate(plot = map2(localidad, data, ~.y %>% ggplot(aes(factor(decade), value)) + 
#  stat_summary(aes(color = gcm), fun.data = mean_cl_boot) + 
#  geom_line(aes(color = gcm, linetype = rcp ), cex = 0.8) +
  geom_boxplot(aes(fill = gcm), alpha = 0.8) + #, group = interaction(gcm, decade))) +
  facet_grid(var ~ rcp, scales = "free", labeller = labeller(var = var_label)) +
  labs(title = paste0(.x, ": Climatologia y GCM"),
       subtitle = "Boxplot Decadal",
       x = "Decada",
       y =  NULL, 
       fill = "Data: ") + 
  theme_bw() + scale_fill_viridis_d(option = "D") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom",
        #    legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold"))))


a$plot[[2]]

# Por periodo
bind_rows(gcm_data, taraco_wth_ref, lampa_wth_ref) %>% 
  group_by(localidad, gcm, rcp, periodo, year) %>% 
  summarise(tmax = median(tmax, na.rm = T), tmin = median(tmin, na.rm = T), prec = sum(prec)) %>%
  pivot_longer(cols = tmax:prec, names_to = "var") %>% 
  mutate(periodo = factor(periodo, c("Referencia", "Cercano", "Medio", "Lejano"))) %>%
  ggplot(aes(periodo, value)) + 
                       #  stat_summary(aes(color = gcm), fun.data = mean_cl_boot) + 
                       #  geom_line(aes(color = gcm, linetype = rcp ), cex = 0.8) +
                       geom_boxplot(aes(fill = gcm), alpha = 0.8) + #, group = interaction(gcm, decade))) +
                       facet_grid(var ~ localidad + rcp, scales = "free", labeller = labeller(var = var_label)) +
                       labs(title = paste0("Zona de Estudio: Climatologia y GCM"),
                            subtitle = "Boxplot por periodo",
                            x = "",
                            y =  NULL, 
                            fill = "Data: ") + 
                       theme_bw() + scale_fill_viridis_d(option = "D") +
                       theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                             legend.position="bottom",
                             #    legend.title = element_blank(),
                             panel.grid.minor = element_blank(),
                             strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
                             strip.text = element_text(face = "bold"))


## Convertir a formato Aquacrop

#taraco_lat = -15.312
#taraco_lon = -69.72
#
#lampa_lat = -15.673
#lampa_lon = -70.372


weather_data <- bind_rows(gcm_data, lampa_wth_ref) %>% 
  select(-c(year:day), -periodo) %>% rename(rain = prec) %>% #pull(date) %>% is.na() %>% sum
  drop_na() %>%
  nest(data = date:rain) %>% 
  mutate(id = paste0("clim", 8:14),
         lat = if_else(localidad == "Lampa", -15.673, -15.312),
         lon = if_else(localidad == "Lampa", -70.372, -69.72),
         alt = 3830,
         co2_f = case_when(rcp=="Base"~ "MaunaLoa.CO2",
                           rcp=="RCP_4.5" ~ "RCP4-5.CO2",
                           rcp=="RCP_8.5" ~"RCP8-5.CO2"))


weather_data  %>%
  mutate(to_aquacrop = pmap(list(x = id,
                                 y = data,
                                 z = lat, 
                                 k = alt, 
                                 m = co2_f),
                            function(x,y,z,k,m) make_weather_aquacrop(path = "aquacrop_files/", 
                                                     id_name = x, 
                                                     wth_data = y, 
                                                     lat = z,
                                                     alt = k,
                                                     co2_file = m))) %>% pull(to_aquacrop)
   



save(weather_data, file = "weather_data_final_2021.RData")                                                        









         