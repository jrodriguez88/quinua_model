## . Lectura de resultados
#path_op <- paste0(plugin_path, "/OUTP/")
library(tidyverse)
library(data.table)
library(Hmisc)
library(lubridate)
source("D:/03_DEVELOPER/aquacrop-R/read_outputs_aquacrop.R")

season_files <- list.dirs(full.names = T) %>% str_subset("plugin") %>% str_subset("OUTP") %>% 
  list.files(pattern = "season", full.names = T)

saf_read_season <- possibly(read_aquacrop_season, NULL)


#estructura del archivo PRM
file_str <- c("id_name", "cultivar", "soil", "id2", "crop_sys")

## REad _p seanson and daily
season_data <- map(season_files,  ~saf_read_season(.x, ""))



id_escenarios <- map(list.files(pattern = "id_escenarioscc_"),  read_csv) %>%  bind_rows()


quinua_final_db <- season_data %>% bind_rows() %>% arrange(Year1) %>% 
  mutate(File = str_replace(File, ".PRM", "")) %>%
  separate(File, file_str, sep = "_") %>% drop_na() %>%
  mutate(crop_sys =  if_else(str_detect(crop_sys, "IRR"), "Riego", "Secano"),
         cultivar = str_remove(cultivar, "CRO")) %>% left_join(id_escenarios) %>% 
  mutate(date = make_date(Year1, Month1, Day1), yday = yday(date)) %>%
  mutate(Periodo = case_when(date <= make_date(2009, 12, 31) ~ "Ref_1980_2009",
                             date >= make_date(2010, 1,1) & date <= make_date(2039, 12, 31) ~ "Cer_2010_2039",
                             date >= make_date(2040, 1,1) & date <= make_date(2069, 12, 31) ~ "Med_2040_2069",
                             date >= make_date(2070, 1,1) & date <= make_date(2099, 12, 31) ~ "Lej_2070_2099"),
         cultivar = case_when(cultivar=="QuinuaBJ" ~ "Blanca de Juli",
                              cultivar=="QuinuaPAS" ~ "Pasankalla",
                              cultivar=="QuinuaSI" ~ "Salcedo INIA"),
         Periodo = factor(Periodo, levels = c("Ref_1980_2009","Cer_2010_2039", "Med_2040_2069", "Lej_2070_2099")))
         

write_csv(quinua_final_db, "quinua_simulaciones_db2021.csv")


b <- quinua_final_db %>% nest(-soil) %>% mutate(plots = map2(soil, data,
  ~ggplot(data = .y, aes(Periodo,  Yield, fill= gcm)) + 
  geom_boxplot(outlier.shape=NA) +
  facet_grid(cultivar+crop_sys ~ rcp , scales = "free") +
  labs(title = paste0("Simulaciones de Quinua con Aquacrop: ", .x),
       x= "Periodo",
       y= "Rendimiento (T/ha)") +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="bottom",
    #    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))))

c <- quinua_final_db %>% nest(-soil) %>% mutate(plots = map2(soil, data,
  ~ggplot(data = .y, aes(Year1,  Yield, fill= gcm, color=gcm, linetype=rcp)) + 
  geom_smooth(alpha=0.3) +
  labs(title = paste0("Simulaciones del cultivo de Quinua con Aquacrop: ", .x),
       x= "Año",
       y= "Rendimiento (T/ha)",
       fill = "GCM: ", color = "GCM: ",
       linetype = "RCP: ") +
  facet_grid(cultivar~crop_sys, scales = "free") +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="bottom",
    #    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))))


summary_data <- quinua_final_db %>% tibble %>% nest(-c(soil, cultivar, gcm, rcp, crop_sys, Periodo)) %>% 
  mutate(avg = map(data, ~bind_rows(smean.cl.boot(.x$Yield)))) %>% 
  unnest(avg) %>% 
  mutate(sd = map_dbl(data, ~sd(.x$Yield)), cv = (sd/Mean)*100) %>% select(-data) %>% 
  write_csv("summary_final_ci.csv")
  
  

summary_data %>% select(cultivar:Mean) %>% filter(rcp!="Base") %>% #select(-rcp) %>% 
  left_join(
  summary_data %>% select(cultivar:Mean) %>% filter(rcp=="Base") %>%
    rename(Reference = Mean) %>% select(-c(gcm, rcp, Periodo)), by = c("soil", "cultivar", "crop_sys")) %>% 
  mutate(diff = ((Reference - Mean)/Reference)*-100,
         Periodo = factor(Periodo, levels = c("Cer_2010_2039", "Med_2040_2069", "Lej_2070_2099"))) %>% 
  filter(crop_sys == "Riego") %>%
  ggplot(aes(gcm, diff, fill=gcm)) +
  geom_bar(stat="identity", alpha=0.8) +
  geom_hline(yintercept = 0) +
  facet_grid(soil+cultivar ~ Periodo+rcp ) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
    legend.position="bottom",
        legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))  +
  labs(title = "Diferencia* de Rendimiento simulado entre modelos climaticos",
       subtitle = "Escenario de Riego - *con referencia a periodo de referencia 1980:2009",
       x= "GCM: ",
       y= "Diferencia(%)")

summary_data %>% select(cultivar:Mean) %>% filter(rcp!="Base") %>% left_join(
  summary_data %>% select(cultivar:Mean) %>% filter(rcp=="Base") %>%
    rename(Reference = Mean) %>% select(-c(gcm, rcp, soil, Periodo)), by = c("cultivar", "crop_sys")) %>% 
  mutate(diff = ((Reference - Mean)/Reference)*-100,
         Periodo = factor(Periodo, levels = c("Cer_2010_2039", "Med_2040_2069", "Lej_2070_2099"))) %>% 
  filter(crop_sys == "Secano") %>%
  ggplot(aes(gcm, diff, fill=gcm)) +
  geom_bar(stat="identity", alpha=0.8) +
  geom_hline(yintercept = 0) +
  facet_grid(soil+cultivar ~ Periodo+rcp ) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold"))  +
  labs(title = "Diferencia* de Rendimiento simulado entre modelos climaticos",
       subtitle = "Escenario de Secano - *con referencia a periodo de referencia 1980:2009",
       x= "GCM: ",
       y= "Diferencia(%)")
  


### escenarios variabilidad


##Enso data
enso <- download_enso() %>% select(Year, Month, ONI, phase) %>%
  mutate(phase = case_when(ONI > 0.5 ~ "El Niño",
                           ONI < -0.5 ~ "La Niña",
                           TRUE ~ "Neutral"))

clim_var_data <- quinua_final_db %>% filter(Periodo == "Ref_1980_2009") %>%
  mutate(Month = lubridate::month(date, label=T)) %>%
  rename(Year = Year1) %>%
  left_join(enso, by= c("Year", "Month")) %>%
  mutate(Month_vg = month((date + days(25)), label = T),
         Month_fl = month((date + days(45)), label = T),
         Month_rp = month((date + days(65)), label = T)) %>% 
  rename(ONI_em = ONI, enso_em =  phase) %>%
  left_join(enso, by= c("Year", "Month_vg" = "Month")) %>%
  rename(ONI_vg = ONI, enso_vg =  phase) %>%
  left_join(enso, by= c("Year", "Month_fl" = "Month")) %>%
  rename(ONI_fl = ONI, enso_fl =  phase) %>%
  left_join(enso, by= c("Year", "Month_rp" = "Month")) %>%
  rename(ONI_rp = ONI, enso_rp =  phase)





## Analisis de variabilidad climatica



clim_var_data %>%
  #    filter(soil=="Clay_Loam") %>% #, cultivar!="F2000") %>% 
  #    ggplot(aes(Month, WRR14)) +
  ggplot(aes(as.Date(yday, origin = "2016-01-01"), Yield)) +
  #    geom_jitter(aes(shape=LOC), alpha=0.3) + 
  stat_summary(fun.data = mean_cl_boot,
               position = position_dodge(width = 0.5),
               aes(color= enso_fl), alpha = 0.7) + 
  facet_grid(crop_sys ~ soil+cultivar, scales = "free") +
  #    scale_x_date(date_labels = "%j") +
  #    facet_grid(soil ~ cultivar) +
  theme_classic() + theme(#legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.major.y = element_line( size=.1, color="grey" )) + 
  scale_color_manual(values = c("red", "dodgerblue4", "limegreen")) +
  labs(x = "Fecha de Siembra",
       y = "Rendimiento (Tn/ha) - Peso seco",
       color = "ENSO: ") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



clim_var_data %>%
  ggplot(aes(factor(Year), Yield)) +
  #    geom_jitter(aes(shape=LOC), alpha=0.3) + 
  geom_boxplot(aes(fill= enso_fl), outlier.shape = NA, alpha=0.8) + 
  #    scale_x_date(date_labels = "%j") +
  #    facet_grid(soil ~ cultivar) +
  theme_classic() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major.y = element_line( size=.1, color="grey" )) +
  scale_fill_manual(values = c("red", "dodgerblue4", "limegreen")) +
  facet_grid(Region ~ ., scales = "free") +
  labs(x = "Year \n \n ENSO",
       y = "Rendimiento (Quintal/Mz)", 
       title = "Analisis de Variabilidad Climatica")












