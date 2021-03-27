library(tidyverse)
library(Hmisc)

path1 <- "data/DATA_CAMP_2017_2018/"
path2 <- "data/DATA_CAMP_2018_2019/"
crop_files1 <- list.files(path1, ".GDD", full.names = F)
crop_files2 <- list.files(path2, ".GDD", full.names = F)

#read_lines(paste0(path, crop_files[1]))


aquacrop_params1 <- crop_files1 %>%
  map(~read_lines(paste0(path1, .x), skip = 1) %>%
        str_split_fixed(":", 2) %>% str_trim() %>% 
        matrix(ncol = 2) %>% as.data.frame() %>%
        tibble() %>% setNames(c("value", "param")) %>%
        mutate(value = as.numeric(value)) %>% select(param, value)) %>% 
  set_names(crop_files1) %>% 
  bind_rows(.id = "crop_file") 


aquacrop_params2 <- crop_files2 %>% 
  map(~read_lines(paste0(path2, .x), skip = 1) %>%
        str_split_fixed(":", 2) %>% str_trim() %>% 
        matrix(ncol = 2) %>% as.data.frame() %>%
        tibble() %>% setNames(c("value", "param")) %>%
        mutate(value = as.numeric(value)) %>% select(param, value)) %>%
  set_names(crop_files2) %>% 
  bind_rows(.id = "crop_file") 


aquacrop_params <- bind_rows(aquacrop_params1, aquacrop_params2)

aquacrop_params %>% pivot_wider(names_from = "crop_file") %>% write_csv("params_all.csv")


aquacrop_params$param %>% unique()

aquacrop_params <- aquacrop_params %>% 
  separate(crop_file, c("trial", "cultivar", "set_date"), sep = "_") %>%
  mutate(set_date = str_sub(set_date, 1, 5)) %>%
  separate(set_date, into = c("set", "camp"), "(?<=[A-Z])(?=[0-9])")

### split data into calibration set (cal_set) and evaluation set (eval_set), proportion=0.7
set.seed(1234)
all_params <- aquacrop_params %>% nest(data = c(trial, set, camp, value)) %>% 
  mutate(cal_set = map(data, ~slice_sample(.x, prop = 0.70)),
         eval_set = map2(data, cal_set, ~setdiff(.x, .y)))   

## second test
all_params <- aquacrop_params %>% nest(data = c(trial, camp, value)) %>% filter(set == "C") %>% rename(cal_set = data)  

  
  all_params %>% mutate(avg = map(cal_set, ~bind_rows(smean.cl.boot(.x$value)))) %>% 
  unnest(avg) %>% select(cultivar, param, Mean, Lower, Upper) %>% write_csv("params_calibration_sample70_2_good_fit.csv")
  
params <- all_params %>% mutate(avg = map(cal_set, ~bind_rows(smean.cl.boot(.x$value)))) %>% 
    unnest(avg) %>% select(cultivar, param, Mean, Lower, Upper) #%>% write_csv("params_all.csv")

#all_params %>% filter(str_detect(param, "Base temperature"))
#all_params %>% filter(str_detect(param, "GDDays: from sowing")) %>% View()

crop_def <- read_lines("aquacrop_files/DEFAULT.CRO")

write_crp_aquacrop <- function(out_path = "aquacrop_files/camp_1718/", params, crop_def, crop = "Quinua"){
  
  have_point <- crop_def %>% str_sub(1,15) %>% str_detect(fixed(".")) %>% enframe(name = NULL) %>% slice(-1)
  
  params_to_crp <- params %>% drop_na()  %>%
    mutate(point = have_point$value,
           to_crp = if_else(point == T, sprintf("%1.6f", Mean), sprintf("%1.0f", Mean))) 
  
  cultivar <- unique(params$cultivar)
  
  
  sink(paste0(out_path, crop, cultivar, ".CRO"), append = F) 
  cat(paste(crop, cultivar, "AquaCrop crp  by https://github.com/jrodriguez88"))
  cat('\n')
  write.table(data.frame(params = sprintf("%-15s", params_to_crp$to_crp),
                         space = ":", 
                         name = params_to_crp$param), row.names = F, quote = F, col.names = F)
  sink()
  
  
} 


#write_crp_aquacrop("aquacrop_files/camp_1718/", params, crop_def, "Quinua")

params %>% group_split(cultivar) %>% map(~write_crp_aquacrop("aquacrop_files/camp_1718/", .x, crop_def, "Quinua"))
params %>% group_split(cultivar) %>% map(~write_crp_aquacrop("aquacrop_files/camp_1819//", .x, crop_def, "Quinua"))








