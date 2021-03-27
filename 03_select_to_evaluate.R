#EvalAquacrop


# read yield data 
yield_quinua <- read_excel("data/yield_data.xlsx") %>% 
  separate(crop_file, c("trial", "cultivar", "set_date"), sep = "_") %>%
  mutate(set_date = str_sub(set_date, 1, 5),
         date = as.Date(date)) %>%
  separate(set_date, into = c("set", "camp"), "(?<=[A-Z])(?=[0-9])")

# plot yields
yield_quinua %>% 
  ggplot(aes(cultivar, yield)) + 
  stat_summary(aes(fill = set), geom = "bar",
               fun.data = mean_cl_boot, 
               position = position_dodge(width = 0.8)) +
  facet_grid(trial ~ camp) +
  theme_bw() +
  theme(legend.position="bottom",
        #    legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) + 
  scale_fill_viridis_d(option = "D") +
  labs(title = "Rendimiento ensayos experimentales de Quinua",
       subtitle = "Valores por campaña ~ epoca de siembra",
       x = "Cultivar",
       y =  "Rendimiento (tn/ha)", 
       fill = "Set: ")

# data to evaluate
set.seed(012345)
eval_data <- yield_quinua %>% nest(data = -cultivar) %>% 
  mutate(cal_set = map(data, ~slice_sample(.x, prop = 0.50)),
         eval_set = map2(data, cal_set, ~setdiff(.x, .y)))

eval_data1 <- eval_data %>% select(cultivar, eval_set) %>% 
  unnest(eval_set) %>% 
  mutate(cultivar = paste0("Quinua", cultivar),
         trial = str_extract(trial, "[0-9]+"))

## 2dn test

eval_data <- yield_quinua %>% filter(set == "V") %>% nest(eval_set = -c(cultivar))

eval_data1 <- eval_data %>% select(cultivar, eval_set) %>% 
  unnest(eval_set) %>% 
  mutate(cultivar = paste0("Quinua", cultivar),
         trial = str_extract(trial, "[0-9]+"))

#eval_data1 


data_to_project <- eval_data1 %>% 
  mutate(
    id_name = paste0("TARACO_", camp),
    irri = paste0("R", trial),
    soil = id_name,
    sowing_dates = date,
    clim_data = map(camp, 
                    ~read_csv(paste0("data/clim_data", .x, ".csv")) %>%
                      mutate(date = mdy(date))),
    max_crop_duration = 175,
    aquacrop_files = paste0(getwd(), "/aquacrop_files/camp_", camp, "/"),
    plugin_path = "plugin/") %>%
  select(id_name, sowing_dates, cultivar, soil, irri, clim_data, max_crop_duration, aquacrop_files, plugin_path, set, yield) 






  
  
