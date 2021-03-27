
library(data.table)
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/read_outputs_aquacrop.R", encoding = "UTF-8")

#aquacrop_files <- "D:/03_DEVELOPER/quinua_model/aquacrop_files/camp_1718/"
#id_name <- "TARACO_1718"
#cultivar <- "QuinuaBJ"
#irri <- "R1"
#soil <- "TARACO_1718"
#sowing_dates <- ymd("2017-10-25")
#clim_data <- read_csv("data/clim_data1718.csv") %>% mutate(date = mdy(date))
#max_crop_duration = 170
#plugin_path <- "plugin/"
#set <- "C"
unlink("plugin/OUTP/*")
unlink("plugin/LIST/*")

make_project_by_date <- function(id_name, sowing_dates, cultivar, soil, irri, clim_data, max_crop_duration = 150, aquacrop_files, plugin_path, set){
  
  ## Create sowing dates vector, use when requiere 1 date 
  #    sowing_dates  <- c(sowing_date - (5:1), sowing_date + (0:4))
  ##### add function // eval inputs    
  ### load aquacrop files
  clim_file <- list.files(aquacrop_files, pattern = paste0(id_name, ".CLI")) %>% str_remove(".CLI")
  co2_file <-  list.files(aquacrop_files, ".CO2")
  crop_file <- list.files(aquacrop_files, pattern = paste0(cultivar, ".CRO"))
  irri_file <- if(length(list.files(aquacrop_files, paste0(irri, ".IRR"))) == 0){"rainfed"} else {list.files(aquacrop_files, paste0(irri, ".IRR"))}
  man_file <-  if(length(list.files(aquacrop_files, ".MAN")) == 0){"none"} else {list.files(aquacrop_files, ".MAN")}
  soil_file <- list.files(aquacrop_files, paste0(soil, ".SOL"))
  ini_file <-  list.files(aquacrop_files, ".SW0")
  proj_file <- list.files(aquacrop_files, ".PRM")
  
  ### Default parameters,  
  def_params <- read_lines(paste0(aquacrop_files, proj_file), skip = 6, n_max = 21) 
  
  
  ### Create multiple combinations of params
  params <- expand.grid(aquacrop_files,
                        clim_file,
                        co2_file,
                        crop_file,
                        irri_file, 
                        man_file,
                        soil_file,
                        ini_file,
                        max_crop_duration,
                        sowing_dates) %>% 
    as_tibble() %>%
    setNames(c("aquacrop_files",
               "clim_file",
               "co2_file",
               "crop_file",
               "irri_file", 
               "man_file",
               "soil_file",
               "ini_file",
               "max_crop_duration",
               "sowing_date"))
  
  
  ## Function to calculate and create crop growing cycles
  cal_cycles_project <- function(clim_data,
                                 aquacrop_files,
                                 clim_file,
                                 co2_file,
                                 crop_file,
                                 irri_file, 
                                 man_file,
                                 soil_file,
                                 ini_file,
                                 max_crop_duration,
                                 sowing_date) {
    
    # path files
    path_files <- aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\")
    
    ### extract "GDDays: from sowing to maturity" from CRO_file
    gdd_mt <- read_lines(file = paste0(aquacrop_files, crop_file)) %>%
      str_subset("GDDays: from sowing to maturity|GDDays: from transplanting to maturity") %>% 
      str_extract("[0-9]+") %>% as.numeric
    
    ### extract Base temperature 
    tbase <- read_lines(file = paste0(aquacrop_files, crop_file)) %>%
      str_subset("Base temperature") %>% 
      str_extract("[0-9]+") %>% as.numeric
    
    #    max_crop_duration <- gdd_mt / clim_data %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>% summarise(median(HUH)) %>% pull(1)
    
    # calculate crop duration 
    crop_duration <- clim_data %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>%
      dplyr::filter(date >= sowing_date,
                    date <= sowing_date + max_crop_duration - 1) %>%
      mutate(sum_gdd = cumsum(HUH)) %>%
      dplyr::filter(sum_gdd<= gdd_mt) %>% 
      count() %>% pull(n)
    
    # Calculate numeric dates
    first_day <- as.numeric(sowing_date - make_date(1900, 12, 31))
    last_day <- first_day + crop_duration
    mat_date <- as.Date(last_day, origin = make_date(1900, 12, 31))
    
    #Write grow cycles
    path_data <- function(){
      
      cat(paste0(first_day, "    : First day of simulation period - ", format(sowing_date, "%d %b %Y")))
      cat('\n')
      cat(paste0(last_day,  "    : Last day of simulation period - ",  format(mat_date, "%d %b %Y")))
      cat('\n')
      cat(paste0(first_day, "    : First day of cropping period - " , format(sowing_date, "%d %b %Y")))
      cat('\n')
      cat(paste0(last_day,  "    : Last day of cropping period - "  , format(mat_date, "%d %b %Y")))
      cat('\n')    
      cat("-- 1. Climate (CLI) file", sep = '\n')
      cat(paste0(clim_file, ".CLI"), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("1.1 Temperature (TMP) file", sep = '\n')
      cat(paste0(clim_file, ".Tnx"), sep = '\n') 
      cat(paste0(path_files), sep = '\n')
      cat("1.2 Reference ET (ETo) file", sep = '\n')
      cat(paste0(clim_file, ".ETo"), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("1.3 Rain (PLU) file", sep = '\n')
      cat(paste0(clim_file, ".PLU"), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("1.4 Atmospheric CO2 (CO2) file", sep = '\n')
      cat(paste(co2_file), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("-- 2. Crop (CRO) file", sep = '\n')
      cat(paste(crop_file), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("-- 3. Irrigation (IRR) file", sep = '\n')
      if(irri_file=="rainfed"){
        cat("(None)", sep = '\n')
        cat("(None)", sep = '\n')
      } else {
        cat(paste(irri_file), sep = '\n')
        cat(paste0(path_files), sep = '\n')
      }
      cat("-- 4. Management (MAN) file", sep = '\n')
      if(man_file == "none"){
        cat("(None)", sep = '\n')
        cat("(None)", sep = '\n')
      } else {
        cat(paste(man_file), sep = '\n')
        cat(paste0(path_files), sep = '\n')
      }
      cat("-- 5. Soil profile (SOL) file", sep = '\n')
      cat(paste(soil_file), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("-- 6. Groundwater (GWT) file", sep = '\n')
      cat("(None)", sep = '\n')
      cat("(None)", sep = '\n')
      cat("-- 7. Initial conditions (SW0) file", sep = '\n')
      cat(paste(ini_file), sep = '\n')
      cat(paste0(path_files), sep = '\n')
      cat("-- 8. Off-season conditions (OFF) file", sep = '\n')
      cat("(None)", sep = '\n')
      cat("(None)", sep = '\n')
    }
    
    list(capture.output(path_data()))
    
  }
  
  
  ## Function to compute all runs for params table
  runs_cal <- function(params, clim_data) {
    
    params %>% mutate(runs = cal_cycles_project(clim_data, 
                                                aquacrop_files,
                                                clim_file,
                                                co2_file,
                                                crop_file,
                                                irri_file, 
                                                man_file,
                                                soil_file,
                                                ini_file,
                                                max_crop_duration,
                                                as.Date(sowing_date))) 
    
  }
  
  sim_cycles <- split(params, 1:nrow(params)) %>% 
    map(., ~runs_cal(., clim_data)) %>%
    bind_rows() 
  
  
  ## Write PRM files
  write_projects <- function(sim_cycles, path, def_params, soil){
    
    #    description <-  paste(unique(sim_cycles$crop_file), 
    #                       unique(sim_cycles$clim_file),
    #                       unique(sim_cycles$soil_file),
    #                       unique(sim_cycles$irri_file), sep = " - ")
    
    prm_name <- paste0(unique(sim_cycles$clim_file), "_",
                       unique(sim_cycles$crop_file), "_",
                       soil, "_",
                       unique(sim_cycles$irri_file), set) %>% 
      str_replace_all(pattern = "[.]+", replacement = "") %>%
      paste0(., ".PRM")
    
    suppressWarnings(dir.create(paste0(path, "/", "LIST")))
    
    sink(file = paste(path, "LIST", prm_name, sep = "/"), append = F)
    cat(paste("by https://github.com/jrodriguez88"))
    cat('\n')
    cat("6.0       : AquaCrop Version (March 2017)")
    cat('\n')
    writeLines(sim_cycles$runs[[1]][1:4])
    writeLines(def_params)
    writeLines(sim_cycles$runs[[1]][-c(1:4)])
    walk(.x=sim_cycles$runs[-1], ~writeLines(.x))
    sink()    
    
  }
  
  map(.x = split(sim_cycles, 
                 list(sim_cycles$crop_file, 
                      sim_cycles$irri_file, 
                      sim_cycles$soil_file)),
      ~write_projects(.x, plugin_path, def_params, soil))
  
  #    toc()
  #25.57 sec elapsed by 1 crop, 
}

# convert to projects
data_to_project %>% select(-yield) %>%
  as.list() %>% pmap(make_project_by_date)

# Run Aquacrop
system("plugin/ACsaV60.exe")

#Read outputs

season_files <- list.files("plugin/OUTP/", pattern = "season") #%>% str_subset("Frijol")
file_str <- c("clima", "camp", "cultivar", "loc", "camp2", "soil", "camp3", "irri")
season_data <- map(.x = season_files, ~read_aquacrop_season(.x, "plugin/OUTP/")) %>%
  bind_rows()


##Evaluate * Metrics
get_metrics <- function(data) {
  
  data %>% filter(complete.cases(.)) %>%
    summarise(n = n(),
              r = cor(obs, sim, method = c("pearson")),
              RMSE = sqrt(mean((sim - obs)^2, na.rm = T)),
              NRMSE = RMSE/mean(obs, na.rm = T),
              MAE = sum(abs(sim - obs)/n),
              MBE = sum((sim - obs))/n,
              d = 1 - ((sum((sim - obs)^2, na.rm = T))/
                         sum((abs(sim - mean(obs, na.rm = T)) +
                                abs(obs - mean(obs, na.rm = T)))^2, na.rm = T)),
              NSE = 1 - ((sum((sim - obs)^2, na.rm = T))/
                           sum((obs - mean(obs, na.rm = T))^2, na.rm = T)),
              rsq = summary(lm(sim ~ obs))$r.squared)
  
}
eval_data_final <- data_to_project %>% 
  mutate(File =  paste(id_name, paste0(cultivar, "CRO"), soil, soil, paste0(irri, "IRR", set, ".PRM"), sep = "_")) %>%
  rename(obs = yield) %>%
  left_join(season_data %>% select(File, Yield) %>% rename(sim = Yield)) 


eval_data_final %>% 
  select(cultivar, obs, sim) %>% group_by(cultivar) %>% group_modify(~get_metrics(.x))
  





