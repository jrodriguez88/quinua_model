### Simulacion escenarios CC

#sowing_date = ymd("2016-06-24")
ccdata_to_project %>% slice(1) %>% as.list() %>% pmap(make_project_by_date)

id_name <- ccdata_to_project$id_name[[1]]
sowing_dates <- ccdata_to_project$sowing_dates
cultivar <- ccdata_to_project$cultivar[[1]]
soil <- ccdata_to_project$soil[[1]]
clim_data <- ccdata_to_project$clim_data[[1]]
co2 <- ccdata_to_project$co2[[1]]
id2 <- ccdata_to_project$id2[[1]]
max_crop_duration <- ccdata_to_project$max_crop_duration[[1]]
aquacrop_files <- ccdata_to_project$aquacrop_files[[1]]
plugin_path <- ccdata_to_project$plugin_path[[1]]



make_project_by_date <- function(id_name, sowing_dates, cultivar, soil, clim_data, co2, id2,  max_crop_duration = 170, aquacrop_files, plugin_path){
  
  ## Create sowing dates vector, use when requiere 1 date 
  #    sowing_dates  <- c(sowing_date - (5:1), sowing_date + (0:4))
  ##### add function // eval inputs    
  ### load aquacrop files
  clim_file <- id_name
  co2_file <-  co2
  crop_file <- list.files(aquacrop_files, pattern = paste0(cultivar, ".CRO"))
  irri_file <- list.files(aquacrop_files, ".IRR") %>% c(., "rainfed")
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
    crop_duration <- clim_data %>% 
      dplyr::filter(date >= sowing_date,
                    date <= sowing_date + max_crop_duration - 1) %>%
      mutate(HUH = ((tmax + tmin)/2) - tbase) %>%
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
                                                sowing_date)) 
    
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
                       soil, "_", id2, "_", 
                       unique(sim_cycles$irri_file)) %>% 
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

wth_test <- weather_data %>% filter(localidad == "Taraco") %>% slice(1) %>% pull(data) %>% pluck(1)



make_hist_dates <- function (imonth = 6, fmonth = 8, clim_data, date_breaks = 5) {
  
  range_data <- year(range(clim_data$date))
  
  dates_base <- seq.Date(make_date(month = imonth, day=1), make_date(month = fmonth+1, day=1), by = date_breaks)
  
  map(dates_base,  ~make_date(year=range_data[1]:(range_data[2]-1), month = month(.x), day = day(.x)))
  
  
}


ccdata_to_project <- weather_data %>% filter(localidad == "Lampa")  %>% 
  mutate(sowing_dates = map(data, ~make_hist_dates(10, 11, .x))) %>% unnest(sowing_dates) %>% 
  mutate(id2 = paste0(localidad, 1:nrow(.)),
         soil = toupper(localidad),
         max_crop_duration = 180, 
         cultivar = list(c("QuinuaBJ", "QuinuaPAS", "QuinuaSI")),
         aquacrop_files = "D:/03_DEVELOPER/quinua_model/aquacrop_files/climate_escenaries/",
         plugin_path = "D:/03_DEVELOPER/quinua_model/plugin/") %>%
  unnest(cultivar) %>% rename(id_name = id, clim_data = data, co2 = co2_f) %>% 
  select(id_name, sowing_dates, cultivar, soil, clim_data, co2, id2, max_crop_duration, aquacrop_files, plugin_path)

# from ccdata
ccdata_to_project %>% select(id_name, id2, gcm, rcp, soil, cultivar) %>% write_csv("id_escenarioscc_lampa.csv")


unlink("plugin/OUTP/*")
unlink("plugin3/LIST/New folder/*")

ccdata_to_project %>% slice(1) %>%
  as.list() %>% pmap(make_project_by_date)


#Exportar proyectos
library(parallel)
####################################################### Setting parallel 
ncores <- detectCores() - 2
cl <- makeCluster(ncores)
clusterExport(cl, c(as.vector(lsf.str()),
                    "ccdata_to_project"))
clusterEvalQ(cl, {library(tidyverse);library(lubridate); library(sirad); library(data.table)})

#tictoc::tic()
parLapply(cl, ccdata_to_project %>% split(.,1:nrow(.)), function(x){ 
  as.list(x) %>% pmap(make_project_by_date)})
#tictoc::toc()
stopCluster(cl)
#########################################################################

### Set forders path for outputs - Preparing system to RUN AC-Plugin  
#rm(list=setdiff(ls(), c"x"))
path_pi <- str_replace("plugin/", pattern = "plugin", c("plugin", paste0("plugin", 2:3)))
path_ls <- list.dirs() %>% str_subset(pattern = "LIST")
path_op <- list.dirs() %>% str_subset(pattern = "OUTP")


## Borra contenido de carpetas LIST y OUTP
#map(paste0(path_ls[-1], "/*"),  ~unlink(paste0(.x)))
#map(paste0(path_op[-1], "/*"),  ~unlink(paste0(.x)))

#Function to distribuite files in different plugin folders
dist_files <- function(from, to, patt) {
  
  file.rename(list.files(from, full.names =  T, pattern = patt),
              paste0(to, "/", list.files(from, pattern = patt )))
}

dist_files("plugin/LIST/", "plugin/LIST/New folder/", "QuinuaBJ")
dist_files("plugin/LIST/", "plugin2/LIST/New folder/", "QuinuaPAS")
dist_files("plugin/LIST/", "plugin3/LIST/New folder/", "QuinuaSI")

gc()

## 9. Ejecuta Aquacrop mediante el PlugIng
tictoc::tic()
walk(paste0(path_pi, "ACsaV60.exe"), ~system(.x, wait = T))
#4278.01 sec elapsed
#2823.87 sec elapsed
tictoc::toc()





files <- list.files("plugin/LIST/New folder/") 


files %>% map(function(x) {
  dist_files("plugin/LIST/New folder/", "plugin/LIST/", x)
  system("plugin/ACsaV60.exe", timeout = 10)
  dist_files("plugin/LIST/", "plugin/LIST/New folder/", x)
})



  
  
  
  


         