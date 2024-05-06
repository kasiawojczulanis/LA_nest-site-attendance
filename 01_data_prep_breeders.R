rm(list = ls())

# Libs ---
library(tidyverse)
library(readxl)
library(lubridate)

# custom funs
source("./custom_functions_william.R")


# pheno data
pheno <- read_excel("C:/Users/User/Dropbox/Metabases/Hornsund/SHO_LIAK_phenology_productivity.xlsx")  
pheno <- pheno %>%
  filter(Hatch_acc <=3) %>% 
  select(Season, Hatch_date) %>%
  group_by(Season) %>% 
  summarise(median_hatch_date = median(Hatch_date))

hatch_date_focalref <- pheno[pheno$Season == "2019",]




# Season specific ----

# Season 2022 ----
# (gps nests to filter out)

identity_data <- read_excel("C:/Users/User/Dropbox/Liak2022/FieldData/CapturingDt2022.xlsx", sheet = "FieldDt")
activity_data <- read_excel("C:/Users/User/Dropbox/Liak2022/Data/LIAK2022_video data.xlsx", sheet = "complete")  
vlogs_data <- read_excel("C:/Users/User/Dropbox/Liak2022/FieldData/VideoLogs2022.xlsx", sheet = "CoordNest") 
basic_intervals <- read_excel("C:/Users/User/Dropbox/Working files/Projects/William/basic_behav_intervals.xlsx", sheet = "intervals_dt")



# General part ---- 

# Add id and session infos
raw_activity_data <- add_sex_ring_device_session(activity_data = activity_data, 
                                         identity_data = identity_data, 
                                         vlogs_data = vlogs_data, 
                                         basic_intervals = basic_intervals)




# Add basic behav intervals

activities_basic_intervals <- basic_intervals # just rename objects for function's sake
activity_data <- raw_activity_data

# units to transform
df_split_unit <- split_data_frame(activity_data, c("Project", "Session", "RingNo"))

# loop for the transforming - basic intervals
df_transformed <- list()

for(d in 1:length(df_split_unit)) {
  df_transformed[[d]] <- timestamp_to_interval_behaviours(data_activity = df_split_unit[[d]], 
                                                          data_intervals = activities_basic_intervals) 
}

# output (not yet trimmed!)
df_transformed <- plyr::ldply(df_transformed, data.frame)

# save data 
season <- unique(year(df_transformed$session_start))
df_transformed_path <- paste0("C:/Users/User/Dropbox/Working files/Projects/William/breeders_transformed_activity_", season, ".rds", sep = "")
saveRDS(df_transformed, df_transformed_path)

# Trimming data ----

df_transformedx <- df_transformed %>% 
  mutate(activity_interval = interval(start = onset, end = end),
         session_1d = interval(start = session_start, end = session_start + hours(24)),
         session_2d = interval(start = session_start + hours(24), end = session_start + hours(48)),
         session_3d = interval(start = session_start + hours(48), end = session_start + hours(72)),
         session_4d = interval(start = session_start + hours(72), end = session_start + hours(96)),
         session_5d = interval(start = session_start + hours(96), end = session_start + hours(120)),
         session_6d = interval(start = session_start + hours(120), end = session_start + hours(144)),
         session_7d = interval(start = session_start + hours(144), end = session_start + hours(168))) %>% 
  mutate(session_days = if_else(int_overlaps(activity_interval, session_1d), 1,
                                             if_else(int_overlaps(activity_interval, session_2d), 2,
                                                     if_else(int_overlaps(activity_interval, session_3d), 3,
                                                             if_else(int_overlaps(activity_interval, session_4d), 4,
                                                                     if_else(int_overlaps(activity_interval, session_5d), 5,
                                                                             if_else(int_overlaps(activity_interval, session_6d), 6,
                                                                                     if_else(int_overlaps(activity_interval, session_7d), 7, 100))))))))





df_transformedx$trimmed <- NA_character_
df_transformedx$session_specific_end <- NA_POSIXct_

for(i in 1:nrow(df_transformedx)) {
  
  # First session day
  
  if(df_transformedx$session_days[i] == 1) {
    
    # first row
    if(df_transformedx$onset[i] > int_start(df_transformedx$session_1d[i])) {
      
      df_transformedx$trimmed[i] = "start_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_1d)[i]
      
    }
    
    
    # core
    if(df_transformedx$end[i] < int_end(df_transformedx$session_1d)[i]) {
      
      df_transformedx$trimmed[i] = "ok"
      df_transformedx$session_specific_end[i] = df_transformedx$end[i]
      
    }
    
    # last row
    if(df_transformedx$end[i] > int_end(df_transformedx$session_1d[i])) {
      
      df_transformedx$trimmed[i] = "end_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_1d[i])
      
    }
  }
  
  # Second session day
  
  if(df_transformedx$session_days[i] == 2) {
    
    # first row
    if(df_transformedx$onset[i] > int_start(df_transformedx$session_2d[i])) {
      
      df_transformedx$trimmed[i] = "start_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_2d[i])
      
    }
    
    
    # core
    if(df_transformedx$end[i] < int_end(df_transformedx$session_2d[i])) {
      
      df_transformedx$trimmed[i] = "ok"
      df_transformedx$session_specific_end[i] = df_transformedx$end[i]
      
    }
    
    # last row
    if(df_transformedx$end[i] > int_end(df_transformedx$session_2d[i])) {
      
      df_transformedx$trimmed[i] = "end_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_2d[i])
      
    }
    
  }
  
  # Third session day
  
  if(df_transformedx$session_days[i] == 3) {
    
    # first row
    if(df_transformedx$onset[i] > int_start(df_transformedx$session_3d[i])) {
      
      df_transformedx$trimmed[i] = "start_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_3d[i])
      
    }
    
    
    # core
    if(df_transformedx$end[i] < int_end(df_transformedx$session_3d[i])) {
      
      df_transformedx$trimmed[i] = "ok"
      df_transformedx$session_specific_end[i] = df_transformedx$end[i]
      
    }
    
    # last row
    if(df_transformedx$end[i] > int_end(df_transformedx$session_3d[i])) {
      
      df_transformedx$trimmed[i] = "end_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_3d[i])
      
    }
    
  }
  
  # Fourth session day
  
  if(df_transformedx$session_days[i] == 4) {
    
    # first row
    if(df_transformedx$onset[i] > int_start(df_transformedx$session_4d[i])) {
      
      df_transformedx$trimmed[i] = "start_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_4d[i])
      
    }
    
    
    # core
    if(df_transformedx$end[i] < int_end(df_transformedx$session_4d[i])) {
      
      df_transformedx$trimmed[i] = "ok"
      df_transformedx$session_specific_end[i] = df_transformedx$end[i]
      
    }
    
    # last row
    if(df_transformedx$end[i] > int_end(df_transformedx$session_4d[i])) {
      
      df_transformedx$trimmed[i] = "end_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_4d[i])
      
    }
    
  }
  
  # Fifth session day
  
  if(df_transformedx$session_days[i] == 5) {
    
    # first row
    if(df_transformedx$onset[i] > int_start(df_transformedx$session_5d[i])) {
      
      df_transformedx$trimmed[i] = "start_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_5d[i])
      
    }
    
    
    # core
    if(df_transformedx$end[i] < int_end(df_transformedx$session_5d[i])) {
      
      df_transformedx$trimmed[i] = "ok"
      df_transformedx$session_specific_end[i] = df_transformedx$end[i]
      
    }
    
    # last row
    if(df_transformedx$end[i] > int_end(df_transformedx$session_5d[i])) {
      
      df_transformedx$trimmed[i] = "end_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_5d[i])
      
    }
    
  }
  
  # Sixth session day
  
  if(df_transformedx$session_days[i] == 6) {
    
    # first row
    if(df_transformedx$onset[i] > int_start(df_transformedx$session_6d[i])) {
      
      df_transformedx$trimmed[i] = "start_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_6d[i])
      
    }
    
    
    # core
    if(df_transformedx$end[i] < int_end(df_transformedx$session_6d[i])) {
      
      df_transformedx$trimmed[i] = "ok"
      df_transformedx$session_specific_end[i] = df_transformedx$end[i]
      
    }
    
    # last row
    if(df_transformedx$end[i] > int_end(df_transformedx$session_6d)[i]) {
      
      df_transformedx$trimmed[i] = "end_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_6d[i])
      
    }
    
  }
  
  # Seventh session day
  
  if(df_transformedx$session_days[i] == 7) {
    
    # first row
    if(df_transformedx$onset[i] > int_start(df_transformedx$session_7d)[i]) {
      
      df_transformedx$trimmed[i] = "start_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_7d[i])
      
    }
    
    
    # core
    if(df_transformedx$end[i] < int_end(df_transformedx$session_7d)[i]) {
      
      df_transformedx$trimmed[i] = "ok"
      df_transformedx$session_specific_end[i] = df_transformedx$end[i]
      
    }
    
    # last row
    if(df_transformedx$end[i] > int_end(df_transformedx$session_7d)[i]) {
      
      df_transformedx$trimmed[i] = "end_"
      df_transformedx$session_specific_end[i] = int_end(df_transformedx$session_7d[i])
      
    }
    
  }
  
  
  # Tail
  
  if(df_transformedx$session_days[i] == 100) {
    
    df_transformedx$trimmed[i] = "delete"
    df_transformedx$session_specific_end[i] = df_transformedx$end[i]
    
  }
}

df_transformedxx <- df_transformedx %>% 
  mutate(session_specific_activity = if_else(trimmed == "ok", basic_interval, 
                                             if_else(trimmed == "delete", "delete", paste0(trimmed, basic_interval, sep = ""))),
         session_specific_activity_duration = as.numeric(difftime(session_specific_end, onset, units = "mins"))) %>% 
  filter(trimmed != "delete") %>% 
  group_by(project, session, nest, sx, ringno, device, session_days, basic_interval) %>% 
  summarise(sum_per_24 = sum(session_specific_activity_duration)) %>% 
  filter(basic_interval == "col_attendance")

ggplot(df_transformedxx, aes(x = as.factor(session_days), y = sum_per_24, fill = sx)) +
  geom_boxplot() +
  facet_wrap(~session)
df_transformedxx %>% 
  filter(session_days <=3) %>% 
ggplot(aes(x = session, y = sum_per_24, fill = sx)) +
  geom_boxplot() 
