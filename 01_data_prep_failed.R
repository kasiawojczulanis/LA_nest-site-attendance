rm(list = ls())

# Libs ---
library(tidyverse)
library(readxl)
library(lubridate)

# pheno data
pheno <- read_excel("C:/Users/User/Dropbox/Metabases/Hornsund/SHO_LIAK_phenology_productivity.xlsx")  
pheno <- pheno %>%
  filter(Hatch_acc <=3) %>% 
  select(Season, Hatch_date) %>%
  group_by(Season) %>% 
  summarise(median_hatch_date = median(Hatch_date))

hatch_date_focalref <- pheno[pheno$Season == "2019",]



# FAILED - read data ----- 

# FAILED behav data
dt19_h_nb <- read_excel("C:/Users/User/Dropbox/Liak2019/Hormones/Hormones video data.xlsx", sheet = "failed egg")

# FAILED session data
dt19_h_nb_vdur <- read_excel("C:/Users/User/Dropbox/Liak2019/Hormones/Hormones video data.xlsx", sheet = "Sessions")

# FAILED - list of focal nests and sessions
failed_nests_list <- dt19_h_nb %>% 
  group_by(Session, Nest, SessionStartRealDate) %>% 
  summarise(n = n()) %>% 
    select(-n) %>% 
  ungroup() %>% 
  mutate(Day_chr = as.numeric(difftime(SessionStartRealDate, hatch_date_focalref$median_hatch_date, units = "days")))

# FAILED - session duration of the focal nests
dt19_h_nb_vdur <- dt19_h_nb_vdur %>% 
  select(session_type, nest, duration_session) %>% 
  filter(nest %in% unique(failed_nests_list$Nest),
         session_type %in% c("failure1", "failure2"))



# FAILED session trimming value
min_duration <- floor(as.numeric(min(dt19_h_nb_vdur$duration_session)))


# FAILED trim data

# # check if there are just one SessionStartRealDate (should be 0 rows in the output table)
# dt19_h_nb %>% 
#   group_by(Session, Nest, SessionStartRealDate) %>% 
#   summarise(n = n()) %>% 
#   group_by(Session, Nest) %>% 
#   summarise(n = n()) %>% 
#   filter(n >1)
  
dt19_h_nb <- dt19_h_nb %>% 
  mutate(session_start = as.POSIXct(paste0(year(SessionStartRealDate), "-", month(SessionStartRealDate), "-", day(SessionStartRealDate), " ",
                                hour(SessionStartRealTime), ":", minute(SessionStartRealTime), ":", second(SessionStartRealTime), sep = "")),
         session_end24 = session_start + hours(24),
         session_end = session_start + hours(min_duration),
         
         sel_event_trimmed24 = if_else(realtime < session_end24, "ok", "censored"),
         sel_event_trimmed = if_else(realtime < session_end, "ok", "censored"),
         birdID = paste0(data_session, "_", BirdIDRings, sep = ""),
         Activity = case_when(
            Activity == "appears?" ~ "appears",
            Activity == "disappears?" ~ "disappears",
            Activity == "enters?" ~  "enters",
            Activity == "exits?" ~  "exits",
            Activity == "appears" ~ "appears",
            Activity == "disappears" ~ "disappears",
            Activity == "enters" ~  "enters",
            Activity == "exits" ~  "exits",
            TRUE ~ NA_character_)
  )  %>% 
  filter(Activity %in% c("appears", "disappears", "exits", "enters"),
         Age == "A")



  
# FAILED on screen - 24 hours interval -----
dt19_h_nb24 <- dt19_h_nb %>% 
  filter(sel_event_trimmed24 == "ok") 



inds <- unique(dt19_h_nb24$birdID)
ind_df_output <- list()

for(j in 1:length(inds)) {

  ind_df <- dt19_h_nb24 %>% 
    filter(birdID == inds[j]) %>% 
    arrange(realtime)
  
  
  on_screen_unit <- numeric()
  
  for(i in 2:nrow(ind_df)) {
    
    if (ind_df$Activity[i] == "disappears" &  ind_df$Activity[i-1] == "appears") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    if (ind_df$Activity[i] == "disappears" &  ind_df$Activity[i-1] == "exits") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    if (ind_df$Activity[i] == "enters" &  ind_df$Activity[i-1] == "appears") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    # last records
    
    if (i == nrow(ind_df) & ind_df$Activity[i] == "appears" ) {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$session_end24[i], units = "min"))
      
    } 
    
    
    if (i == nrow(ind_df) & ind_df$Activity[i] == "exits" ) {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$session_end24[i], units = "min"))
      
    } 
    
  }
  
  
  on_screen_unit <- sum(on_screen_unit, na.rm = TRUE)
  
  ind_df_output[[j]] <- data.frame(Session = unique(ind_df$Session), Session_Dt = (ind_df$session_start), Nest = unique(ind_df$Nest), 
                                   BirdRings = unique(ind_df$BirdIDRings), BirdMark = unique(ind_df$BirdIDMark), 
                                   on_screen_min24 = on_screen_unit)
  
  
  
}

ind_df_output <- plyr::ldply(ind_df_output, data.frame)

# Add sex
capt <- read_excel("C:/Users/User/Dropbox/Liak2019/FieldData/CapturingDt2019.xlsx")
capt <- capt %>% 
  select(Nest, RingNo, Mark, Sx) %>% 
  rename(BirdMark = Mark) %>% 
  filter(Nest %in% unique(ind_df_output$Nest)) %>% 
  distinct() %>% 
  arrange(Nest)


failed24_onscreen_time <- left_join(ind_df_output, capt, by = c("Nest", "BirdMark"))
failed24_onscreen_time <- failed24_onscreen_time %>% 
  mutate(Day_chr = floor(as.numeric(difftime(Session_Dt, hatch_date_focalref$median_hatch_date, units = "days"))))

saveRDS(failed24_onscreen_time, "failed24_onscreen_time.rds")




# FAILED on screen - 46 hours interval -----

dt19_h_nb46 <- dt19_h_nb %>% 
  filter(sel_event_trimmed == "ok")


inds <- unique(dt19_h_nb46$birdID)
ind_df_output <- list()

for(j in 1:length(inds)) {
  
  ind_df <- dt19_h_nb46 %>% 
    filter(birdID == inds[j]) %>% 
    arrange(realtime)
  
  
  on_screen_unit <- numeric()
  
  for(i in 2:nrow(ind_df)) {
    
    if (ind_df$Activity[i] == "disappears" &  ind_df$Activity[i-1] == "appears") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    if (ind_df$Activity[i] == "disappears" &  ind_df$Activity[i-1] == "exits") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    if (ind_df$Activity[i] == "enters" &  ind_df$Activity[i-1] == "appears") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    # last records
    
    if (i == nrow(ind_df) & ind_df$Activity[i] == "appears" ) {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$session_end[i], units = "min"))
      
    } 
    
    
    if (i == nrow(ind_df) & ind_df$Activity[i] == "exits" ) {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$session_end[i], units = "min"))
      
    } 
    
  }
  

  
  on_screen_unit <- sum(on_screen_unit, na.rm = TRUE)
  
  ind_df_output[[j]] <- data.frame(Session = unique(ind_df$Session), Session_Dt = (ind_df$session_start), Nest = unique(ind_df$Nest), 
                                   BirdRings = unique(ind_df$BirdIDRings), BirdMark = unique(ind_df$BirdIDMark), 
                                   on_screen_min46 = on_screen_unit)
  
  
  
}

ind_df_output <- plyr::ldply(ind_df_output, data.frame)

# Add sex
capt <- read_excel("C:/Users/User/Dropbox/Liak2019/FieldData/CapturingDt2019.xlsx")
capt <- capt %>% 
  select(Nest, RingNo, Mark, Sx) %>% 
  rename(BirdMark = Mark) %>% 
  filter(Nest %in% unique(ind_df_output$Nest)) %>% 
  distinct() %>% 
  arrange(Nest)


failed46_onscreen_time <- left_join(ind_df_output, capt, by = c("Nest", "BirdMark"))
failed46_onscreen_time <- failed46_onscreen_time %>% 
  mutate(Day_chr = floor(as.numeric(difftime(Session_Dt, hatch_date_focalref$median_hatch_date, units = "days"))))

saveRDS(failed46_onscreen_time, "failed46_onscreen_time.rds")



# FAILED presence/absence 24 ----

monitored_nests <- unique(dt19_h_nb_vdur$nest)
monitored_nests_partners <- capt %>% 
  filter(Nest %in% monitored_nests) 

f1 <- monitored_nests_partners %>% 
  mutate(Session = "failure1")
  
f2 <- monitored_nests_partners %>% 
  mutate(Session = "failure2")

pres_abs_dt <- rbind(f1, f2)

present24_inds <- failed24_onscreen_time %>% 
  select(Session, Nest, BirdRings, BirdRings, RingNo, Sx) %>% 
  distinct() %>% 
  mutate(presence = 1)

presence24_inds_df <- left_join(pres_abs_dt, present24_inds, by = join_by(Session, Nest, RingNo, Sx))

saveRDS(presence24_inds_df, "failed24_presence.rds")




# FAILED presence/absence 46 ----

present46_inds <- failed46_onscreen_time %>% 
  select(Session, Nest, BirdRings, BirdRings, RingNo, Sx) %>% 
  distinct() %>% 
  mutate(presence = 1)

presence46_inds_df <- left_join(pres_abs_dt, present46_inds, by = join_by(Session, Nest, RingNo, Sx))


saveRDS(presence46_inds_df, "failed46_presence.rds")


# BREEDERS - read data ----- 

# BREEDERS behav data
dt19_breeders <- read_excel("C:/Users/User/Dropbox/Liak2019/Coord Consistency/Coordination consistency video data.xlsx", 
                            sheet = "complete")

# BREEDERS session data
dt19_breeders_vdur <- read_excel("C:/Users/User/Dropbox/Liak2019/Coord Consistency/Coordination consistency video data.xlsx",
                                 sheet = "Sessions")

# BREEDERS - list of focal nests and sessions
breeders_nests_list <- dt19_breeders %>% 
  group_by(Session, Nest, SessionStartRealDate) %>% 
  summarise(n = n()) %>% 
  select(-n) %>% 
  ungroup() %>% 
  mutate(Day_chr = as.numeric(difftime(SessionStartRealDate, hatch_date_focalref$median_hatch_date, units = "days"))) %>% 
  filter(Day_chr >=3 & Day_chr <=19)


# BREEDERS - session duration of the focal nests
dt19_breeders_vdur <- dt19_breeders_vdur %>% 
  select(session_type, nest, duration_session) %>% 
  filter(nest %in% unique(breeders_nests_list$Nest),
         session_type %in% c("chick rearing1", "chick rearing2"))


# BREEDERS session trimming value
min_duration_breeders <- floor(as.numeric(min(dt19_breeders_vdur$duration_session)))
target_treshold <- 46 # as for failed breeders

# BREEDERS trim data

# check if there are just one SessionStartRealDate (should be 0 rows in the output table)
# dt19_breeders %>%
#   group_by(Session, Nest, SessionStartRealDate) %>%
#   summarise(n = n()) %>%
#   group_by(Session, Nest) %>%
#   summarise(n = n()) %>%
#   filter(n >1)

dt19_breeders <- dt19_breeders %>% 
  mutate(session_start = as.POSIXct(paste0(year(SessionStartRealDate), "-", month(SessionStartRealDate), "-", day(SessionStartRealDate), " ",
                                           hour(SessionStartRealTime), ":", minute(SessionStartRealTime), ":", second(SessionStartRealTime), sep = "")),
         session_end24 = session_start + hours(24),
         session_end = session_start + hours(target_treshold),
         
         sel_event_trimmed24 = if_else(realtime < session_end24, "ok", "censored"),
         sel_event_trimmed = if_else(realtime < session_end, "ok", "censored"),
         birdID = paste0(data_session, "_", BirdIDRings, sep = ""),
         Activity = case_when(
              Activity == "appears?" ~ "appears",
              Activity == "disappears?" ~ "disappears",
              Activity == "enters?" ~  "enters",
              Activity == "exits?" ~  "exits",
              Activity == "appears" ~ "appears",
              Activity == "disappears" ~ "disappears",
              Activity == "enters" ~  "enters",
              Activity == "exits" ~  "exits",
              
              TRUE ~ NA_character_))  %>%
           filter(Activity %in% c("appears", "disappears", "exits", "enters"),
                                  Age == "A",
                                  Session %in% c("chick rearing1", "chick rearing2")) 




# BREEDERS on screen - 24 hours interval -----

dt19_breeders24 <- dt19_breeders %>% 
  filter(sel_event_trimmed24 == "ok") 

inds <- unique(dt19_breeders24$birdID)
ind_df_output <- list()

for(j in 1:length(inds)) {
  
  ind_df <- dt19_breeders24 %>% 
    filter(birdID == inds[j]) %>% 
    arrange(realtime)
  
  on_screen_unit <- numeric()
  
  for(i in 2:nrow(ind_df)) {
    
    if (ind_df$Activity[i] == "disappears" &  ind_df$Activity[i-1] == "appears") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    if (ind_df$Activity[i] == "disappears" &  ind_df$Activity[i-1] == "exits") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    if (ind_df$Activity[i] == "enters" &  ind_df$Activity[i-1] == "appears") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    # last records
    
    if (i == nrow(ind_df) & ind_df$Activity[i] == "appears" ) {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$session_end24[i], units = "min"))
      
    } 
    
    
    if (i == nrow(ind_df) & ind_df$Activity[i] == "exits" ) {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$session_end24[i], units = "min"))
      
    } 
    
  }
  
  on_screen_unit <- sum(on_screen_unit, na.rm = TRUE)
  
  ind_df_output[[j]] <- data.frame(Session = unique(ind_df$Session), Session_Dt = (ind_df$session_start), Nest = unique(ind_df$Nest), 
                                   BirdRings = unique(ind_df$BirdIDRings), BirdMark = unique(ind_df$BirdIDMark), 
                                   on_screen_min24 = on_screen_unit)
  
  
  
}

ind_df_output <- plyr::ldply(ind_df_output, data.frame)

# filter extra adults

ind_df_output <- ind_df_output %>% 
  filter(BirdRings != "NA")

# add sex
capt <- read_excel("C:/Users/User/Dropbox/Liak2019/FieldData/CapturingDt2019.xlsx")
capt <- capt %>%
  mutate(BirdRings = paste0(R_leg, L_leg, sep = "")) %>% 
  rename(BirdMark = Mark) %>% 
  select(Nest, RingNo, BirdRings, BirdMark, Sx) %>% 
  filter(Nest %in% unique(ind_df_output$Nest)) %>% 
  distinct() %>% 
  arrange(Nest) %>% 
  filter(!is.na(BirdMark))

breeders24_onscreen_time <- left_join(ind_df_output, capt, by = c("Nest", "BirdMark", "BirdRings"))
breeders24_onscreen_time <- breeders24_onscreen_time %>% 
  mutate(Day_chr = floor(as.numeric(difftime(Session_Dt, hatch_date_focalref$median_hatch_date, units = "days"))))

saveRDS(breeders24_onscreen_time, "breeders24_onscreen_time.rds")



# BREEDERS on screen - 46 hours interval -----

dt19_breeders46 <- dt19_breeders %>% 
  filter(sel_event_trimmed == "ok") 

inds <- unique(dt19_breeders46$birdID)
ind_df_output <- list()

for(j in 1:length(inds)) {
  
  ind_df <- dt19_breeders46 %>% 
    filter(birdID == inds[j]) %>% 
    arrange(realtime)
  
  on_screen_unit <- numeric()
  
  for(i in 2:nrow(ind_df)) {
    
    if (ind_df$Activity[i] == "disappears" &  ind_df$Activity[i-1] == "appears") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    if (ind_df$Activity[i] == "disappears" &  ind_df$Activity[i-1] == "exits") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    if (ind_df$Activity[i] == "enters" &  ind_df$Activity[i-1] == "appears") {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$realtime[i-1], units = "min"))
      
    } 
    
    # last records
    
    if (i == nrow(ind_df) & ind_df$Activity[i] == "appears" ) {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$session_end[i], units = "min"))
      
    } 
    
    
    if (i == nrow(ind_df) & ind_df$Activity[i] == "exits" ) {
      
      on_screen_unit[i] <- as.numeric(difftime((ind_df$realtime[i]+seconds(1)), ind_df$session_end[i], units = "min"))
      
    } 
    
  }
  
  on_screen_unit <- sum(on_screen_unit, na.rm = TRUE)
  
  ind_df_output[[j]] <- data.frame(Session = unique(ind_df$Session), Session_Dt = (ind_df$session_start), Nest = unique(ind_df$Nest), 
                                   BirdRings = unique(ind_df$BirdIDRings), BirdMark = unique(ind_df$BirdIDMark), 
                                   on_screen_min46 = on_screen_unit)
  
  
  
}

ind_df_output <- plyr::ldply(ind_df_output, data.frame)

# filter extra adults

ind_df_output <- ind_df_output %>% 
  filter(BirdRings != "NA")

# add sex
capt <- read_excel("C:/Users/User/Dropbox/Liak2019/FieldData/CapturingDt2019.xlsx")
capt <- capt %>%
  mutate(BirdRings = paste0(R_leg, L_leg, sep = "")) %>% 
  rename(BirdMark = Mark) %>% 
  select(Nest, RingNo, BirdRings, BirdMark, Sx) %>% 
  filter(Nest %in% unique(ind_df_output$Nest)) %>% 
  distinct() %>% 
  arrange(Nest) %>% 
  filter(!is.na(BirdMark))

breeders46_onscreen_time <- left_join(ind_df_output, capt, by = c("Nest", "BirdMark", "BirdRings"))
breeders46_onscreen_time <- breeders46_onscreen_time %>% 
  mutate(Day_chr = floor(as.numeric(difftime(Session_Dt, hatch_date_focalref$median_hatch_date, units = "days"))))

saveRDS(breeders46_onscreen_time, "breeders46_onscreen_time.rds")


# Combined failed with breeders ------
failed24_onscreen_time <- failed24_onscreen_time %>% 
  mutate(status = "failed breeder",
         session = if_else(Session == "failure1", "early chick rearing", "mid chick rearing"))

failed46_onscreen_time <- failed46_onscreen_time %>% 
  mutate(status = "failed breeder",
         session = if_else(Session == "failure1", "early chick rearing", "mid chick rearing"))


breeders24_onscreen_time <- breeders24_onscreen_time %>% 
  mutate(status = "breeder",
         session = if_else(Session == "chick rearing1", "early chick rearing", "mid chick rearing"))

breeders46_onscreen_time <- breeders46_onscreen_time %>% 
  mutate(status = "breeder",
         session = if_else(Session == "chick rearing1", "early chick rearing", "mid chick rearing"))

all24 <- rbind(failed24_onscreen_time, breeders24_onscreen_time) 
saveRDS(all24, "all24_onscreen_time.rds")

all46 <- rbind(failed46_onscreen_time, breeders46_onscreen_time) 
saveRDS(all46, "all46_onscreen_time.rds")

