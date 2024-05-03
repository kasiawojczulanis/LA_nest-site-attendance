# Libs ---
library(tidyverse)
library(readxl)
library(lubridate)

# Read data --- 

# pheno data
pheno <- read_excel("C:/Users/User/Dropbox/Metabases/Hornsund/SHO_LIAK_phenology_productivity.xlsx")  
pheno <- pheno %>%
  filter(Hatch_acc <=3) %>% 
  select(Season, Hatch_date) %>%
  group_by(Season) %>% 
  summarise(median_hatch_date = median(Hatch_date))

hatch_date_focalref <- pheno[pheno$Season == "2019",]


# behav data
dt19_h_nb <- read_excel("C:/Users/User/Dropbox/Liak2019/Hormones/Hormones video data.xlsx", sheet = "failed egg")

# session data
dt19_h_nb_vdur <- read_excel("C:/Users/User/Dropbox/Liak2019/Hormones/Hormones video data.xlsx", sheet = "Sessions")

# List of focal nests and sessions
failed_nests_list <- dt19_h_nb %>% 
  group_by(Session, Nest, SessionStartRealDate) %>% 
  summarise(n = n()) %>% 
    select(-n) %>% 
  ungroup() %>% 
  mutate(Day_chr = as.numeric(difftime(SessionStartRealDate, hatch_date$median_hatch_date, units = "days")))


# Session duration of the focal nests
dt19_h_nb_vdur <- dt19_h_nb_vdur %>% 
  select(session_type, nest, duration_session) %>% 
  filter(nest %in% unique(failed_nests_list$Nest),
         session_type %in% c("failure1", "failure2"))

# session trimming value
min_duration <- floor(as.numeric(min(dt19_h_nb_vdur$duration_session)))



# # custom funs
# source("./Working files/Projects/GPSeffect/GPS_effect/custom_functions.R")
