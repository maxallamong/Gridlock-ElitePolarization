# Gridlock, Elite Polarization, and Attitudes about the Parties
# In-Party/Out-Party Favorability Ratings
# Author: Max Allamong
# Created: Oct. 16th, 2020
# Last Updated: Oct. 16th, 2020

# READ ME

# Packages ----
  #install.packages(c("ropercenter","here","haven","foreign","tidyverse","weights","conflicted))

# Libraries ----
  library(ropercenter)
  library(here)
  library(haven)
  library(foreign)
  library(tidyverse)
  library(weights)
  library(conflicted)
    conflict_prefer("summarize","dplyr")
    conflict_prefer("filter","dplyr")

# Working Directory ----
  setwd(here())

# Load Surveys ----
  # ANES -----
  anes <- read_dta("Data/anes_timeseries_cdf.dta") 
  anes <- anes %>% 
    select(VCF0004, VCF0218, VCF0224, VCF0301, VCF0009x) %>% # select necessary variables
    rename(year = VCF0004, dem.therm = VCF0218, rep.therm = VCF0224, # rename variables
           pid = VCF0301, weight = VCF0009x) %>%
    mutate(pid3 = ifelse(pid == 5 | pid == 6 | pid == 7, 1, # Strong/Weak/Leaning Republicans = 1
                         ifelse(pid == 1 | pid == 2 | pid == 3, 2, 3))) %>% # Strong/Weak/Leaning Democrats = 2, Pure Independents = 3
    mutate(strong.rep = ifelse(pid == 7, 1, 0)) %>% # Strong Republican = 1
    mutate(strong.dem = ifelse(pid == 1, 1, 0)) # Strong Democrat = 1
  
  repins <- anes %>%
    filter(pid3 == 1) %>%
    mutate(therm = rep.therm) %>%
    select(therm, year) %>%
    drop_na()
  
  demins <- anes %>%
    filter(pid3 == 2) %>%
    mutate(therm = dem.therm) %>%
    select(therm, year) %>%
    drop_na()
  
  anes.ins <- rbind(repins, demins)
  anes.ins <- anes.ins %>%
    group_by(year) %>%
    summarize(VALUE = mean(therm, na.rm = T), N = n()) %>%
    drop_na() %>%
    mutate(DATE = paste0("11-01-", year), VARNAME = "ANES", VALUE = round(VALUE, 2)) %>%
    select(VARNAME, DATE, VALUE, N)
  
  repouts <- anes %>%
    filter(pid3 == 1) %>%
    mutate(therm = dem.therm) %>%
    select(therm, year) %>%
    drop_na()
  
  demouts <- anes %>%
    filter(pid3 == 2) %>%
    mutate(therm = rep.therm) %>%
    select(therm, year) %>%
    drop_na()
  
  anes.outs <- rbind(repouts, demouts)
  anes.outs <- anes.outs %>%
    group_by(year) %>%
    summarize(VALUE = mean(therm, na.rm = T), N = n()) %>%
    drop_na() %>%
    mutate(DATE = paste0("11-01-", year), VARNAME = "ANES", VALUE = round(VALUE, 2)) %>%
    select(VARNAME, DATE, VALUE, N)
  
  rm(anes, demins, demouts, repins, repouts)
  
  
  # Gallup ----
    # Binary ----
      # September 1, 1996 ----
      gallup.19960901 <- read_ascii("Data/Surveys/g9608025a.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(1,90,91,40,41),
                                    var_widths = c(4,1,1,1,1))
      gallup.19960901.repins <- gallup.19960901 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19960901.demins <- gallup.19960901 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19960901.repouts <- gallup.19960901 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19960901.demouts <- gallup.19960901 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19960901.ins <- rbind(gallup.19960901.repins, gallup.19960901.demins)
      wpct(gallup.19960901.ins$fav, as.numeric(gallup.19960901.ins$weight)) # 92.09
      length(gallup.19960901.ins$weight) # 928
      
      gallup.19960901.outs <- rbind(gallup.19960901.repouts, gallup.19960901.demouts)
      wpct(gallup.19960901.outs$fav, as.numeric(gallup.19960901.outs$weight)) # 25.22
      length(gallup.19960901.outs$weight) # 909
      
      rm(gallup.19960901, 
         gallup.19960901.repins, gallup.19960901.demins,
         gallup.19960901.repouts, gallup.19960901.demouts,
         gallup.19960901.ins, gallup.19960901.outs)
    
      # October 29, 1997 ----
      gallup.19971029 <- read_ascii("Data/Surveys/a9710026.dat", # Good
                                    total_cards = 7,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,29,30,18,19),
                                    var_widths = c(3,1,1,1,1),
                                    var_cards = c(1,6,6,5,5))
      gallup.19971029.repins <- gallup.19971029 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19971029.demins <- gallup.19971029 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19971029.repouts <- gallup.19971029 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19971029.demouts <- gallup.19971029 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19971029.ins <- rbind(gallup.19971029.repins, gallup.19971029.demins)
      wpct(gallup.19971029.ins$fav, as.numeric(gallup.19971029.ins$weight)) # 90.08
      length(gallup.19971029.ins$weight) # 904
      
      gallup.19971029.outs <- rbind(gallup.19971029.repouts, gallup.19971029.demouts)
      wpct(gallup.19971029.outs$fav, as.numeric(gallup.19971029.outs$weight)) # 22.12
      length(gallup.19971029.outs$weight) # 878
      
      rm(gallup.19971029, 
         gallup.19971029.repins, gallup.19971029.demins,
         gallup.19971029.repouts, gallup.19971029.demouts,
         gallup.19971029.ins, gallup.19971029.outs)
      
    
      # December 16, 1998 ----
      gallup.19981216 <- read_ascii("Data/Surveys/a9812054.dat", # Good
                                    total_cards = 6,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,35,36,18,19),
                                    var_widths = c(3,1,1,1,1),
                                    var_cards = c(1,6,6,5,5))
      gallup.19981216.repins <- gallup.19981216 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19981216.demins <- gallup.19981216 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19981216.repouts <- gallup.19981216 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19981216.demouts <- gallup.19981216 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19981216.ins <- rbind(gallup.19981216.repins, gallup.19981216.demins)
      wpct(gallup.19981216.ins$fav, as.numeric(gallup.19981216.ins$weight)) # 88.47
      length(gallup.19981216.ins$weight) # 914
      
      gallup.19981216.outs <- rbind(gallup.19981216.repouts, gallup.19981216.demouts)
      wpct(gallup.19981216.outs$fav, as.numeric(gallup.19981216.outs$weight)) # 23.28
      length(gallup.19981216.outs$weight) # 890
      
      rm(gallup.19981216, 
         gallup.19981216.repins, gallup.19981216.demins,
         gallup.19981216.repouts, gallup.19981216.demouts,
         gallup.19981216.ins, gallup.19981216.outs)
      
      # February 7, 1999 ----
      # gallup.19990207 <- read_ascii("Data/Surveys/9902008g.dat", # Number of lines in the file is not a multiple of the number of cards
      #                               total_cards = 9,
      #                               var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
      #                               var_positions = c(13,36,37,18,19),
      #                               var_widths = c(3,1,1,1,1),
      #                               var_cards = c(1,6,6,5,5))
    
      # February 13, 1999 ----
      gallup.19990213 <- read_ascii("Data/Surveys/a9902011.dat", # Good
                                    total_cards = 9,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,26,27,18,19),
                                    var_widths = c(3,1,1,1,1),
                                    var_cards = c(1,6,6,5,5))
      gallup.19990213.repins <- gallup.19990213 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19990213.demins <- gallup.19990213 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19990213.repouts <- gallup.19990213 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19990213.demouts <- gallup.19990213 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19990213.ins <- rbind(gallup.19990213.repins, gallup.19990213.demins)
      wpct(gallup.19990213.ins$fav, as.numeric(gallup.19990213.ins$weight)) # 83.18
      length(gallup.19990213.ins$weight) # 909
      
      gallup.19990213.outs <- rbind(gallup.19990213.repouts, gallup.19990213.demouts)
      wpct(gallup.19990213.outs$fav, as.numeric(gallup.19990213.outs$weight)) # 21.37
      length(gallup.19990213.outs$weight) # 902
      
      rm(gallup.19990213, 
         gallup.19990213.repins, gallup.19990213.demins,
         gallup.19990213.repouts, gallup.19990213.demouts,
         gallup.19990213.ins, gallup.19990213.outs)
      
      # February 21, 1999 ----
      gallup.19990221 <- read_ascii("Data/Surveys/9902012.dat", # Good
                                    total_cards = 7,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,45,46,18,19),
                                    var_widths = c(3,1,1,1,1),
                                    var_cards = c(1,6,6,5,5))
      gallup.19990221.repins <- gallup.19990221 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19990221.demins <- gallup.19990221 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19990221.repouts <- gallup.19990221 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19990221.demouts <- gallup.19990221 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19990221.ins <- rbind(gallup.19990221.repins, gallup.19990221.demins)
      wpct(gallup.19990221.ins$fav, as.numeric(gallup.19990221.ins$weight)) # 89.98
      length(gallup.19990221.ins$weight) # 886
      
      gallup.19990221.outs <- rbind(gallup.19990221.repouts, gallup.19990221.demouts)
      wpct(gallup.19990221.outs$fav, as.numeric(gallup.19990221.outs$weight)) # 19.81
      length(gallup.19990221.outs$weight) # 864
      
      rm(gallup.19990221, 
         gallup.19990221.repins, gallup.19990221.demins,
         gallup.19990221.repouts, gallup.19990221.demouts,
         gallup.19990221.ins, gallup.19990221.outs)
  
      # May 2, 1999 ----
      gallup.19990502 <- read_ascii("Data/Surveys/a9904025.dat", # Good
                                    total_cards = 7,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,15,16,18,19),
                                    var_widths = c(3,1,1,1,1),
                                    var_cards = c(1,7,7,5,5))
      gallup.19990502.repins <- gallup.19990502 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19990502.demins <- gallup.19990502 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19990502.repouts <- gallup.19990502 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19990502.demouts <- gallup.19990502 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19990502.ins <- rbind(gallup.19990502.repins, gallup.19990502.demins)
      wpct(gallup.19990502.ins$fav, as.numeric(gallup.19990502.ins$weight)) # 87.00
      length(gallup.19990502.ins$weight) # 856
      
      gallup.19990502.outs <- rbind(gallup.19990502.repouts, gallup.19990502.demouts)
      wpct(gallup.19990502.outs$fav, as.numeric(gallup.19990502.outs$weight)) # 22.02
      length(gallup.19990502.outs$weight) # 826
      
      rm(gallup.19990502, 
         gallup.19990502.repins, gallup.19990502.demins,
         gallup.19990502.repouts, gallup.19990502.demouts,
         gallup.19990502.ins, gallup.19990502.outs)
  
      # November 21, 1999 ----
      gallup.19991121 <- read_ascii("Data/Surveys/g9911045.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(7,93,94,67,68),
                                    var_widths = c(4,1,1,1,1))
      gallup.19991121.repins <- gallup.19991121 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19991121.demins <- gallup.19991121 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19991121.repouts <- gallup.19991121 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19991121.demouts <- gallup.19991121 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19991121.ins <- rbind(gallup.19991121.repins, gallup.19991121.demins)
      wpct(gallup.19991121.ins$fav, as.numeric(gallup.19991121.ins$weight)) # 84.09
      length(gallup.19991121.ins$weight) # 857
      
      gallup.19991121.outs <- rbind(gallup.19991121.repouts, gallup.19991121.demouts)
      wpct(gallup.19991121.outs$fav, as.numeric(gallup.19991121.outs$weight)) # 25.99
      length(gallup.19991121.outs$weight) # 845
      
      rm(gallup.19991121, 
         gallup.19991121.repins, gallup.19991121.demins,
         gallup.19991121.repouts, gallup.19991121.demouts,
         gallup.19991121.ins, gallup.19991121.outs)
    
      # December 20, 1999 ----
      gallup.19991220 <- read_ascii("Data/Surveys/g9812056.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(7,86,87,66,67),
                                    var_widths = c(4,1,1,1,1))
      gallup.19991220.repins <- gallup.19991220 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19991220.demins <- gallup.19991220 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19991220.repouts <- gallup.19991220 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19991220.demouts <- gallup.19991220 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19991220.ins <- rbind(gallup.19991220.repins, gallup.19991220.demins)
      wpct(gallup.19991220.ins$fav, as.numeric(gallup.19991220.ins$weight)) # 90.18
      length(gallup.19991220.ins$weight) # 709
      
      gallup.19991220.outs <- rbind(gallup.19991220.repouts, gallup.19991220.demouts)
      wpct(gallup.19991220.outs$fav, as.numeric(gallup.19991220.outs$weight)) # 18.83
      length(gallup.19991220.outs$weight) # 692
      
      rm(gallup.19991220, 
         gallup.19991220.repins, gallup.19991220.demins,
         gallup.19991220.repouts, gallup.19991220.demouts,
         gallup.19991220.ins, gallup.19991220.outs)
    
      # January 10, 2000 ----
      gallup.20000110 <- read_ascii("Data/Surveys/a200001.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(7,99,100,40,41),
                                    var_widths = c(4,1,1,1,1))
      gallup.20000110.repins <- gallup.20000110 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20000110.demins <- gallup.20000110 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20000110.repouts <- gallup.20000110 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20000110.demouts <- gallup.20000110 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20000110.ins <- rbind(gallup.20000110.repins, gallup.20000110.demins)
      wpct(gallup.20000110.ins$fav, as.numeric(gallup.20000110.ins$weight)) # 90.41
      length(gallup.20000110.ins$weight) # 1412
      
      gallup.20000110.outs <- rbind(gallup.20000110.repouts, gallup.20000110.demouts)
      wpct(gallup.20000110.outs$fav, as.numeric(gallup.20000110.outs$weight)) # 32.65
      length(gallup.20000110.outs$weight) # 1384
      
      rm(gallup.20000110, 
         gallup.20000110.repins, gallup.20000110.demins,
         gallup.20000110.repouts, gallup.20000110.demouts,
         gallup.20000110.ins, gallup.20000110.outs)
      # July 26, 2000 ----
      gallup.20000726 <- read_ascii("Data/Surveys/a200030.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(7,100,101,60,61),
                                    var_widths = c(4,1,1,1,1))
      gallup.20000726.repins <- gallup.20000726 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20000726.demins <- gallup.20000726 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20000726.repouts <- gallup.20000726 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20000726.demouts <- gallup.20000726 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20000726.ins <- rbind(gallup.20000726.repins, gallup.20000726.demins)
      wpct(gallup.20000726.ins$fav, as.numeric(gallup.20000726.ins$weight)) # 96.00
      length(gallup.20000726.ins$weight) # 896
      
      gallup.20000726.outs <- rbind(gallup.20000726.repouts, gallup.20000726.demouts)
      wpct(gallup.20000726.outs$fav, as.numeric(gallup.20000726.outs$weight)) # 20.81
      length(gallup.20000726.outs$weight) # 850
      
      rm(gallup.20000726, 
         gallup.20000726.repins, gallup.20000726.demins,
         gallup.20000726.repouts, gallup.20000726.demouts,
         gallup.20000726.ins, gallup.20000726.outs)
  
      # August 5, 2000 ----
      gallup.20000805 <- read_ascii("Data/Surveys/a200032.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(7,98,99,60,61),
                                    var_widths = c(4,1,1,1,1))
      gallup.20000805.repins <- gallup.20000805 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20000805.demins <- gallup.20000805 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20000805.repouts <- gallup.20000805 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20000805.demouts <- gallup.20000805 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20000805.ins <- rbind(gallup.20000805.repins, gallup.20000805.demins)
      wpct(gallup.20000805.ins$fav, as.numeric(gallup.20000805.ins$weight)) # 94.06
      length(gallup.20000805.ins$weight) # 924
      
      gallup.20000805.outs <- rbind(gallup.20000805.repouts, gallup.20000805.demouts)
      wpct(gallup.20000805.outs$fav, as.numeric(gallup.20000805.outs$weight)) # 24.05
      length(gallup.20000805.outs$weight) # 894
      
      rm(gallup.20000805, 
         gallup.20000805.repins, gallup.20000805.demins,
         gallup.20000805.repouts, gallup.20000805.demouts,
         gallup.20000805.ins, gallup.20000805.outs)
    
      # November 15, 2000 ----
      gallup.20001115 <- read_ascii("Data/Surveys/g200049.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,93,94,68,69),
                                    var_widths = c(4,1,1,1,1))
      gallup.20001115.repins <- gallup.20001115 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20001115.demins <- gallup.20001115 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20001115.repouts <- gallup.20001115 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20001115.demouts <- gallup.20001115 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20001115.ins <- rbind(gallup.20001115.repins, gallup.20001115.demins)
      wpct(gallup.20001115.ins$fav, as.numeric(gallup.20001115.ins$weight)) # 93.74
      length(gallup.20001115.ins$weight) # 916
      
      gallup.20001115.outs <- rbind(gallup.20001115.repouts, gallup.20001115.demouts)
      wpct(gallup.20001115.outs$fav, as.numeric(gallup.20001115.outs$weight)) # 17.61
      length(gallup.20001115.outs$weight) # 888
      
      rm(gallup.20001115, 
         gallup.20001115.repins, gallup.20001115.demins,
         gallup.20001115.repouts, gallup.20001115.demouts,
         gallup.20001115.ins, gallup.20001115.outs)
    
      # January 14, 2002 ----
      gallup.20020114 <- read_ascii("Data/Surveys/200202.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,100,101,70,71),
                                    var_widths = c(4,1,1,1,1))
      gallup.20020114.repins <- gallup.20020114 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20020114.demins <- gallup.20020114 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20020114.repouts <- gallup.20020114 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20020114.demouts <- gallup.20020114 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20020114.ins <- rbind(gallup.20020114.repins, gallup.20020114.demins)
      wpct(gallup.20020114.ins$fav, as.numeric(gallup.20020114.ins$weight)) # 93.31
      length(gallup.20020114.ins$weight) # 887
      
      gallup.20020114.outs <- rbind(gallup.20020114.repouts, gallup.20020114.demouts)
      wpct(gallup.20020114.outs$fav, as.numeric(gallup.20020114.outs$weight)) # 37.13
      length(gallup.20020114.outs$weight) # 841
      
      rm(gallup.20020114, 
         gallup.20020114.repins, gallup.20020114.demins,
         gallup.20020114.repouts, gallup.20020114.demouts,
         gallup.20020114.ins, gallup.20020114.outs)
    
      # July 28, 2002 ----
      gallup.20020728 <- read_ascii("Data/Surveys/2002_07_26x.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,132,133,83,84),
                                    var_widths = c(4,1,1,1,1))
      gallup.20020728.repins <- gallup.20020728 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20020728.demins <- gallup.20020728 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20020728.repouts <- gallup.20020728 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20020728.demouts <- gallup.20020728 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20020728.ins <- rbind(gallup.20020728.repins, gallup.20020728.demins)
      wpct(gallup.20020728.ins$fav, as.numeric(gallup.20020728.ins$weight)) # 93.73
      length(gallup.20020728.ins$weight) # 851
      
      gallup.20020728.outs <- rbind(gallup.20020728.repouts, gallup.20020728.demouts)
      wpct(gallup.20020728.outs$fav, as.numeric(gallup.20020728.outs$weight)) # 32.65
      length(gallup.20020728.outs$weight) # 817
      
      rm(gallup.20020728, 
         gallup.20020728.repins, gallup.20020728.demins,
         gallup.20020728.repouts, gallup.20020728.demouts,
         gallup.20020728.ins, gallup.20020728.outs)
      # November 10, 2002 ----
      gallup.20021110 <- read_ascii("Data/Surveys/2002_11_08x.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,105,106,78,79),
                                    var_widths = c(4,1,1,1,1))
      gallup.20021110.repins <- gallup.20021110 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20021110.demins <- gallup.20021110 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20021110.repouts <- gallup.20021110 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20021110.demouts <- gallup.20021110 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20021110.ins <- rbind(gallup.20021110.repins, gallup.20021110.demins)
      wpct(gallup.20021110.ins$fav, as.numeric(gallup.20021110.ins$weight)) # 89.71
      length(gallup.20021110.ins$weight) # 885
      
      gallup.20021110.outs <- rbind(gallup.20021110.repouts, gallup.20021110.demouts)
      wpct(gallup.20021110.outs$fav, as.numeric(gallup.20021110.outs$weight)) # 25.83
      length(gallup.20021110.outs$weight) # 849
      
      rm(gallup.20021110, 
         gallup.20021110.repins, gallup.20021110.demins,
         gallup.20021110.repouts, gallup.20021110.demouts,
         gallup.20021110.ins, gallup.20021110.outs)
    
      # December 17, 2002 ----
      gallup.20021217 <- read_ascii("Data/Surveys/2002_12_16x.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,121,122,78,79),
                                    var_widths = c(4,1,1,1,1))
      gallup.20021217.repins <- gallup.20021217 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20021217.demins <- gallup.20021217 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20021217.repouts <- gallup.20021217 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20021217.demouts <- gallup.20021217 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20021217.ins <- rbind(gallup.20021217.repins, gallup.20021217.demins)
      wpct(gallup.20021217.ins$fav, as.numeric(gallup.20021217.ins$weight)) # 90.30
      length(gallup.20021217.ins$weight) # 889
      
      gallup.20021217.outs <- rbind(gallup.20021217.repouts, gallup.20021217.demouts)
      wpct(gallup.20021217.outs$fav, as.numeric(gallup.20021217.outs$weight)) # 24.02
      length(gallup.20021217.outs$weight) # 857
      
      rm(gallup.20021217, 
         gallup.20021217.repins, gallup.20021217.demins,
         gallup.20021217.repouts, gallup.20021217.demouts,
         gallup.20021217.ins, gallup.20021217.outs)
      # January 4, 2003 ----
      gallup.20030104 <- read_ascii("Data/Surveys/2003_01_03x.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,111,112,78,79),
                                    var_widths = c(4,1,1,1,1))
      gallup.20030104.repins <- gallup.20030104 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20030104.demins <- gallup.20030104 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20030104.repouts <- gallup.20030104 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20030104.demouts <- gallup.20030104 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20030104.ins <- rbind(gallup.20030104.repins, gallup.20030104.demins)
      wpct(gallup.20030104.ins$fav, as.numeric(gallup.20030104.ins$weight)) # 91.36
      length(gallup.20030104.ins$weight) # 859
      
      gallup.20030104.outs <- rbind(gallup.20030104.repouts, gallup.20030104.demouts)
      wpct(gallup.20030104.outs$fav, as.numeric(gallup.20030104.outs$weight)) # 25.62
      length(gallup.20030104.outs$weight) # 832
      
      rm(gallup.20030104, 
         gallup.20030104.repins, gallup.20030104.demins,
         gallup.20030104.repouts, gallup.20030104.demouts,
         gallup.20030104.ins, gallup.20030104.outs)
      
      # March 30, 2002 ----
      gallup.20030330 <- read_ascii("Data/Surveys/2003_03_29X.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,115,116,80,81),
                                    var_widths = c(4,1,1,1,1))
      gallup.20030330.repins <- gallup.20030330 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20030330.demins <- gallup.20030330 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20030330.repouts <- gallup.20030330 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20030330.demouts <- gallup.20030330 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20030330.ins <- rbind(gallup.20030330.repins, gallup.20030330.demins)
      wpct(gallup.20030330.ins$fav, as.numeric(gallup.20030330.ins$weight)) # 92.96
      length(gallup.20030330.ins$weight) # 859
      
      gallup.20030330.outs <- rbind(gallup.20030330.repouts, gallup.20030330.demouts)
      wpct(gallup.20030330.outs$fav, as.numeric(gallup.20030330.outs$weight)) # 26.50
      length(gallup.20030330.outs$weight) # 830
      
      rm(gallup.20030330, 
         gallup.20030330.repins, gallup.20030330.demins,
         gallup.20030330.repouts, gallup.20030330.demouts,
         gallup.20030330.ins, gallup.20030330.outs)
    
      # January 5, 2004 ----
      gallup.20040105 <- read_ascii("Data/Surveys/ga200401.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,131,130,85,86),
                                    var_widths = c(4,1,1,1,1))
      gallup.20040105.repins <- gallup.20040105 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20040105.demins <- gallup.20040105 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040105.repouts <- gallup.20040105 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040105.demouts <- gallup.20040105 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20040105.ins <- rbind(gallup.20040105.repins, gallup.20040105.demins)
      wpct(gallup.20040105.ins$fav, as.numeric(gallup.20040105.ins$weight)) # 87.45
      length(gallup.20040105.ins$weight) # 925
      
      gallup.20040105.outs <- rbind(gallup.20040105.repouts, gallup.20040105.demouts)
      wpct(gallup.20040105.outs$fav, as.numeric(gallup.20040105.outs$weight)) # 20.37
      length(gallup.20040105.outs$weight) # 895
      
      rm(gallup.20040105, 
         gallup.20040105.repins, gallup.20040105.demins,
         gallup.20040105.repouts, gallup.20040105.demouts,
         gallup.20040105.ins, gallup.20040105.outs)
    
      # February 1, 2004 ----
      gallup.20040201 <- read_ascii("Data/Surveys/ga200407.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,134,133,87,88),
                                    var_widths = c(4,1,1,1,1))
      gallup.20040201.repins <- gallup.20040201 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20040201.demins <- gallup.20040201 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040201.repouts <- gallup.20040201 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040201.demouts <- gallup.20040201 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20040201.ins <- rbind(gallup.20040201.repins, gallup.20040201.demins)
      wpct(gallup.20040201.ins$fav, as.numeric(gallup.20040201.ins$weight)) # 94.27
      length(gallup.20040201.ins$weight) # 925
      
      gallup.20040201.outs <- rbind(gallup.20040201.repouts, gallup.20040201.demouts)
      wpct(gallup.20040201.outs$fav, as.numeric(gallup.20040201.outs$weight)) # 21.08
      length(gallup.20040201.outs$weight) # 898
      
      rm(gallup.20040201, 
         gallup.20040201.repins, gallup.20040201.demins,
         gallup.20040201.repouts, gallup.20040201.demouts,
         gallup.20040201.ins, gallup.20040201.outs)
    
      # July 21, 2004 ----
      gallup.20040721 <- read_ascii("Data/Surveys/g200424.dat", # Good (half sample)
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,129,128,87,88),
                                    var_widths = c(4,1,1,1,1))
      gallup.20040721.repins <- gallup.20040721 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20040721.demins <- gallup.20040721 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040721.repouts <- gallup.20040721 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040721.demouts <- gallup.20040721 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20040721.ins <- rbind(gallup.20040721.repins, gallup.20040721.demins)
      wpct(gallup.20040721.ins$fav, as.numeric(gallup.20040721.ins$weight)) # 89.11
      length(gallup.20040721.ins$weight) # 459
      
      gallup.20040721.outs <- rbind(gallup.20040721.repouts, gallup.20040721.demouts)
      wpct(gallup.20040721.outs$fav, as.numeric(gallup.20040721.outs$weight)) # 16.29
      length(gallup.20040721.outs$weight) # 446
      
      rm(gallup.20040721, 
         gallup.20040721.repins, gallup.20040721.demins,
         gallup.20040721.repouts, gallup.20040721.demouts,
         gallup.20040721.ins, gallup.20040721.outs)
    
      # August 1, 2004 ----
      gallup.20040801 <- read_ascii("Data/Surveys/g200425.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", 'pid.lean'),
                                    var_positions = c(7,132,131,87,88),
                                    var_widths = c(4,1,1,1,1))
      gallup.20040801.repins <- gallup.20040801 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20040801.demins <- gallup.20040801 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040801.repouts <- gallup.20040801 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040801.demouts <- gallup.20040801 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20040801.ins <- rbind(gallup.20040801.repins, gallup.20040801.demins)
      wpct(gallup.20040801.ins$fav, as.numeric(gallup.20040801.ins$weight)) # 95.08
      length(gallup.20040801.ins$weight) # 1382
      
      gallup.20040801.outs <- rbind(gallup.20040801.repouts, gallup.20040801.demouts)
      wpct(gallup.20040801.outs$fav, as.numeric(gallup.20040801.outs$weight)) # 14.42
      length(gallup.20040801.outs$weight) # 1333
      
      rm(gallup.20040801, 
         gallup.20040801.repins, gallup.20040801.demins,
         gallup.20040801.repouts, gallup.20040801.demouts,
         gallup.20040801.ins, gallup.20040801.outs)
  
      # August 25, 2004 ----
      gallup.20040825 <- read_ascii("Data/Surveys/g200427.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,127,126,83,84),
                                    var_widths = c(4,1,1,1,1))
      gallup.20040825.repins <- gallup.20040825 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20040825.demins <- gallup.20040825 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040825.repouts <- gallup.20040825 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040825.demouts <- gallup.20040825 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20040825.ins <- rbind(gallup.20040825.repins, gallup.20040825.demins)
      wpct(gallup.20040825.ins$fav, as.numeric(gallup.20040825.ins$weight)) # 90.77
      length(gallup.20040825.ins$weight) # 419
      
      gallup.20040825.outs <- rbind(gallup.20040825.repouts, gallup.20040825.demouts)
      wpct(gallup.20040825.outs$fav, as.numeric(gallup.20040825.outs$weight)) # 26.67
      length(gallup.20040825.outs$weight) # 410
      
      rm(gallup.20040825, 
         gallup.20040825.repins, gallup.20040825.demins,
         gallup.20040825.repouts, gallup.20040825.demouts,
         gallup.20040825.ins, gallup.20040825.outs)
    
      # September 5, 2004 ----
      gallup.20040905 <- read_ascii("Data/Surveys/g200428.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,132,131,87,88),
                                    var_widths = c(4,1,1,1,1))
      gallup.20040905.repins <- gallup.20040905 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20040905.demins <- gallup.20040905 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040905.repouts <- gallup.20040905 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20040905.demouts <- gallup.20040905 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20040905.ins <- rbind(gallup.20040905.repins, gallup.20040905.demins)
      wpct(gallup.20040905.ins$fav, as.numeric(gallup.20040905.ins$weight)) # 93.99
      length(gallup.20040905.ins$weight) # 930
      
      gallup.20040905.outs <- rbind(gallup.20040905.repouts, gallup.20040905.demouts)
      wpct(gallup.20040905.outs$fav, as.numeric(gallup.20040905.outs$weight)) # 19.37
      length(gallup.20040905.outs$weight) # 896
      
      rm(gallup.20040905, 
         gallup.20040905.repins, gallup.20040905.demins,
         gallup.20040905.repouts, gallup.20040905.demouts,
         gallup.20040905.ins, gallup.20040905.outs)
    
      # February 6, 2005 ----
      gallup.20050206 <- read_ascii("Data/Surveys/g200509.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,129,130,87,88),
                                    var_widths = c(4,1,1,1,1))
      gallup.20050206.repins <- gallup.20050206 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20050206.demins <- gallup.20050206 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050206.repouts <- gallup.20050206 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050206.demouts <- gallup.20050206 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20050206.ins <- rbind(gallup.20050206.repins, gallup.20050206.demins)
      wpct(gallup.20050206.ins$fav, as.numeric(gallup.20050206.ins$weight)) # 87.40
      length(gallup.20050206.ins$weight) # 918
      
      gallup.20050206.outs <- rbind(gallup.20050206.repouts, gallup.20050206.demouts)
      wpct(gallup.20050206.outs$fav, as.numeric(gallup.20050206.outs$weight)) # 19.60
      length(gallup.20050206.outs$weight) # 900
      
      rm(gallup.20050206, 
         gallup.20050206.repins, gallup.20050206.demins,
         gallup.20050206.repouts, gallup.20050206.demouts,
         gallup.20050206.ins, gallup.20050206.outs)
  
      # February 27, 2005 ----
      gallup.20050227 <- read_ascii("Data/Surveys/g200512.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,127,128,87,88),
                                    var_widths = c(4,1,1,1,1))
      gallup.20050227.repins <- gallup.20050227 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20050227.demins <- gallup.20050227 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050227.repouts <- gallup.20050227 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050227.demouts <- gallup.20050227 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20050227.ins <- rbind(gallup.20050227.repins, gallup.20050227.demins)
      wpct(gallup.20050227.ins$fav, as.numeric(gallup.20050227.ins$weight)) # 90.70
      length(gallup.20050227.ins$weight) # 897
      
      gallup.20050227.outs <- rbind(gallup.20050227.repouts, gallup.20050227.demouts)
      wpct(gallup.20050227.outs$fav, as.numeric(gallup.20050227.outs$weight)) # 21.24
      length(gallup.20050227.outs$weight) # 879
      
      rm(gallup.20050227, 
         gallup.20050227.repins, gallup.20050227.demins,
         gallup.20050227.repouts, gallup.20050227.demouts,
         gallup.20050227.ins, gallup.20050227.outs)
    
      # April 2, 2005 ----
      gallup.20050402 <- read_ascii("Data/Surveys/g200518.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,127,128,87,88),
                                    var_widths = c(4,1,1,1,1))
      gallup.20050402.repins <- gallup.20050402 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20050402.demins <- gallup.20050402 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050402.repouts <- gallup.20050402 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050402.demouts <- gallup.20050402 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20050402.ins <- rbind(gallup.20050402.repins, gallup.20050402.demins)
      wpct(gallup.20050402.ins$fav, as.numeric(gallup.20050402.ins$weight)) # 85.58
      length(gallup.20050402.ins$weight) # 925
      
      gallup.20050402.outs <- rbind(gallup.20050402.repouts, gallup.20050402.demouts)
      wpct(gallup.20050402.outs$fav, as.numeric(gallup.20050402.outs$weight)) # 24.24
      length(gallup.20050402.outs$weight) # 917
      
      rm(gallup.20050402, 
         gallup.20050402.repins, gallup.20050402.demins,
         gallup.20050402.repouts, gallup.20050402.demouts,
         gallup.20050402.ins, gallup.20050402.outs)
  
      # July 24, 2005 ----
      gallup.20050724 <- read_ascii("Data/Surveys/g200535.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,123,124,89,90),
                                    var_widths = c(4,1,1,1,1))
      gallup.20050724.repins <- gallup.20050724 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20050724.demins <- gallup.20050724 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050724.repouts <- gallup.20050724 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050724.demouts <- gallup.20050724 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20050724.ins <- rbind(gallup.20050724.repins, gallup.20050724.demins)
      wpct(gallup.20050724.ins$fav, as.numeric(gallup.20050724.ins$weight)) # 90.90
      length(gallup.20050724.ins$weight) # 888
      
      gallup.20050724.outs <- rbind(gallup.20050724.repouts, gallup.20050724.demouts)
      wpct(gallup.20050724.outs$fav, as.numeric(gallup.20050724.outs$weight)) # 21.06
      length(gallup.20050724.outs$weight) # 874
      
      rm(gallup.20050724, 
         gallup.20050724.repins, gallup.20050724.demins,
         gallup.20050724.repouts, gallup.20050724.demouts,
         gallup.20050724.ins, gallup.20050724.outs)
    
      # Sepember 11, 2005 ----
      gallup.20050911 <- read_ascii("Data/Surveys/g200542.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,134,135,87,88),
                                    var_widths = c(4,1,1,1,1))
      gallup.20050911.repins <- gallup.20050911 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20050911.demins <- gallup.20050911 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050911.repouts <- gallup.20050911 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20050911.demouts <- gallup.20050911 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20050911.ins <- rbind(gallup.20050911.repins, gallup.20050911.demins)
      wpct(gallup.20050911.ins$fav, as.numeric(gallup.20050911.ins$weight)) # 88.88
      length(gallup.20050911.ins$weight) # 866
      
      gallup.20050911.outs <- rbind(gallup.20050911.repouts, gallup.20050911.demouts)
      wpct(gallup.20050911.outs$fav, as.numeric(gallup.20050911.outs$weight)) # 19.10
      length(gallup.20050911.outs$weight) # 849
      
      rm(gallup.20050911, 
         gallup.20050911.repins, gallup.20050911.demins,
         gallup.20050911.repouts, gallup.20050911.demouts,
         gallup.20050911.ins, gallup.20050911.outs)
  
      # December 18, 2005 ----
      gallup.20051218 <- read_ascii("Data/Surveys/g200561.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,128,129,89,90),
                                    var_widths = c(4,1,1,1,1))
      gallup.20051218.repins <- gallup.20051218 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20051218.demins <- gallup.20051218 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20051218.repouts <- gallup.20051218 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20051218.demouts <- gallup.20051218 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20051218.ins <- rbind(gallup.20051218.repins, gallup.20051218.demins)
      wpct(gallup.20051218.ins$fav, as.numeric(gallup.20051218.ins$weight)) # 84.17
      length(gallup.20051218.ins$weight) # 873
      
      gallup.20051218.outs <- rbind(gallup.20051218.repouts, gallup.20051218.demouts)
      wpct(gallup.20051218.outs$fav, as.numeric(gallup.20051218.outs$weight)) # 21.48
      length(gallup.20051218.outs$weight) # 864
      
      rm(gallup.20051218, 
         gallup.20051218.repins, gallup.20051218.demins,
         gallup.20051218.repouts, gallup.20051218.demouts,
         gallup.20051218.ins, gallup.20051218.outs)
      
      # April 30, 2006 ----
      gallup.20060430 <- read_ascii("Data/Surveys/g200617.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,126,127,86,87),
                                    var_widths = c(4,1,1,1,1))
      gallup.20060430.repins <- gallup.20060430 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20060430.demins <- gallup.20060430 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20060430.repouts <- gallup.20060430 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20060430.demouts <- gallup.20060430 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20060430.ins <- rbind(gallup.20060430.repins, gallup.20060430.demins)
      wpct(gallup.20060430.ins$fav, as.numeric(gallup.20060430.ins$weight)) # 76.64
      length(gallup.20060430.ins$weight) # 896
      
      gallup.20060430.outs <- rbind(gallup.20060430.repouts, gallup.20060430.demouts)
      wpct(gallup.20060430.outs$fav, as.numeric(gallup.20060430.outs$weight)) # 15.34
      length(gallup.20060430.outs$weight) # 911
      
      rm(gallup.20060430, 
         gallup.20060430.repins, gallup.20060430.demins,
         gallup.20060430.repouts, gallup.20060430.demouts,
         gallup.20060430.ins, gallup.20060430.outs)
  
      # July 30, 2006 ----
      gallup.20060730 <- read_ascii("Data/Surveys/g200629.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(5,115,116,81,82),
                                    var_widths = c(4,1,1,1,1))
      gallup.20060730.repins <- gallup.20060730 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20060730.demins <- gallup.20060730 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20060730.repouts <- gallup.20060730 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20060730.demouts <- gallup.20060730 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20060730.ins <- rbind(gallup.20060730.repins, gallup.20060730.demins)
      wpct(gallup.20060730.ins$fav, as.numeric(gallup.20060730.ins$weight)) # 90.15
      length(gallup.20060730.ins$weight) # 888
      
      gallup.20060730.outs <- rbind(gallup.20060730.repouts, gallup.20060730.demouts)
      wpct(gallup.20060730.outs$fav, as.numeric(gallup.20060730.outs$weight)) # 13.66
      length(gallup.20060730.outs$weight) # 875
      
      rm(gallup.20060730, 
         gallup.20060730.repins, gallup.20060730.demins,
         gallup.20060730.repouts, gallup.20060730.demouts,
         gallup.20060730.ins, gallup.20060730.outs)
  
      # October 8, 2006 ----
      gallup.20061008 <- read_ascii("Data/Surveys/G200640A.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(5,128,129,82,83),
                                    var_widths = c(4,1,1,1,1))
      gallup.20061008.repins <- gallup.20061008 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20061008.demins <- gallup.20061008 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20061008.repouts <- gallup.20061008 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20061008.demouts <- gallup.20061008 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20061008.ins <- rbind(gallup.20061008.repins, gallup.20061008.demins)
      wpct(gallup.20061008.ins$fav, as.numeric(gallup.20061008.ins$weight)) # 84.78
      length(gallup.20061008.ins$weight) # 878
      
      gallup.20061008.outs <- rbind(gallup.20061008.repouts, gallup.20061008.demouts)
      wpct(gallup.20061008.outs$fav, as.numeric(gallup.20061008.outs$weight)) # 17.11
      length(gallup.20061008.outs$weight) # 861
      
      rm(gallup.20061008, 
         gallup.20061008.repins, gallup.20061008.demins,
         gallup.20061008.repouts, gallup.20061008.demouts,
         gallup.20061008.ins, gallup.20061008.outs)
      
      # April 15, 2007 ----
      gallup.20070415 <- read_ascii("Data/Surveys/a200713.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(5,55,56,123,124),
                                    var_widths = c(4,1,1,1,1))
      gallup.20070415.repins <- gallup.20070415 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20070415.demins <- gallup.20070415 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20070415.repouts <- gallup.20070415 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20070415.demouts <- gallup.20070415 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20070415.ins <- rbind(gallup.20070415.repins, gallup.20070415.demins)
      wpct(gallup.20070415.ins$fav, as.numeric(gallup.20070415.ins$weight)) # 92.88
      length(gallup.20070415.ins$weight) # 905
      
      gallup.20070415.outs <- rbind(gallup.20070415.repouts, gallup.20070415.demouts)
      wpct(gallup.20070415.outs$fav, as.numeric(gallup.20070415.outs$weight)) # 15.41
      length(gallup.20070415.outs$weight) # 886
      
      rm(gallup.20070415, 
         gallup.20070415.repins, gallup.20070415.demins,
         gallup.20070415.repouts, gallup.20070415.demouts,
         gallup.20070415.ins, gallup.20070415.outs)
  
      # July 8, 2007 ----
      gallup.20070708 <- read_ascii("Data/Surveys/a200723.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(5,47,48,156,157),
                                    var_widths = c(4,1,1,1,1))
      gallup.20070708.repins <- gallup.20070708 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20070708.demins <- gallup.20070708 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20070708.repouts <- gallup.20070708 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20070708.demouts <- gallup.20070708 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20070708.ins <- rbind(gallup.20070708.repins, gallup.20070708.demins)
      wpct(gallup.20070708.ins$fav, as.numeric(gallup.20070708.ins$weight)) # 85.50
      length(gallup.20070708.ins$weight) # 869
      
      gallup.20070708.outs <- rbind(gallup.20070708.repouts, gallup.20070708.demouts)
      wpct(gallup.20070708.outs$fav, as.numeric(gallup.20070708.outs$weight)) # 14.83
      length(gallup.20070708.outs$weight) # 866
      
      rm(gallup.20070708, 
         gallup.20070708.repins, gallup.20070708.demins,
         gallup.20070708.repouts, gallup.20070708.demouts,
         gallup.20070708.ins, gallup.20070708.outs)
    
      # November 4, 2007 ----
      gallup.20071104 <- read_ascii("Data/Surveys/g200736.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,132,133,93,94),
                                    var_widths = c(4,1,1,1,1))
      gallup.20071104.repins <- gallup.20071104 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20071104.demins <- gallup.20071104 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20071104.repouts <- gallup.20071104 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20071104.demouts <- gallup.20071104 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20071104.ins <- rbind(gallup.20071104.repins, gallup.20071104.demins)
      wpct(gallup.20071104.ins$fav, as.numeric(gallup.20071104.ins$weight)) # 93.48
      length(gallup.20071104.ins$weight) # 883
      
      gallup.20071104.outs <- rbind(gallup.20071104.repouts, gallup.20071104.demouts)
      wpct(gallup.20071104.outs$fav, as.numeric(gallup.20071104.outs$weight)) # 15.54
      length(gallup.20071104.outs$weight) # 876
      
      rm(gallup.20071104, 
         gallup.20071104.repins, gallup.20071104.demins,
         gallup.20071104.repouts, gallup.20071104.demouts,
         gallup.20071104.ins, gallup.20071104.outs)
    
      # January 13, 2008 ----
      gallup.20080113 <- read_ascii("Data/Surveys/g200803.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,134,135,93,94),
                                    var_widths = c(4,1,1,1,1))
      gallup.20080113.repins <- gallup.20080113 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20080113.demins <- gallup.20080113 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080113.repouts <- gallup.20080113 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080113.demouts <- gallup.20080113 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20080113.ins <- rbind(gallup.20080113.repins, gallup.20080113.demins)
      wpct(gallup.20080113.ins$fav, as.numeric(gallup.20080113.ins$weight)) # 91.45
      length(gallup.20080113.ins$weight) # 1749
      
      gallup.20080113.outs <- rbind(gallup.20080113.repouts, gallup.20080113.demouts)
      wpct(gallup.20080113.outs$fav, as.numeric(gallup.20080113.outs$weight)) # 16.71
      length(gallup.20080113.outs$weight) # 1739
      
      rm(gallup.20080113, 
         gallup.20080113.repins, gallup.20080113.demins,
         gallup.20080113.repouts, gallup.20080113.demouts,
         gallup.20080113.ins, gallup.20080113.outs)
  
      # February 10, 2008 ----
      gallup.20080210 <- read_ascii("Data/Surveys/g200809.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,126,127,87,88),
                                    var_widths = c(4,1,1,1,1))
      gallup.20080210.repins <- gallup.20080210 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20080210.demins <- gallup.20080210 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080210.repouts <- gallup.20080210 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080210.demouts <- gallup.20080210 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20080210.ins <- rbind(gallup.20080210.repins, gallup.20080210.demins)
      wpct(gallup.20080210.ins$fav, as.numeric(gallup.20080210.ins$weight)) # 90.20
      length(gallup.20080210.ins$weight) # 921
      
      gallup.20080210.outs <- rbind(gallup.20080210.repouts, gallup.20080210.demouts)
      wpct(gallup.20080210.outs$fav, as.numeric(gallup.20080210.outs$weight)) # 15.93
      length(gallup.20080210.outs$weight) # 895
      
      rm(gallup.20080210, 
         gallup.20080210.repins, gallup.20080210.demins,
         gallup.20080210.repouts, gallup.20080210.demouts,
         gallup.20080210.ins, gallup.20080210.outs)
    
      # April 20, 2008 ----
      gallup.20080420 <- read_ascii("Data/Surveys/usa200816.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,134,135,90,91),
                                    var_widths = c(4,1,1,1,1))
      gallup.20080420.repins <- gallup.20080420 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20080420.demins <- gallup.20080420 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080420.repouts <- gallup.20080420 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080420.demouts <- gallup.20080420 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20080420.ins <- rbind(gallup.20080420.repins, gallup.20080420.demins)
      wpct(gallup.20080420.ins$fav, as.numeric(gallup.20080420.ins$weight)) # 84.33
      length(gallup.20080420.ins$weight) # 919
      
      gallup.20080420.outs <- rbind(gallup.20080420.repouts, gallup.20080420.demouts)
      wpct(gallup.20080420.outs$fav, as.numeric(gallup.20080420.outs$weight)) # 19.22
      length(gallup.20080420.outs$weight) # 906
      
      rm(gallup.20080420, 
         gallup.20080420.repins, gallup.20080420.demins,
         gallup.20080420.repouts, gallup.20080420.demouts,
         gallup.20080420.ins, gallup.20080420.outs)
    
      # August 23, 2008 ----
      gallup.20080823 <- read_ascii("Data/Surveys/usa200829.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,146,147,92,93),
                                    var_widths = c(4,1,1,1,1))
      gallup.20080823.repins <- gallup.20080823 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20080823.demins <- gallup.20080823 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080823.repouts <- gallup.20080823 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080823.demouts <- gallup.20080823 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20080823.ins <- rbind(gallup.20080823.repins, gallup.20080823.demins)
      wpct(gallup.20080823.ins$fav, as.numeric(gallup.20080823.ins$weight)) # 87.02
      length(gallup.20080823.ins$weight) # 904
      
      gallup.20080823.outs <- rbind(gallup.20080823.repouts, gallup.20080823.demouts)
      wpct(gallup.20080823.outs$fav, as.numeric(gallup.20080823.outs$weight)) # 18.44
      length(gallup.20080823.outs$weight) # 891
      
      rm(gallup.20080823, 
         gallup.20080823.repins, gallup.20080823.demins,
         gallup.20080823.repouts, gallup.20080823.demouts,
         gallup.20080823.ins, gallup.20080823.outs)
    
      # August 31, 2008 ----
      gallup.20080831 <- read_ascii("Data/Surveys/usa200831a.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,131,132,116,117),
                                    var_widths = c(5,1,1,1,1))
      gallup.20080831.repins <- gallup.20080831 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20080831.demins <- gallup.20080831 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080831.repouts <- gallup.20080831 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080831.demouts <- gallup.20080831 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20080831.ins <- rbind(gallup.20080831.repins, gallup.20080831.demins)
      wpct(gallup.20080831.ins$fav, as.numeric(gallup.20080831.ins$weight)) # 91.07
      length(gallup.20080831.ins$weight) # 1808
      
      gallup.20080831.outs <- rbind(gallup.20080831.repouts, gallup.20080831.demouts)
      wpct(gallup.20080831.outs$fav, as.numeric(gallup.20080831.outs$weight)) # 12.19
      length(gallup.20080831.outs$weight) # 1775
      
      rm(gallup.20080831, 
         gallup.20080831.repins, gallup.20080831.demins,
         gallup.20080831.repouts, gallup.20080831.demouts,
         gallup.20080831.ins, gallup.20080831.outs)
    
      # September 7, 2008 ----
      gallup.20080907 <- read_ascii("Data/Surveys/usa200831b.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,140,141,92,93),
                                    var_widths = c(4,1,1,1,1))
      gallup.20080907.repins <- gallup.20080907 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20080907.demins <- gallup.20080907 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080907.repouts <- gallup.20080907 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20080907.demouts <- gallup.20080907 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20080907.ins <- rbind(gallup.20080907.repins, gallup.20080907.demins)
      wpct(gallup.20080907.ins$fav, as.numeric(gallup.20080907.ins$weight)) # 89.69
      length(gallup.20080907.ins$weight) # 928
      
      gallup.20080907.outs <- rbind(gallup.20080907.repouts, gallup.20080907.demouts)
      wpct(gallup.20080907.outs$fav, as.numeric(gallup.20080907.outs$weight)) # 18.90
      length(gallup.20080907.outs$weight) # 922
      
      rm(gallup.20080907, 
         gallup.20080907.repins, gallup.20080907.demins,
         gallup.20080907.repouts, gallup.20080907.demouts,
         gallup.20080907.ins, gallup.20080907.outs)
  
      # October 12, 2008 ----
      gallup.20081012 <- read_ascii("Data/Surveys/usa200839.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,144,145,90,91),
                                    var_widths = c(4,1,1,1,1))
      gallup.20081012.repins <- gallup.20081012 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20081012.demins <- gallup.20081012 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20081012.repouts <- gallup.20081012 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20081012.demouts <- gallup.20081012 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20081012.ins <- rbind(gallup.20081012.repins, gallup.20081012.demins)
      wpct(gallup.20081012.ins$fav, as.numeric(gallup.20081012.ins$weight)) # 87.93
      length(gallup.20081012.ins$weight) # 1135
      
      gallup.20081012.outs <- rbind(gallup.20081012.repouts, gallup.20081012.demouts)
      wpct(gallup.20081012.outs$fav, as.numeric(gallup.20081012.outs$weight)) # 16.68
      length(gallup.20081012.outs$weight) # 1138
      
      rm(gallup.20081012, 
         gallup.20081012.repins, gallup.20081012.demins,
         gallup.20081012.repouts, gallup.20081012.demouts,
         gallup.20081012.ins, gallup.20081012.outs)
    
      # November 16, 2008 ----
      gallup.20081116 <- read_ascii("Data/Surveys/g200845.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,140,141,88,89),
                                    var_widths = c(4,1,1,1,1))
      gallup.20081116.repins <- gallup.20081116 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20081116.demins <- gallup.20081116 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20081116.repouts <- gallup.20081116 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20081116.demouts <- gallup.20081116 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20081116.ins <- rbind(gallup.20081116.repins, gallup.20081116.demins)
      wpct(gallup.20081116.ins$fav, as.numeric(gallup.20081116.ins$weight)) # 86.86
      length(gallup.20081116.ins$weight) # 904
      
      gallup.20081116.outs <- rbind(gallup.20081116.repouts, gallup.20081116.demouts)
      wpct(gallup.20081116.outs$fav, as.numeric(gallup.20081116.outs$weight)) # 13.64
      length(gallup.20081116.outs$weight) # 904
      
      rm(gallup.20081116, 
         gallup.20081116.repins, gallup.20081116.demins,
         gallup.20081116.repouts, gallup.20081116.demouts,
         gallup.20081116.ins, gallup.20081116.outs)
  
      # May 31, 2009 ----
      gallup.20090531 <- read_ascii("Data/Surveys/usa200910.dat", # Good
                                    total_cards = 1, 
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,161,160,88,89),
                                    var_widths = c(4,1,1,1,1))
      gallup.20090531.repins <- gallup.20090531 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20090531.demins <- gallup.20090531 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20090531.repouts <- gallup.20090531 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20090531.demouts <- gallup.20090531 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20090531.ins <- rbind(gallup.20090531.repins, gallup.20090531.demins)
      wpct(gallup.20090531.ins$fav, as.numeric(gallup.20090531.ins$weight)) # 81.97
      length(gallup.20090531.ins$weight) # 870
      
      gallup.20090531.outs <- rbind(gallup.20090531.repouts, gallup.20090531.demouts)
      wpct(gallup.20090531.outs$fav, as.numeric(gallup.20090531.outs$weight)) # 18.17
      length(gallup.20090531.outs$weight) # 872
      
      rm(gallup.20090531, 
         gallup.20090531.repins, gallup.20090531.demins,
         gallup.20090531.repouts, gallup.20090531.demouts,
         gallup.20090531.ins, gallup.20090531.outs)
    
      # March 28, 2010 ----
      gallup.20100328 <- read_ascii("Data/Surveys/g201004.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,151,152,88,89),
                                    var_widths = c(4,1,1,1,1))
      gallup.20100328.repins <- gallup.20100328 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20100328.demins <- gallup.20100328 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20100328.repouts <- gallup.20100328 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20100328.demouts <- gallup.20100328 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20100328.ins <- rbind(gallup.20100328.repins, gallup.20100328.demins)
      wpct(gallup.20100328.ins$fav, as.numeric(gallup.20100328.ins$weight)) # 80.64
      length(gallup.20100328.ins$weight) # 929
      
      gallup.20100328.outs <- rbind(gallup.20100328.repouts, gallup.20100328.demouts)
      wpct(gallup.20100328.outs$fav, as.numeric(gallup.20100328.outs$weight)) # 12.69
      length(gallup.20100328.outs$weight) # 919
      
      rm(gallup.20100328, 
         gallup.20100328.repins, gallup.20100328.demins,
         gallup.20100328.repouts, gallup.20100328.demouts,
         gallup.20100328.ins, gallup.20100328.outs)
    
      # May 25, 2010 ----
      gallup.20100525 <- read_ascii("Data/Surveys/g201007.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,133,134,88,89),
                                    var_widths = c(4,1,1,1,1))
      gallup.20100525.repins <- gallup.20100525 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20100525.demins <- gallup.20100525 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20100525.repouts <- gallup.20100525 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20100525.demouts <- gallup.20100525 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20100525.ins <- rbind(gallup.20100525.repins, gallup.20100525.demins)
      wpct(gallup.20100525.ins$fav, as.numeric(gallup.20100525.ins$weight)) # 73.54
      length(gallup.20100525.ins$weight) # 930
      
      gallup.20100525.outs <- rbind(gallup.20100525.repouts, gallup.20100525.demouts)
      wpct(gallup.20100525.outs$fav, as.numeric(gallup.20100525.outs$weight)) # 18.95
      length(gallup.20100525.outs$weight) # 938
      
      rm(gallup.20100525, 
         gallup.20100525.repins, gallup.20100525.demins,
         gallup.20100525.repouts, gallup.20100525.demouts,
         gallup.20100525.ins, gallup.20100525.outs)
    
      # November 17, 2010 ----
      gallup.20101117 <- read_ascii("Data/Surveys/g201020.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,133,134,88,89),
                                    var_widths = c(4,1,1,1,1))
      gallup.20101117.repins <- gallup.20101117 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20101117.demins <- gallup.20101117 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20101117.repouts <- gallup.20101117 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20101117.demouts <- gallup.20101117 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20101117.ins <- rbind(gallup.20101117.repins, gallup.20101117.demins)
      wpct(gallup.20101117.ins$fav, as.numeric(gallup.20101117.ins$weight)) # 81.71
      length(gallup.20101117.ins$weight) # 903
      
      gallup.20101117.outs <- rbind(gallup.20101117.repouts, gallup.20101117.demouts)
      wpct(gallup.20101117.outs$fav, as.numeric(gallup.20101117.outs$weight)) # 14.93
      length(gallup.20101117.outs$weight) # 906
      
      rm(gallup.20101117, 
         gallup.20101117.repins, gallup.20101117.demins,
         gallup.20101117.repouts, gallup.20101117.demouts,
         gallup.20101117.ins, gallup.20101117.outs)
  
      # January 16, 2010 ----
      gallup.20110116 <- read_ascii("Data/Surveys/u201102.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,125,126,88,89),
                                    var_widths = c(4,1,1,1,1))
      gallup.20110116.repins <- gallup.20110116 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20110116.demins <- gallup.20110116 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20110116.repouts <- gallup.20110116 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20110116.demouts <- gallup.20110116 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20110116.ins <- rbind(gallup.20110116.repins, gallup.20110116.demins)
      wpct(gallup.20110116.ins$fav, as.numeric(gallup.20110116.ins$weight)) # 87.31
      length(gallup.20110116.ins$weight) # 895
      
      gallup.20110116.outs <- rbind(gallup.20110116.repouts, gallup.20110116.demouts)
      wpct(gallup.20110116.outs$fav, as.numeric(gallup.20110116.outs$weight)) # 18.73
      length(gallup.20110116.outs$weight) # 871
      
      rm(gallup.20110116, 
         gallup.20110116.repins, gallup.20110116.demins,
         gallup.20110116.repouts, gallup.20110116.demouts,
         gallup.20110116.ins, gallup.20110116.outs)
    
      # April 23, 2010 ----
      gallup.20110423 <- read_ascii("Data/Surveys/usa201107.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,129,130,94,95),
                                    var_widths = c(4,1,1,1,1))
      gallup.20110423.repins <- gallup.20110423 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20110423.demins <- gallup.20110423 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20110423.repouts <- gallup.20110423 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20110423.demouts <- gallup.20110423 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20110423.ins <- rbind(gallup.20110423.repins, gallup.20110423.demins)
      wpct(gallup.20110423.ins$fav, as.numeric(gallup.20110423.ins$weight)) # 79.95
      length(gallup.20110423.ins$weight) # 876
      
      gallup.20110423.outs <- rbind(gallup.20110423.repouts, gallup.20110423.demouts)
      wpct(gallup.20110423.outs$fav, as.numeric(gallup.20110423.outs$weight)) # 21.02
      length(gallup.20110423.outs$weight) # 883
      
      rm(gallup.20110423, 
         gallup.20110423.repins, gallup.20110423.demins,
         gallup.20110423.repouts, gallup.20110423.demouts,
         gallup.20110423.ins, gallup.20110423.outs)
    
      # February 5, 2012 ----
      gallup.20120205 <- read_ascii("Data/Surveys/g201202.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,141,142,91,92),
                                    var_widths = c(4,1,1,1,1))
      gallup.20120205.repins <- gallup.20120205 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20120205.demins <- gallup.20120205 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20120205.repouts <- gallup.20120205 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20120205.demouts <- gallup.20120205 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20120205.ins <- rbind(gallup.20120205.repins, gallup.20120205.demins)
      wpct(gallup.20120205.ins$fav, as.numeric(gallup.20120205.ins$weight)) # 84.21
      length(gallup.20120205.ins$weight) # 922
      
      gallup.20120205.outs <- rbind(gallup.20120205.repouts, gallup.20120205.demouts)
      wpct(gallup.20120205.outs$fav, as.numeric(gallup.20120205.outs$weight)) # 15.30
      length(gallup.20120205.outs$weight) # 918
      
      rm(gallup.20120205, 
         gallup.20120205.repins, gallup.20120205.demins,
         gallup.20120205.repouts, gallup.20120205.demouts,
         gallup.20120205.ins, gallup.20120205.outs)
    
      # February 19, 2012 ----
      gallup.20120219 <- read_ascii("Data/Surveys/usa201203.dat", # Good
                                    total_cards = 1, 
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,126,127,91,92),
                                    var_widths = c(4,1,1,1,1))
      gallup.20120219.repins <- gallup.20120219 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20120219.demins <- gallup.20120219 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20120219.repouts <- gallup.20120219 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20120219.demouts <- gallup.20120219 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20120219.ins <- rbind(gallup.20120219.repins, gallup.20120219.demins)
      wpct(gallup.20120219.ins$fav, as.numeric(gallup.20120219.ins$weight)) # 85.09
      length(gallup.20120219.ins$weight) # 880
      
      gallup.20120219.outs <- rbind(gallup.20120219.repouts, gallup.20120219.demouts)
      wpct(gallup.20120219.outs$fav, as.numeric(gallup.20120219.outs$weight)) # 14.15
      length(gallup.20120219.outs$weight) # 874
      
      rm(gallup.20120219, 
         gallup.20120219.repins, gallup.20120219.demins,
         gallup.20120219.repouts, gallup.20120219.demouts,
         gallup.20120219.ins, gallup.20120219.outs)
    
      # August 22, 2012 ----
      gallup.20120822 <- read_ascii("Data/Surveys/u201212.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,142,143,95,96),
                                    var_widths = c(4,1,1,1,1))
      gallup.20120822.repins <- gallup.20120822 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20120822.demins <- gallup.20120822 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20120822.repouts <- gallup.20120822 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20120822.demouts <- gallup.20120822 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20120822.ins <- rbind(gallup.20120822.repins, gallup.20120822.demins)
      wpct(gallup.20120822.ins$fav, as.numeric(gallup.20120822.ins$weight)) # 82.12
      length(gallup.20120822.ins$weight) # 926
      
      gallup.20120822.outs <- rbind(gallup.20120822.repouts, gallup.20120822.demouts)
      wpct(gallup.20120822.outs$fav, as.numeric(gallup.20120822.outs$weight)) # 14.18
      length(gallup.20120822.outs$weight) # 928
      
      rm(gallup.20120822, 
         gallup.20120822.repins, gallup.20120822.demins,
         gallup.20120822.repouts, gallup.20120822.demouts,
         gallup.20120822.ins, gallup.20120822.outs)
  
      # November 12, 2012 ----
      gallup.20121112 <- read_ascii("Data/Surveys/usa201217.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,141,142,101,102),
                                    var_widths = c(4,1,1,1,1))
      gallup.20121112.repins <- gallup.20121112 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20121112.demins <- gallup.20121112 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20121112.repouts <- gallup.20121112 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20121112.demouts <- gallup.20121112 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20121112.ins <- rbind(gallup.20121112.repins, gallup.20121112.demins)
      wpct(gallup.20121112.ins$fav, as.numeric(gallup.20121112.ins$weight)) # 88.42
      length(gallup.20121112.ins$weight) # 890
      
      gallup.20121112.outs <- rbind(gallup.20121112.repouts, gallup.20121112.demouts)
      wpct(gallup.20121112.outs$fav, as.numeric(gallup.20121112.outs$weight)) # 14.98
      length(gallup.20121112.outs$weight) # 878
      
      rm(gallup.20121112, 
         gallup.20121112.repins, gallup.20121112.demins,
         gallup.20121112.repouts, gallup.20121112.demouts,
         gallup.20121112.ins, gallup.20121112.outs)
    
      # June 4, 2013 ----
      gallup.20130604 <- read_ascii("Data/Surveys/g201307.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,175,174,108,109),
                                    var_widths = c(4,1,1,1,1))
      gallup.20130604.repins <- gallup.20130604 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20130604.demins <- gallup.20130604 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20130604.repouts <- gallup.20130604 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20130604.demouts <- gallup.20130604 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20130604.ins <- rbind(gallup.20130604.repins, gallup.20130604.demins)
      wpct(gallup.20130604.ins$fav, as.numeric(gallup.20130604.ins$weight)) # 82.07
      length(gallup.20130604.ins$weight) # 1322
      
      gallup.20130604.outs <- rbind(gallup.20130604.repouts, gallup.20130604.demouts)
      wpct(gallup.20130604.outs$fav, as.numeric(gallup.20130604.outs$weight)) # 16.92
      length(gallup.20130604.outs$weight) # 1308
      
      rm(gallup.20130604, 
         gallup.20130604.repins, gallup.20130604.demins,
         gallup.20130604.repouts, gallup.20130604.demouts,
         gallup.20130604.ins, gallup.20130604.outs)
    
      # October 6, 2013 ----
      gallup.20131006 <- read_ascii("Data/Surveys/g201316.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,155,156,104,105),
                                    var_widths = c(4,1,1,1,1))
      gallup.20131006.repins <- gallup.20131006 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20131006.demins <- gallup.20131006 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20131006.repouts <- gallup.20131006 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20131006.demouts <- gallup.20131006 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20131006.ins <- rbind(gallup.20131006.repins, gallup.20131006.demins)
      wpct(gallup.20131006.ins$fav, as.numeric(gallup.20131006.ins$weight)) # 77.36
      length(gallup.20131006.ins$weight) # 848
      
      gallup.20131006.outs <- rbind(gallup.20131006.repouts, gallup.20131006.demouts)
      wpct(gallup.20131006.outs$fav, as.numeric(gallup.20131006.outs$weight)) # 9.52
      length(gallup.20131006.outs$weight) # 852
      
      rm(gallup.20131006, 
         gallup.20131006.repins, gallup.20131006.demins,
         gallup.20131006.repouts, gallup.20131006.demouts,
         gallup.20131006.ins, gallup.20131006.outs)
  
      # December 8, 2013 ----
      gallup.20131208 <- read_ascii("Data/Surveys/g201321.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,213,214,108,109),
                                    var_widths = c(4,1,1,1,1))
      gallup.20131208.repins <- gallup.20131208 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20131208.demins <- gallup.20131208 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20131208.repouts <- gallup.20131208 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20131208.demouts <- gallup.20131208 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20131208.ins <- rbind(gallup.20131208.repins, gallup.20131208.demins)
      wpct(gallup.20131208.ins$fav, as.numeric(gallup.20131208.ins$weight)) # 75.71
      length(gallup.20131208.ins$weight) # 864
      
      gallup.20131208.outs <- rbind(gallup.20131208.repouts, gallup.20131208.demouts)
      wpct(gallup.20131208.outs$fav, as.numeric(gallup.20131208.outs$weight)) # 10.47
      length(gallup.20131208.outs$weight) # 865
      
      rm(gallup.20131208, 
         gallup.20131208.repins, gallup.20131208.demins,
         gallup.20131208.repouts, gallup.20131208.demouts,
         gallup.20131208.ins, gallup.20131208.outs)
    
      # April 30, 2014 ----
      gallup.20140430 <- read_ascii("Data/Surveys/g201405.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,143,144,108,109),
                                    var_widths = c(4,1,1,1,1))
      gallup.20140430.repins <- gallup.20140430 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20140430.demins <- gallup.20140430 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20140430.repouts <- gallup.20140430 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20140430.demouts <- gallup.20140430 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20140430.ins <- rbind(gallup.20140430.repins, gallup.20140430.demins)
      wpct(gallup.20140430.ins$fav, as.numeric(gallup.20140430.ins$weight)) # 75.23
      length(gallup.20140430.ins$weight) # 1303
      
      gallup.20140430.outs <- rbind(gallup.20140430.repouts, gallup.20140430.demouts)
      wpct(gallup.20140430.outs$fav, as.numeric(gallup.20140430.outs$weight)) # 13.08
      length(gallup.20140430.outs$weight) # 1302
      
      rm(gallup.20140430, 
         gallup.20140430.repins, gallup.20140430.demins,
         gallup.20140430.repouts, gallup.20140430.demouts,
         gallup.20140430.ins, gallup.20140430.outs)
  
      # November 9, 2014 ----
      gallup.20141109 <- read_ascii("Data/Surveys/g201414.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,151,152,109,110),
                                    var_widths = c(4,1,1,1,1))
      gallup.20141109.repins <- gallup.20141109 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20141109.demins <- gallup.20141109 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20141109.repouts <- gallup.20141109 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20141109.demouts <- gallup.20141109 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20141109.ins <- rbind(gallup.20141109.repins, gallup.20141109.demins)
      wpct(gallup.20141109.ins$fav, as.numeric(gallup.20141109.ins$weight)) # 76.33
      length(gallup.20141109.ins$weight) # 714
      
      gallup.20141109.outs <- rbind(gallup.20141109.repouts, gallup.20141109.demouts)
      wpct(gallup.20141109.outs$fav, as.numeric(gallup.20141109.outs$weight)) # 12.26
      length(gallup.20141109.outs$weight) # 712
      
      rm(gallup.20141109, 
         gallup.20141109.repins, gallup.20141109.demins,
         gallup.20141109.repouts, gallup.20141109.demouts,
         gallup.20141109.ins, gallup.20141109.outs)
      
      # March 8, 2015 ----
      gallup.20150308 <- read_ascii("Data/Surveys/g201503.dat", # Good
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(7,155,156,115,116),
                                    var_widths = c(4,1,1,1,1))
      gallup.20150308.repins <- gallup.20150308 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.20150308.demins <- gallup.20150308 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20150308.repouts <- gallup.20150308 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.20150308.demouts <- gallup.20150308 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.20150308.ins <- rbind(gallup.20150308.repins, gallup.20150308.demins)
      wpct(gallup.20150308.ins$fav, as.numeric(gallup.20150308.ins$weight)) # 74.21
      length(gallup.20150308.ins$weight) # 851
      
      gallup.20150308.outs <- rbind(gallup.20150308.repouts, gallup.20150308.demouts)
      wpct(gallup.20150308.outs$fav, as.numeric(gallup.20150308.outs$weight)) # 16.30
      length(gallup.20150308.outs$weight) # 858
      
      rm(gallup.20150308, 
         gallup.20150308.repins, gallup.20150308.demins,
         gallup.20150308.repouts, gallup.20150308.demouts,
         gallup.20150308.ins, gallup.20150308.outs)
  
    # 10-point scale ----
      # May 19, 1980 ----
      # gallup.19800519 <- read_ascii("Data/Surveys/a1155g.dat", # No PID Lean, +5 to -5
      #                               total_cards = 1,
      #                               var_names = c("weight", "rep.fav", "dem.fav","pid"),
      #                               var_positions = c(1,89,91,94),
      #                               var_widths = c(1,2,2,1))
      
      # October 13, 1980 ----
      gallup.19801013 <- read_ascii("Data/Surveys/1163.dat", # +5 to -5
                                    total_cards = 1,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                    var_positions = c(1,64,62,125,126),
                                    var_widths = c(1,2,2,1,1))
      gallup.19801013.repins <- gallup.19801013 %>%
        filter(pid == 1 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 1,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 2,
                                9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19801013.demins <- gallup.19801013 %>%
        filter(pid == 2 | pid.lean == 2) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 1,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 2,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19801013.repouts <- gallup.19801013 %>%
        filter(pid == 1 | pid.lean == 1) %>%
        mutate(dem.fav = ifelse(dem.fav == "01" | dem.fav == "02" | dem.fav == "03" | dem.fav == "04" | dem.fav == "05", 1,
                                ifelse(dem.fav == "06" | dem.fav == "07" | dem.fav == "08" | dem.fav == "09" | dem.fav == "10", 2,
                                       9))) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19801013.demouts <- gallup.19801013 %>%
        filter(pid == 2 | pid.lean == 2) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 1,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 2,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19801013.ins <- rbind(gallup.19801013.repins, gallup.19801013.demins)
      wpct(gallup.19801013.ins$fav, as.numeric(gallup.19801013.ins$weight)) # 68.74
      length(gallup.19801013.ins$weight) # 1374
      
      gallup.19801013.outs <- rbind(gallup.19801013.repouts, gallup.19801013.demouts)
      wpct(gallup.19801013.outs$fav, as.numeric(gallup.19801013.outs$weight)) # 54.49
      length(gallup.19801013.outs$weight) # 1370
      
      rm(gallup.19801013, 
         gallup.19801013.repins, gallup.19801013.demins,
         gallup.19801013.repouts, gallup.19801013.demouts,
         gallup.19801013.ins, gallup.19801013.outs)
      

      # March 16, 1981 ----
      # gallup.19810316 <- read_ascii("Data/Surveys/1170.dat", # No PID Lean, +5 to -5
      #                               total_cards = 1,
      #                               var_names = c("weight", "rep.fav", "dem.fav", "pid", "pty.lean"),
      #                               var_positions = c(1,78,76,54,54),
      #                               var_widths = c(3,2,2,1,1))
      # October 20, 1991 ----
      gallup.19911020 <- read_ascii("Data/Surveys/a222020.dat", # Good
                                    total_cards = 3,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,33,35,57,49),
                                    var_widths = c(3,2,2,1,1),
                                    var_cards = c(1,3,3,1,1))
      gallup.19911020.repins <- gallup.19911020 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19911020.demins <- gallup.19911020 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19911020.repouts <- gallup.19911020 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(dem.fav = ifelse(dem.fav == "01" | dem.fav == "02" | dem.fav == "03" | dem.fav == "04" | dem.fav == "05", 2,
                                ifelse(dem.fav == "06" | dem.fav == "07" | dem.fav == "08" | dem.fav == "09" | dem.fav == "10", 1,
                                       9))) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19911020.demouts <- gallup.19911020 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19911020.ins <- rbind(gallup.19911020.repins, gallup.19911020.demins)
      wpct(gallup.19911020.ins$fav, as.numeric(gallup.19911020.ins$weight)) # 71.05
      length(gallup.19911020.ins$weight) # 862
      
      gallup.19911020.outs <- rbind(gallup.19911020.repouts, gallup.19911020.demouts)
      wpct(gallup.19911020.outs$fav, as.numeric(gallup.19911020.outs$weight)) # 50.24
      length(gallup.19911020.outs$weight) # 858
      
      rm(gallup.19911020, 
         gallup.19911020.repins, gallup.19911020.demins,
         gallup.19911020.repouts, gallup.19911020.demouts,
         gallup.19911020.ins, gallup.19911020.outs)
      
  
      # December 6, 1992 ----
      gallup.19921206 <- read_ascii("Data/Surveys/322036.dat", # Good
                                    total_cards = 4,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,40,42,54,55),
                                    var_widths = c(3,2,2,1,1),
                                    var_cards = c(1,4,4,4,4))
      gallup.19921206.repins <- gallup.19921206 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19921206.demins <- gallup.19921206 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19921206.repouts <- gallup.19921206 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(dem.fav = ifelse(dem.fav == "01" | dem.fav == "02" | dem.fav == "03" | dem.fav == "04" | dem.fav == "05", 2,
                                ifelse(dem.fav == "06" | dem.fav == "07" | dem.fav == "08" | dem.fav == "09" | dem.fav == "10", 1,
                                       9))) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19921206.demouts <- gallup.19921206 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19921206.ins <- rbind(gallup.19921206.repins, gallup.19921206.demins)
      wpct(gallup.19921206.ins$fav, as.numeric(gallup.19921206.ins$weight)) # 62.97
      length(gallup.19921206.ins$weight) # 903
      
      gallup.19921206.outs <- rbind(gallup.19921206.repouts, gallup.19921206.demouts)
      wpct(gallup.19921206.outs$fav, as.numeric(gallup.19921206.outs$weight)) # 45.72
      length(gallup.19921206.outs$weight) # 897
      
      rm(gallup.19921206, 
         gallup.19921206.repins, gallup.19921206.demins,
         gallup.19921206.repouts, gallup.19921206.demouts,
         gallup.19921206.ins, gallup.19921206.outs)
      
  
      # September 24, 1984 ----
      # gallup.19840924 <- read_ascii("Data/Surveys/1242.dat", # No PID Lean, +5 to -5
      #                               total_cards = 1,
      #                               var_names = c("weight", "rep.fav", "dem.fav", "pid"),
      #                               var_positions = c(1,29,27,54),
      #                               var_widths = c(1,2,2,1))
      
      # November 29, 1994 ----
      gallup.19941129 <- read_ascii("Data/Surveys/a5001002.dat", # Good
                                    total_cards = 6,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,29,31,54,55),
                                    var_widths = c(3,2,2,1,1),
                                    var_cards = c(1,5,5,4,4))
      gallup.19941129.repins <- gallup.19941129 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19941129.demins <- gallup.19941129 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19941129.repouts <- gallup.19941129 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(dem.fav = ifelse(dem.fav == "01" | dem.fav == "02" | dem.fav == "03" | dem.fav == "04" | dem.fav == "05", 2,
                                ifelse(dem.fav == "06" | dem.fav == "07" | dem.fav == "08" | dem.fav == "09" | dem.fav == "10", 1,
                                       9))) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19941129.demouts <- gallup.19941129 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19941129.ins <- rbind(gallup.19941129.repins, gallup.19941129.demins)
      wpct(gallup.19941129.ins$fav, as.numeric(gallup.19941129.ins$weight)) # 71.86
      length(gallup.19941129.ins$weight) # 953
      
      gallup.19941129.outs <- rbind(gallup.19941129.repouts, gallup.19941129.demouts)
      wpct(gallup.19941129.outs$fav, as.numeric(gallup.19941129.outs$weight)) # 36.30
      length(gallup.19941129.outs$weight) # 954
      
      rm(gallup.19941129, 
         gallup.19941129.repins, gallup.19941129.demins,
         gallup.19941129.repouts, gallup.19941129.demouts,
         gallup.19941129.ins, gallup.19941129.outs)
  
      # November 15, 1998 ----
      gallup.19981115 <- read_ascii("Data/Surveys/a9811043.dat", # Good
                                    total_cards = 8,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,22,24,18,19),
                                    var_widths = c(3,2,2,1,1),
                                    var_cards = c(1,6,6,5,5))
      gallup.19981115.repins <- gallup.19981115 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19981115.demins <- gallup.19981115 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19981115.repouts <- gallup.19981115 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(dem.fav = ifelse(dem.fav == "01" | dem.fav == "02" | dem.fav == "03" | dem.fav == "04" | dem.fav == "05", 2,
                                ifelse(dem.fav == "06" | dem.fav == "07" | dem.fav == "08" | dem.fav == "09" | dem.fav == "10", 1,
                                       9))) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19981115.demouts <- gallup.19981115 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19981115.ins <- rbind(gallup.19981115.repins, gallup.19981115.demins)
      wpct(gallup.19981115.ins$fav, as.numeric(gallup.19981115.ins$weight)) # 59.17
      length(gallup.19981115.ins$weight) # 903
      
      gallup.19981115.outs <- rbind(gallup.19981115.repouts, gallup.19981115.demouts)
      wpct(gallup.19981115.outs$fav, as.numeric(gallup.19981115.outs$weight)) # 39.06
      length(gallup.19981115.outs$weight) # 931
      
      rm(gallup.19981115, 
         gallup.19981115.repins, gallup.19981115.demins,
         gallup.19981115.repouts, gallup.19981115.demouts,
         gallup.19981115.ins, gallup.19981115.outs)
  
      # December 13, 1998 ----
      gallup.19981213 <- read_ascii("Data/Surveys/a9812053.dat", # Good
                                    total_cards = 6,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,47,49,18,19),
                                    var_widths = c(3,2,2,1,1),
                                    var_cards = c(1,6,6,5,5))
      gallup.19981213.repins <- gallup.19981213 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19981213.demins <- gallup.19981213 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19981213.repouts <- gallup.19981213 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(dem.fav = ifelse(dem.fav == "01" | dem.fav == "02" | dem.fav == "03" | dem.fav == "04" | dem.fav == "05", 2,
                                ifelse(dem.fav == "06" | dem.fav == "07" | dem.fav == "08" | dem.fav == "09" | dem.fav == "10", 1,
                                       9))) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19981213.demouts <- gallup.19981213 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19981213.ins <- rbind(gallup.19981213.repins, gallup.19981213.demins)
      wpct(gallup.19981213.ins$fav, as.numeric(gallup.19981213.ins$weight)) # 59.24
      length(gallup.19981213.ins$weight) # 756
      
      gallup.19981213.outs <- rbind(gallup.19981213.repouts, gallup.19981213.demouts)
      wpct(gallup.19981213.outs$fav, as.numeric(gallup.19981213.outs$weight)) # 38.01
      length(gallup.19981213.outs$weight) # 754
      
      rm(gallup.19981213, 
         gallup.19981213.repins, gallup.19981213.demins,
         gallup.19981213.repouts, gallup.19981213.demouts,
         gallup.19981213.ins, gallup.19981213.outs)
  
      # January 17, 1999 ----
      gallup.19990117 <- read_ascii("Data/Surveys/a9901003.dat", # Good
                                    total_cards = 7,
                                    var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                    var_positions = c(13,65,67,18,19),
                                    var_widths = c(3,2,2,1,1),
                                    var_cards = c(1,6,6,5,5))
      gallup.19990117.repins <- gallup.19990117 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19990117.demins <- gallup.19990117 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      gallup.19990117.repouts <- gallup.19990117 %>%
        filter(pid == 1 | pid.lean == 2) %>%
        mutate(dem.fav = ifelse(dem.fav == "01" | dem.fav == "02" | dem.fav == "03" | dem.fav == "04" | dem.fav == "05", 2,
                                ifelse(dem.fav == "06" | dem.fav == "07" | dem.fav == "08" | dem.fav == "09" | dem.fav == "10", 1,
                                       9))) %>%
        filter(dem.fav == 1 | dem.fav == 2) %>%
        rename(fav = dem.fav) %>%
        select(weight, fav)
      gallup.19990117.demouts <- gallup.19990117 %>%
        filter(pid == 2 | pid.lean == 1) %>%
        mutate(rep.fav = ifelse(rep.fav == "01" | rep.fav == "02" | rep.fav == "03" | rep.fav == "04" | rep.fav == "05", 2,
                                ifelse(rep.fav == "06" | rep.fav == "07" | rep.fav == "08" | rep.fav == "09" | rep.fav == "10", 1,
                                       9))) %>%
        filter(rep.fav == 1 | rep.fav == 2) %>%
        rename(fav = rep.fav) %>%
        select(weight, fav)
      
      gallup.19990117.ins <- rbind(gallup.19990117.repins, gallup.19990117.demins)
      wpct(gallup.19990117.ins$fav, as.numeric(gallup.19990117.ins$weight)) #58.95
      length(gallup.19990117.ins$weight) # 900
      
      gallup.19990117.outs <- rbind(gallup.19990117.repouts, gallup.19990117.demouts)
      wpct(gallup.19990117.outs$fav, as.numeric(gallup.19990117.outs$weight)) # 39.96
      length(gallup.19990117.outs$weight) # 900
      
      rm(gallup.19990117, 
         gallup.19990117.repins, gallup.19990117.demins,
         gallup.19990117.repouts, gallup.19990117.demouts,
         gallup.19990117.ins, gallup.19990117.outs)
      
      
      
      
      # November 24, 2002 ----
      # gallup.20021124 <- read_ascii("Data/Surveys/2002_11_22x.dat", # Favorabilities not working
      #                               total_cards = 1,
      #                               var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
      #                               var_positions = c(7,127,129,78,79),
      #                               var_widths = c(4,2,2,1,1))
      
      # November 21, 2004 ----
      # gallup.20041121 <- read_ascii("Data/Surveys/g200444.dat", # Favorabilities not working
      #                               total_cards = 1,
      #                               var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
      #                               var_positions = c(7,141,143,87,88),
      #                               var_widths = c(4,2,2,1,1))
      
  # CBS ----
    # November 28, 1994 ----
    cbs.19941128 <- read_ascii("Data/Surveys/cbsnov94a.dat", # Good
                               total_cards = 3,
                               var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                               var_positions = c(49,54,55,67,68),
                               var_widths = c(6,1,1,1,1),
                               var_cards = c(3,1,1,2,2))
    cbs.19941128.repins <- cbs.19941128 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.19941128.demins <- cbs.19941128 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19941128.repouts <- cbs.19941128 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19941128.demouts <- cbs.19941128 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.19941128.ins <- rbind(cbs.19941128.repins, cbs.19941128.demins)
    wpct(cbs.19941128.ins$fav, as.numeric(cbs.19941128.ins$weight)) # 84.42
    length(cbs.19941128.ins$weight) # 971
    
    cbs.19941128.outs <- rbind(cbs.19941128.repouts, cbs.19941128.demouts)
    wpct(cbs.19941128.outs$fav, as.numeric(cbs.19941128.outs$weight)) # 24.27
    length(cbs.19941128.outs$weight) # 946
    
    rm(cbs.19941128, 
       cbs.19941128.repins, cbs.19941128.demins,
       cbs.19941128.repouts, cbs.19941128.demouts,
       cbs.19941128.ins, cbs.19941128.outs)
    
    
    # August 28, 1996 ----
    cbs.19960828 <- read_ascii("Data/Surveys/c96008d.dat", # Good
                               total_cards = 3,
                               var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                               var_cards = c(3,1,1,2,2),
                               var_positions = c(49,54,53,67,68),
                               var_widths = c(6,1,1,1,1))
    cbs.19960828.repins <- cbs.19960828 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.19960828.demins <- cbs.19960828 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19960828.repouts <- cbs.19960828 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19960828.demouts <- cbs.19960828 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.19960828.ins <- rbind(cbs.19960828.repins, cbs.19960828.demins)
    wpct(cbs.19960828.ins$fav, as.numeric(cbs.19960828.ins$weight)) # 89.71
    length(cbs.19960828.ins$weight) # 1231
    
    cbs.19960828.outs <- rbind(cbs.19960828.repouts, cbs.19960828.demouts)
    wpct(cbs.19960828.outs$fav, as.numeric(cbs.19960828.outs$weight)) # 23.25
    length(cbs.19960828.outs$weight) # 1204
    
    rm(cbs.19960828, 
       cbs.19960828.repins, cbs.19960828.demins,
       cbs.19960828.repouts, cbs.19960828.demouts,
       cbs.19960828.ins, cbs.19960828.outs)
    
    # December 13, 1998 ----
    gallup.19981213 <- read_ascii("Data/Surveys/a9812053.dat", # Good
                                  total_cards = 6,
                                  var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                  var_positions = c(13,47,49,18,19),
                                  var_widths = c(3,2,2,1,1),
                                  var_cards = c(1,6,6,5,5))
    gallup.19981213.repins <- gallup.19981213 %>%
      filter(pid == 1 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    gallup.19981213.demins <- gallup.19981213 %>%
      filter(pid == 2 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    gallup.19981213.repouts <- gallup.19981213 %>%
      filter(pid == 1 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    gallup.19981213.demouts <- gallup.19981213 %>%
      filter(pid == 2 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    gallup.19981213.ins <- rbind(gallup.19981213.repins, gallup.19981213.demins)
    wpct(gallup.19981213.ins$fav, as.numeric(gallup.19981213.ins$weight)) # 92.09
    length(gallup.19981213.ins$weight) # 928
    
    gallup.19981213.outs <- rbind(gallup.19981213.repouts, gallup.19981213.demouts)
    wpct(gallup.19981213.outs$fav, as.numeric(gallup.19981213.outs$weight)) # 25.22
    length(gallup.19981213.outs$weight) # 909
    
    rm(gallup.19981213, 
       gallup.19981213.repins, gallup.19981213.demins,
       gallup.19981213.repouts, gallup.19981213.demouts,
       gallup.19981213.ins, gallup.19981213.outs)
    # January 13, 1999 ----
    cbs.19990113 <- read_ascii("Data/Surveys/c99001b.dat", # Good
                               total_cards = 3,
                               var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                               var_cards = c(3,1,1,2,2),
                               var_positions = c(49,41,42,67,68),
                               var_widths = c(6,1,1,1,1))
    cbs.19990113.repins <- cbs.19990113 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.19990113.demins <- cbs.19990113 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990113.repouts <- cbs.19990113 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990113.demouts <- cbs.19990113 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.19990113.ins <- rbind(cbs.19990113.repins, cbs.19990113.demins)
    wpct(cbs.19990113.ins$fav, as.numeric(cbs.19990113.ins$weight)) # 84.42
    length(cbs.19990113.ins$weight) # 1895
    
    cbs.19990113.outs <- rbind(cbs.19990113.repouts, cbs.19990113.demouts)
    wpct(cbs.19990113.outs$fav, as.numeric(cbs.19990113.outs$weight)) # 21.34
    length(cbs.19990113.outs$weight) # 1868
    
    rm(cbs.19990113, 
       cbs.19990113.repins, cbs.19990113.demins,
       cbs.19990113.repouts, cbs.19990113.demouts,
       cbs.19990113.ins, cbs.19990113.outs)
  
    # January 4, 1999 ----
    cbs.19990104 <- read_ascii("Data/Surveys/c99001a.dat", # Good
                               total_cards = 3,
                               var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                               var_cards = c(3,1,1,2,2),
                               var_positions = c(49,42,43,67,68),
                               var_widths = c(6,1,1,1,1))
    cbs.19990104.repins <- cbs.19990104 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.19990104.demins <- cbs.19990104 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990104.repouts <- cbs.19990104 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990104.demouts <- cbs.19990104 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.19990104.ins <- rbind(cbs.19990104.repins, cbs.19990104.demins)
    wpct(cbs.19990104.ins$fav, as.numeric(cbs.19990104.ins$weight)) # 84.36
    length(cbs.19990104.ins$weight) # 1003
    
    cbs.19990104.outs <- rbind(cbs.19990104.repouts, cbs.19990104.demouts)
    wpct(cbs.19990104.outs$fav, as.numeric(cbs.19990104.outs$weight)) # 23.16
    length(cbs.19990104.outs$weight) # 996
    
    rm(cbs.19990104, 
       cbs.19990104.repins, cbs.19990104.demins,
       cbs.19990104.repouts, cbs.19990104.demouts,
       cbs.19990104.ins, cbs.19990104.outs)
    
    # February 7, 1999 ----
    cbs.19990207 <- read_ascii("Data/Surveys/c99002a.dat", # Good
                               total_cards = 3,
                               var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                               var_cards = c(3,1,1,2,2),
                               var_positions = c(49,37,40,67,68),
                               var_widths = c(6,1,1,1,1))
    cbs.19990207.repins <- cbs.19990207 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.19990207.demins <- cbs.19990207 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990207.repouts <- cbs.19990207 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990207.demouts <- cbs.19990207 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.19990207.ins <- rbind(cbs.19990207.repins, cbs.19990207.demins)
    wpct(cbs.19990207.ins$fav, as.numeric(cbs.19990207.ins$weight)) # 85.19
    length(cbs.19990207.ins$weight) # 520
    
    cbs.19990207.outs <- rbind(cbs.19990207.repouts, cbs.19990207.demouts)
    wpct(cbs.19990207.outs$fav, as.numeric(cbs.19990207.outs$weight)) # 23.29
    length(cbs.19990207.outs$weight) # 517
    
    rm(cbs.19990207, 
       cbs.19990207.repins, cbs.19990207.demins,
       cbs.19990207.repouts, cbs.19990207.demouts,
       cbs.19990207.ins, cbs.19990207.outs)
    
    # June 6, 1999 ----
    cbs.19990606 <- read_ascii("Data/Surveys/c99006a.dat", # Good
                               total_cards = 3,
                               var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                               var_cards = c(3,1,1,2,2),
                               var_positions = c(49,39,40,67,68),
                               var_widths = c(6,1,1,1,1))
    cbs.19990606.repins <- cbs.19990606 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.19990606.demins <- cbs.19990606 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990606.repouts <- cbs.19990606 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990606.demouts <- cbs.19990606 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.19990606.ins <- rbind(cbs.19990606.repins, cbs.19990606.demins)
    wpct(cbs.19990606.ins$fav, as.numeric(cbs.19990606.ins$weight)) # 80.81
    length(cbs.19990606.ins$weight) # 823
    
    cbs.19990606.outs <- rbind(cbs.19990606.repouts, cbs.19990606.demouts)
    wpct(cbs.19990606.outs$fav, as.numeric(cbs.19990606.outs$weight)) # 24.14
    length(cbs.19990606.outs$weight) # 810
    
    rm(cbs.19990606, 
       cbs.19990606.repins, cbs.19990606.demins,
       cbs.19990606.repouts, cbs.19990606.demouts,
       cbs.19990606.ins, cbs.19990606.outs)
    
    # September 18, 1999 ----
    cbs.19990918 <- read_ascii("Data/Surveys/c99009a.dat", # Good
                               total_cards = 3,
                               var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                               var_cards = c(3,1,1,2,2),
                               var_positions = c(49,60,59,67,68),
                               var_widths = c(6,1,1,1,1))
    cbs.19990918.repins <- cbs.19990918 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.19990918.demins <- cbs.19990918 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990918.repouts <- cbs.19990918 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.19990918.demouts <- cbs.19990918 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.19990918.ins <- rbind(cbs.19990918.repins, cbs.19990918.demins)
    wpct(cbs.19990918.ins$fav, as.numeric(cbs.19990918.ins$weight)) # 86.58
    length(cbs.19990918.ins$weight) # 1127
    
    cbs.19990918.outs <- rbind(cbs.19990918.repouts, cbs.19990918.demouts)
    wpct(cbs.19990918.outs$fav, as.numeric(cbs.19990918.outs$weight)) # 22.91
    length(cbs.19990918.outs$weight) # 1113
    
    rm(cbs.19990918, 
       cbs.19990918.repins, cbs.19990918.demins,
       cbs.19990918.repouts, cbs.19990918.demouts,
       cbs.19990918.ins, cbs.19990918.outs)
    
    # August 6, 2000 ----
    cbs.20000806 <- read_ascii("Data/Surveys/c200008b.dat", # Good
                               total_cards = 3,
                               var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                               var_cards = c(3,1,1,2,2),
                               var_positions = c(49,70,69,67,68),
                               var_widths = c(6,1,1,1,1))
    cbs.20000806.repins <- cbs.20000806 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.20000806.demins <- cbs.20000806 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.20000806.repouts <- cbs.20000806 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.20000806.demouts <- cbs.20000806 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.20000806.ins <- rbind(cbs.20000806.repins, cbs.20000806.demins)
    wpct(cbs.20000806.ins$fav, as.numeric(cbs.20000806.ins$weight)) # 88.77
    length(cbs.20000806.ins$weight) # 450
    
    cbs.20000806.outs <- rbind(cbs.20000806.repouts, cbs.20000806.demouts)
    wpct(cbs.20000806.outs$fav, as.numeric(cbs.20000806.outs$weight)) # 24.34
    length(cbs.20000806.outs$weight) # 439
    
    rm(cbs.20000806, 
       cbs.20000806.repins, cbs.20000806.demins,
       cbs.20000806.repouts, cbs.20000806.demouts,
       cbs.20000806.ins, cbs.20000806.outs)
    
    # August 30, 2001 ----
    cbs.20010830 <- read_ascii("Data/Surveys/c200108c.dat", # Good
                               total_cards = 3,
                               var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                               var_cards = c(3,1,1,2,2),
                               var_positions = c(49,47,48,67,68),
                               var_widths = c(6,1,1,1,1))
    cbs.20010830.repins <- cbs.20010830 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.20010830.demins <- cbs.20010830 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.20010830.repouts <- cbs.20010830 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.20010830.demouts <- cbs.20010830 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.20010830.ins <- rbind(cbs.20010830.repins, cbs.20010830.demins)
    wpct(cbs.20010830.ins$fav, as.numeric(cbs.20010830.ins$weight)) # 85.60
    length(cbs.20010830.ins$weight) # 730
    
    cbs.20010830.outs <- rbind(cbs.20010830.repouts, cbs.20010830.demouts)
    wpct(cbs.20010830.outs$fav, as.numeric(cbs.20010830.outs$weight)) # 21.67
    length(cbs.20010830.outs$weight) # 730
    
    rm(cbs.20010830, 
       cbs.20010830.repins, cbs.20010830.demins,
       cbs.20010830.repouts, cbs.20010830.demouts,
       cbs.20010830.ins, cbs.20010830.outs)
  
  # CBS/New York Times ----
    # October 4, 1984 ----
    # cbs.nyt.19841004 <- read_ascii("Data/Surveys/oct1.dat", # Weight not working
    #                                total_cards = 3,
    #                                var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
    #                                var_cards = c(3,1,1,2,2),
    #                                var_positions = c(49,48,47,67,68),
    #                                var_widths = c(6,1,1,1,1))
  
    # November 2, 1984 ----
    cbs.nyt.19841102 <- read_ascii("Data/Surveys/c1984oct4.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_cards = c(3,1,1,2,2),
                                   var_positions = c(49,59,60,67,69),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.19841102.repins <- cbs.nyt.19841102 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19841102.demins <- cbs.nyt.19841102 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19841102.repouts <- cbs.nyt.19841102 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19841102.demouts <- cbs.nyt.19841102 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19841102.ins <- rbind(cbs.nyt.19841102.repins, cbs.nyt.19841102.demins)
    wpct(cbs.nyt.19841102.ins$fav, as.numeric(cbs.nyt.19841102.ins$weight)) # 90.23
    length(cbs.nyt.19841102.ins$weight) # 1377
    
    cbs.nyt.19841102.outs <- rbind(cbs.nyt.19841102.repouts, cbs.nyt.19841102.demouts)
    wpct(cbs.nyt.19841102.outs$fav, as.numeric(cbs.nyt.19841102.outs$weight)) # 25.06
    length(cbs.nyt.19841102.outs$weight) # 1359
    
    rm(cbs.nyt.19841102, 
       cbs.nyt.19841102.repins, cbs.nyt.19841102.demins,
       cbs.nyt.19841102.repouts, cbs.nyt.19841102.demouts,
       cbs.nyt.19841102.ins, cbs.nyt.19841102.outs)
    
    # November 3, 1984 ----
    # cbs.nyt.19841103 <- read_ascii("Data/Surveys/novpre1.dat", # Weight not working
    #                                total_cards = 3,
    #                                var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
    #                                var_cards = c(3,1,1,2,2),
    #                                var_positions = c(49,59,60,67,68),
    #                                var_widths = c(6,1,1,1,1))
    
    # January 15, 1990 ----
    cbs.nyt.19900115 <- read_ascii("Data/Surveys/jan.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,12,13,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,2,2,2,2))
    cbs.nyt.19900115.repins <- cbs.nyt.19900115 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19900115.demins <- cbs.nyt.19900115 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19900115.repouts <- cbs.nyt.19900115 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19900115.demouts <- cbs.nyt.19900115 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19900115.ins <- rbind(cbs.nyt.19900115.repins, cbs.nyt.19900115.demins)
    wpct(cbs.nyt.19900115.ins$fav, as.numeric(cbs.nyt.19900115.ins$weight)) # 88.36
    length(cbs.nyt.19900115.ins$weight) # 1326
    
    cbs.nyt.19900115.outs <- rbind(cbs.nyt.19900115.repouts, cbs.nyt.19900115.demouts)
    wpct(cbs.nyt.19900115.outs$fav, as.numeric(cbs.nyt.19900115.outs$weight)) # 43.94
    length(cbs.nyt.19900115.outs$weight) # 1282
    
    rm(cbs.nyt.19900115, 
       cbs.nyt.19900115.repins, cbs.nyt.19900115.demins,
       cbs.nyt.19900115.repouts, cbs.nyt.19900115.demouts,
       cbs.nyt.19900115.ins, cbs.nyt.19900115.outs)
    
    # April 2, 1990 ----
    # cbs.nyt.19900402 <- read_ascii("Data/Surveys/apr.dat", # Seems miscoded
    #                                total_cards = 3,
    #                                var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
    #                                var_positions = c(49,43,44,67,70),
    #                                var_widths = c(6,1,1,1,1),
    #                                var_cards = c(3,1,1,2,2))
    # cbs.nyt.19900402.repins <- cbs.nyt.19900402 %>%
    #   filter(pid == 1 | pid.lean == 1) %>%
    #   filter(rep.fav == 1 | rep.fav == 2) %>%
    #   rename(fav = rep.fav) %>%
    #   select(weight, fav)
    # cbs.nyt.19900402.demins <- cbs.nyt.19900402 %>%
    #   filter(pid == 2 | pid.lean == 2) %>%
    #   filter(dem.fav == 1 | dem.fav == 2) %>%
    #   rename(fav = dem.fav) %>%
    #   select(weight, fav)
    # cbs.nyt.19900402.repouts <- cbs.nyt.19900402 %>%
    #   filter(pid == 1 | pid.lean == 1) %>%
    #   filter(dem.fav == 1 | dem.fav == 2) %>%
    #   rename(fav = dem.fav) %>%
    #   select(weight, fav)
    # cbs.nyt.19900402.demouts <- cbs.nyt.19900402 %>%
    #   filter(pid == 2 | pid.lean == 2) %>%
    #   filter(rep.fav == 1 | rep.fav == 2) %>%
    #   rename(fav = rep.fav) %>%
    #   select(weight, fav)
    # 
    # cbs.nyt.19900402.ins <- rbind(cbs.nyt.19900402.repins, cbs.nyt.19900402.demins)
    # wpct(cbs.nyt.19900402.ins$fav, as.numeric(cbs.nyt.19900402.ins$weight)) # 68.56
    # length(cbs.nyt.19900402.ins$weight) # 1437
    # 
    # cbs.nyt.19900402.outs <- rbind(cbs.nyt.19900402.repouts, cbs.nyt.19900402.demouts)
    # wpct(cbs.nyt.19900402.outs$fav, as.numeric(cbs.nyt.19900402.outs$weight)) # 45.78
    # length(cbs.nyt.19900402.outs$weight) # 1432
    # 
    # rm(cbs.nyt.19900402, 
    #    cbs.nyt.19900402.repins, cbs.nyt.19900402.demins,
    #    cbs.nyt.19900402.repouts, cbs.nyt.19900402.demouts,
    #    cbs.nyt.19900402.ins, cbs.nyt.19900402.outs)
    
    # May 30, 1992 ----
    cbs.nyt.19920530 <- read_ascii("Data/Surveys/may92b.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,60,61,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,1,1,2,2))
    cbs.nyt.19920530.repins <- cbs.nyt.19920530 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19920530.demins <- cbs.nyt.19920530 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19920530.repouts <- cbs.nyt.19920530 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19920530.demouts <- cbs.nyt.19920530 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19920530.ins <- rbind(cbs.nyt.19920530.repins, cbs.nyt.19920530.demins)
    wpct(cbs.nyt.19920530.ins$fav, as.numeric(cbs.nyt.19920530.ins$weight)) # 77.79
    length(cbs.nyt.19920530.ins$weight) # 1105
    
    cbs.nyt.19920530.outs <- rbind(cbs.nyt.19920530.repouts, cbs.nyt.19920530.demouts)
    wpct(cbs.nyt.19920530.outs$fav, as.numeric(cbs.nyt.19920530.outs$weight)) # 23.57
    length(cbs.nyt.19920530.outs$weight) # 1092
    
    rm(cbs.nyt.19920530, 
       cbs.nyt.19920530.repins, cbs.nyt.19920530.demins,
       cbs.nyt.19920530.repouts, cbs.nyt.19920530.demouts,
       cbs.nyt.19920530.ins, cbs.nyt.19920530.outs)
    
    # June 20, 1992 ----
    cbs.nyt.19920620 <- read_ascii("Data/Surveys/jun92a.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,23,24,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,2,2,2,2))
    cbs.nyt.19920620.repins <- cbs.nyt.19920620 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19920620.demins <- cbs.nyt.19920620 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19920620.repouts <- cbs.nyt.19920620 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19920620.demouts <- cbs.nyt.19920620 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19920620.ins <- rbind(cbs.nyt.19920620.repins, cbs.nyt.19920620.demins)
    wpct(cbs.nyt.19920620.ins$fav, as.numeric(cbs.nyt.19920620.ins$weight)) # 82.46
    length(cbs.nyt.19920620.ins$weight) # 1119
    
    cbs.nyt.19920620.outs <- rbind(cbs.nyt.19920620.repouts, cbs.nyt.19920620.demouts)
    wpct(cbs.nyt.19920620.outs$fav, as.numeric(cbs.nyt.19920620.outs$weight)) # 28.03
    length(cbs.nyt.19920620.outs$weight) # 1099
    
    rm(cbs.nyt.19920620, 
       cbs.nyt.19920620.repins, cbs.nyt.19920620.demins,
       cbs.nyt.19920620.repouts, cbs.nyt.19920620.demouts,
       cbs.nyt.19920620.ins, cbs.nyt.19920620.outs)
  
    # January 17, 1994 ----
    cbs.nyt.19940117 <- read_ascii("Data/Surveys/c1994jan94c.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,23,24,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,2,2,2,2))
    cbs.nyt.19940117.repins <- cbs.nyt.19940117 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19940117.demins <- cbs.nyt.19940117 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19940117.repouts <- cbs.nyt.19940117 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19940117.demouts <- cbs.nyt.19940117 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19940117.ins <- rbind(cbs.nyt.19940117.repins, cbs.nyt.19940117.demins)
    wpct(cbs.nyt.19940117.ins$fav, as.numeric(cbs.nyt.19940117.ins$weight)) # 83.19
    length(cbs.nyt.19940117.ins$weight) # 927
    
    cbs.nyt.19940117.outs <- rbind(cbs.nyt.19940117.repouts, cbs.nyt.19940117.demouts)
    wpct(cbs.nyt.19940117.outs$fav, as.numeric(cbs.nyt.19940117.outs$weight)) # 31.39
    length(cbs.nyt.19940117.outs$weight) # 916
    
    rm(cbs.nyt.19940117, 
       cbs.nyt.19940117.repins, cbs.nyt.19940117.demins,
       cbs.nyt.19940117.repouts, cbs.nyt.19940117.demouts,
       cbs.nyt.19940117.ins, cbs.nyt.19940117.outs)
    
    # September 11, 1994 ----
    cbs.nyt.19940911 <- read_ascii("Data/Surveys/sep94a.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,62,63,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,1,1,2,2))
    cbs.nyt.19940911.repins <- cbs.nyt.19940911 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19940911.demins <- cbs.nyt.19940911 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19940911.repouts <- cbs.nyt.19940911 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19940911.demouts <- cbs.nyt.19940911 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19940911.ins <- rbind(cbs.nyt.19940911.repins, cbs.nyt.19940911.demins)
    wpct(cbs.nyt.19940911.ins$fav, as.numeric(cbs.nyt.19940911.ins$weight)) # 83.69
    length(cbs.nyt.19940911.ins$weight) # 1002
    
    cbs.nyt.19940911.outs <- rbind(cbs.nyt.19940911.repouts, cbs.nyt.19940911.demouts)
    wpct(cbs.nyt.19940911.outs$fav, as.numeric(cbs.nyt.19940911.outs$weight)) # 26.65
    length(cbs.nyt.19940911.outs$weight) # 974
    
    rm(cbs.nyt.19940911, 
       cbs.nyt.19940911.repins, cbs.nyt.19940911.demins,
       cbs.nyt.19940911.repouts, cbs.nyt.19940911.demouts,
       cbs.nyt.19940911.ins, cbs.nyt.19940911.outs)
    
    # November 1, 1994 ----
    cbs.nyt.19941101 <- read_ascii("Data/Surveys/oct94b.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,8,9,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,2,2,2,2))
    cbs.nyt.19941101.repins <- cbs.nyt.19941101 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19941101.demins <- cbs.nyt.19941101 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19941101.repouts <- cbs.nyt.19941101 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19941101.demouts <- cbs.nyt.19941101 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19941101.ins <- rbind(cbs.nyt.19941101.repins, cbs.nyt.19941101.demins)
    wpct(cbs.nyt.19941101.ins$fav, as.numeric(cbs.nyt.19941101.ins$weight)) # 85.19
    length(cbs.nyt.19941101.ins$weight) # 1240
    
    cbs.nyt.19941101.outs <- rbind(cbs.nyt.19941101.repouts, cbs.nyt.19941101.demouts)
    wpct(cbs.nyt.19941101.outs$fav, as.numeric(cbs.nyt.19941101.outs$weight)) # 21.57
    length(cbs.nyt.19941101.outs$weight) # 1224
    
    rm(cbs.nyt.19941101, 
       cbs.nyt.19941101.repins, cbs.nyt.19941101.demins,
       cbs.nyt.19941101.repouts, cbs.nyt.19941101.demouts,
       cbs.nyt.19941101.ins, cbs.nyt.19941101.outs)
  
    # April 4, 1995 ----
    cbs.nyt.19950404 <- read_ascii("Data/Surveys/capr95a.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_positions = c(49,60,61,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,1,1,2,2))
    cbs.nyt.19950404.repins <- cbs.nyt.19950404 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19950404.demins <- cbs.nyt.19950404 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19950404.repouts <- cbs.nyt.19950404 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19950404.demouts <- cbs.nyt.19950404 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19950404.ins <- rbind(cbs.nyt.19950404.repins, cbs.nyt.19950404.demins)
    wpct(cbs.nyt.19950404.ins$fav, as.numeric(cbs.nyt.19950404.ins$weight)) # 89.52
    length(cbs.nyt.19950404.ins$weight) # 939
    
    cbs.nyt.19950404.outs <- rbind(cbs.nyt.19950404.repouts, cbs.nyt.19950404.demouts)
    wpct(cbs.nyt.19950404.outs$fav, as.numeric(cbs.nyt.19950404.outs$weight)) # 18.92
    length(cbs.nyt.19950404.outs$weight) # 912
    
    rm(cbs.nyt.19950404, 
       cbs.nyt.19950404.repins, cbs.nyt.19950404.demins,
       cbs.nyt.19950404.repouts, cbs.nyt.19950404.demouts,
       cbs.nyt.19950404.ins, cbs.nyt.19950404.outs)
  
    # August 9, 1995 ----
    cbs.nyt.19950809 <- read_ascii("Data/Surveys/c95008a.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_positions = c(49,16,17,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,2,2,2,2))
    cbs.nyt.19950809.repins <- cbs.nyt.19950809 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19950809.demins <- cbs.nyt.19950809 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19950809.repouts <- cbs.nyt.19950809 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19950809.demouts <- cbs.nyt.19950809 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19950809.ins <- rbind(cbs.nyt.19950809.repins, cbs.nyt.19950809.demins)
    wpct(cbs.nyt.19950809.ins$fav, as.numeric(cbs.nyt.19950809.ins$weight)) # 84.63
    length(cbs.nyt.19950809.ins$weight) # 1266
    
    cbs.nyt.19950809.outs <- rbind(cbs.nyt.19950809.repouts, cbs.nyt.19950809.demouts)
    wpct(cbs.nyt.19950809.outs$fav, as.numeric(cbs.nyt.19950809.outs$weight)) # 25.68
    length(cbs.nyt.19950809.outs$weight) # 1260
    
    rm(cbs.nyt.19950809, 
       cbs.nyt.19950809.repins, cbs.nyt.19950809.demins,
       cbs.nyt.19950809.repouts, cbs.nyt.19950809.demouts,
       cbs.nyt.19950809.ins, cbs.nyt.19950809.outs)
  
    # April 2, 1996 ----
    cbs.nyt.19960402 <- read_ascii("Data/Surveys/c96003d.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_positions = c(49,37,36,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,1,1,2,2))
    cbs.nyt.19960402.repins <- cbs.nyt.19960402 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19960402.demins <- cbs.nyt.19960402 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19960402.repouts <- cbs.nyt.19960402 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19960402.demouts <- cbs.nyt.19960402 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19960402.ins <- rbind(cbs.nyt.19960402.repins, cbs.nyt.19960402.demins)
    wpct(cbs.nyt.19960402.ins$fav, as.numeric(cbs.nyt.19960402.ins$weight)) # 79.84
    length(cbs.nyt.19960402.ins$weight) # 1096
    
    cbs.nyt.19960402.outs <- rbind(cbs.nyt.19960402.repouts, cbs.nyt.19960402.demouts)
    wpct(cbs.nyt.19960402.outs$fav, as.numeric(cbs.nyt.19960402.outs$weight)) # 17.14
    length(cbs.nyt.19960402.outs$weight) # 1092
    
    rm(cbs.nyt.19960402, 
       cbs.nyt.19960402.repins, cbs.nyt.19960402.demins,
       cbs.nyt.19960402.repouts, cbs.nyt.19960402.demouts,
       cbs.nyt.19960402.ins, cbs.nyt.19960402.outs)
  
    # August 5, 1996 ----
    # cbs.nyt.19960805 <- read_ascii("Data/Surveys/c96008a.dat", # Number of lines in file not multiple of cards in the file
    #                                total_cards = 3,
    #                                var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
    #                                var_positions = c(49,64,63,67,68),
    #                                var_widths = c(6,1,1,1,1),
    #                                var_cards = c(3,1,1,2,2))
  
    # August 14, 1996 ----
    # cbs.nyt.19960814 <- read_ascii("Data/Surveys/c96008b.dat", # Number of lines in file not multiple of cards in the file
    #                                total_cards = 3,
    #                                var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
    #                                var_positions = c(49,44,43,67,68),
    #                                var_widths = c(6,1,1,1,1),
    #                                var_cards = c(3,1,1,2,2))
  
    # August 18, 1996 ----
    # cbs.nyt.19960818 <- read_ascii("Data/Surveys/c96008c.dat", # Number of lines in file not multiple of cards in the file
    #                                total_cards = 3,
    #                                var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
    #                                var_positions = c(49,61,60,67,68),
    #                                var_widths = c(6,1,1,1,1),
    #                                var_cards = c(3,1,1,2,2))

    # January 17, 1997 ----
    cbs.nyt.19970117 <- read_ascii("Data/Surveys/97001c.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,51,52,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,1,1,2,2))
    cbs.nyt.19970117.repins <- cbs.nyt.19970117 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19970117.demins <- cbs.nyt.19970117 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19970117.repouts <- cbs.nyt.19970117 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19970117.demouts <- cbs.nyt.19970117 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19970117.ins <- rbind(cbs.nyt.19970117.repins, cbs.nyt.19970117.demins)
    wpct(cbs.nyt.19970117.ins$fav, as.numeric(cbs.nyt.19970117.ins$weight)) # 85.24
    length(cbs.nyt.19970117.ins$weight) # 1120
    
    cbs.nyt.19970117.outs <- rbind(cbs.nyt.19970117.repouts, cbs.nyt.19970117.demouts)
    wpct(cbs.nyt.19970117.outs$fav, as.numeric(cbs.nyt.19970117.outs$weight)) # 28.71
    length(cbs.nyt.19970117.outs$weight) # 1096
    
    rm(cbs.nyt.19970117, 
       cbs.nyt.19970117.repins, cbs.nyt.19970117.demins,
       cbs.nyt.19970117.repouts, cbs.nyt.19970117.demouts,
       cbs.nyt.19970117.ins, cbs.nyt.19970117.outs)
  
    # August 20, 1998 ----
    # cbs.nyt.19980820 <- read_ascii("Data/Surveys/c98009a.dat", # Wrong Codebook
    #                                total_cards = 3,
    #                                var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
    #                                var_positions = c(49,63,64,147,148),
    #                                var_widths = c(6,1,1,1,1),
    #                                var_cards = c(3,1,1,1,1))
  
    # October 28, 1998 ----
    cbs.nyt.19981028 <- read_ascii("Data/Surveys/c98010f.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,63,64,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,1,1,2,2))
    cbs.nyt.19981028.repins <- cbs.nyt.19981028 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19981028.demins <- cbs.nyt.19981028 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19981028.repouts <- cbs.nyt.19981028 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19981028.demouts <- cbs.nyt.19981028 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19981028.ins <- rbind(cbs.nyt.19981028.repins, cbs.nyt.19981028.demins)
    wpct(cbs.nyt.19981028.ins$fav, as.numeric(cbs.nyt.19981028.ins$weight)) # 86.66
    length(cbs.nyt.19981028.ins$weight) # 970
    
    cbs.nyt.19981028.outs <- rbind(cbs.nyt.19981028.repouts, cbs.nyt.19981028.demouts)
    wpct(cbs.nyt.19981028.outs$fav, as.numeric(cbs.nyt.19981028.outs$weight)) # 23.66
    length(cbs.nyt.19981028.outs$weight) # 955
    
    rm(cbs.nyt.19981028, 
       cbs.nyt.19981028.repins, cbs.nyt.19981028.demins,
       cbs.nyt.19981028.repouts, cbs.nyt.19981028.demouts,
       cbs.nyt.19981028.ins, cbs.nyt.19981028.outs)
  
    # December 15, 1998 ----
    cbs.nyt.19981215 <- read_ascii("Data/Surveys/c98012b.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,37,38,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,1,1,2,2))
    cbs.nyt.19981215.repins <- cbs.nyt.19981215 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19981215.demins <- cbs.nyt.19981215 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19981215.repouts <- cbs.nyt.19981215 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19981215.demouts <- cbs.nyt.19981215 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19981215.ins <- rbind(cbs.nyt.19981215.repins, cbs.nyt.19981215.demins)
    wpct(cbs.nyt.19981215.ins$fav, as.numeric(cbs.nyt.19981215.ins$weight)) # 84.01
    length(cbs.nyt.19981215.ins$weight) # 1410
    
    cbs.nyt.19981215.outs <- rbind(cbs.nyt.19981215.repouts, cbs.nyt.19981215.demouts)
    wpct(cbs.nyt.19981215.outs$fav, as.numeric(cbs.nyt.19981215.outs$weight)) # 22.48
    length(cbs.nyt.19981215.outs$weight) # 1388
    
    rm(cbs.nyt.19981215, 
       cbs.nyt.19981215.repins, cbs.nyt.19981215.demins,
       cbs.nyt.19981215.repouts, cbs.nyt.19981215.demouts,
       cbs.nyt.19981215.ins, cbs.nyt.19981215.outs)
  
    # December 17, 1998 ----
    cbs.nyt.19981217 <- read_ascii("Data/Surveys/c98012d.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,37,38,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,1,1,2,2))
    cbs.nyt.19981217.repins <- cbs.nyt.19981217 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19981217.demins <- cbs.nyt.19981217 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19981217.repouts <- cbs.nyt.19981217 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19981217.demouts <- cbs.nyt.19981217 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19981217.ins <- rbind(cbs.nyt.19981217.repins, cbs.nyt.19981217.demins)
    wpct(cbs.nyt.19981217.ins$fav, as.numeric(cbs.nyt.19981217.ins$weight)) # 83.85
    length(cbs.nyt.19981217.ins$weight) # 1677
    
    cbs.nyt.19981217.outs <- rbind(cbs.nyt.19981217.repouts, cbs.nyt.19981217.demouts)
    wpct(cbs.nyt.19981217.outs$fav, as.numeric(cbs.nyt.19981217.outs$weight)) # 21.48
    length(cbs.nyt.19981217.outs$weight) # 1650
    
    rm(cbs.nyt.19981217, 
       cbs.nyt.19981217.repins, cbs.nyt.19981217.demins,
       cbs.nyt.19981217.repouts, cbs.nyt.19981217.demouts,
       cbs.nyt.19981217.ins, cbs.nyt.19981217.outs)
  
    # February 1, 1999 ----
    cbs.nyt.19990201 <- read_ascii("Data/Surveys/c99001d.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(49,39,40,67,68),
                                   var_widths = c(6,1,1,1,1),
                                   var_cards = c(3,1,1,2,2))
    cbs.nyt.19990201.repins <- cbs.nyt.19990201 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.19990201.demins <- cbs.nyt.19990201 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19990201.repouts <- cbs.nyt.19990201 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.19990201.demouts <- cbs.nyt.19990201 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.19990201.ins <- rbind(cbs.nyt.19990201.repins, cbs.nyt.19990201.demins)
    wpct(cbs.nyt.19990201.ins$fav, as.numeric(cbs.nyt.19990201.ins$weight)) # 83.70
    length(cbs.nyt.19990201.ins$weight) # 909
    
    cbs.nyt.19990201.outs <- rbind(cbs.nyt.19990201.repouts, cbs.nyt.19990201.demouts)
    wpct(cbs.nyt.19990201.outs$fav, as.numeric(cbs.nyt.19990201.outs$weight)) # 18.90
    length(cbs.nyt.19990201.outs$weight) # 903
    
    rm(cbs.nyt.19990201, 
       cbs.nyt.19990201.repins, cbs.nyt.19990201.demins,
       cbs.nyt.19990201.repouts, cbs.nyt.19990201.demouts,
       cbs.nyt.19990201.ins, cbs.nyt.19990201.outs)
  
    # October 1, 2000 ----
    cbs.nyt.20001001 <- read_ascii("Data/Surveys/c200009d.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_cards = c(3,2,2,2,2),
                                   var_positions = c(49,28,29,67,68),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.20001001.repins <- cbs.nyt.20001001 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.20001001.demins <- cbs.nyt.20001001 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20001001.repouts <- cbs.nyt.20001001 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20001001.demouts <- cbs.nyt.20001001 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.20001001.ins <- rbind(cbs.nyt.20001001.repins, cbs.nyt.20001001.demins)
    wpct(cbs.nyt.20001001.ins$fav, as.numeric(cbs.nyt.20001001.ins$weight)) # 89.44
    length(cbs.nyt.20001001.ins$weight) # 1011
    
    cbs.nyt.20001001.outs <- rbind(cbs.nyt.20001001.repouts, cbs.nyt.20001001.demouts)
    wpct(cbs.nyt.20001001.outs$fav, as.numeric(cbs.nyt.20001001.outs$weight)) # 24.80
    length(cbs.nyt.20001001.outs$weight) # 988
    
    rm(cbs.nyt.20001001, 
       cbs.nyt.20001001.repins, cbs.nyt.20001001.demins,
       cbs.nyt.20001001.repouts, cbs.nyt.20001001.demouts,
       cbs.nyt.20001001.ins, cbs.nyt.20001001.outs)
  
    # February 14, 2000 ----
    cbs.nyt.20000214 <- read_ascii("Data/Surveys/c200002b.dat", # Good
                                   total_cards = 1,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(209,79,78,147,148),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.20000214.repins <- cbs.nyt.20000214 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.20000214.demins <- cbs.nyt.20000214 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20000214.repouts <- cbs.nyt.20000214 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20000214.demouts <- cbs.nyt.20000214 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.20000214.ins <- rbind(cbs.nyt.20000214.repins, cbs.nyt.20000214.demins)
    wpct(cbs.nyt.20000214.ins$fav, as.numeric(cbs.nyt.20000214.ins$weight)) # 88.18
    length(cbs.nyt.20000214.ins$weight) # 1047
    
    cbs.nyt.20000214.outs <- rbind(cbs.nyt.20000214.repouts, cbs.nyt.20000214.demouts)
    wpct(cbs.nyt.20000214.outs$fav, as.numeric(cbs.nyt.20000214.outs$weight)) # 32.05
    length(cbs.nyt.20000214.outs$weight) # 1025
    
    rm(cbs.nyt.20000214, 
       cbs.nyt.20000214.repins, cbs.nyt.20000214.demins,
       cbs.nyt.20000214.repouts, cbs.nyt.20000214.demouts,
       cbs.nyt.20000214.ins, cbs.nyt.20000214.outs)
  
    # May 13, 2000 ----
    cbs.nyt.20000513 <- read_ascii("Data/Surveys/c200005b.dat", # Good
                                   total_cards = 1,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_positions = c(209,96,95,147,148),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.20000513.repins <- cbs.nyt.20000513 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.20000513.demins <- cbs.nyt.20000513 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20000513.repouts <- cbs.nyt.20000513 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20000513.demouts <- cbs.nyt.20000513 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.20000513.ins <- rbind(cbs.nyt.20000513.repins, cbs.nyt.20000513.demins)
    wpct(cbs.nyt.20000513.ins$fav, as.numeric(cbs.nyt.20000513.ins$weight)) # 87.50
    length(cbs.nyt.20000513.ins$weight) # 625
    
    cbs.nyt.20000513.outs <- rbind(cbs.nyt.20000513.repouts, cbs.nyt.20000513.demouts)
    wpct(cbs.nyt.20000513.outs$fav, as.numeric(cbs.nyt.20000513.outs$weight)) # 26.09
    length(cbs.nyt.20000513.outs$weight) # 620
    
    rm(cbs.nyt.20000513, 
       cbs.nyt.20000513.repins, cbs.nyt.20000513.demins,
       cbs.nyt.20000513.repouts, cbs.nyt.20000513.demouts,
       cbs.nyt.20000513.ins, cbs.nyt.20000513.outs)
  
    # July 23, 2000 ----
    cbs.nyt.20000723 <- read_ascii("Data/Surveys/c200007c.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid","pid.lean"),
                                   var_cards = c(3,2,2,2,2),
                                   var_positions = c(49,13,12,67,68),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.20000723.repins <- cbs.nyt.20000723 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.20000723.demins <- cbs.nyt.20000723 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20000723.repouts <- cbs.nyt.20000723 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20000723.demouts <- cbs.nyt.20000723 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.20000723.ins <- rbind(cbs.nyt.20000723.repins, cbs.nyt.20000723.demins)
    wpct(cbs.nyt.20000723.ins$fav, as.numeric(cbs.nyt.20000723.ins$weight)) # 89.63
    length(cbs.nyt.20000723.ins$weight) # 377
    
    cbs.nyt.20000723.outs <- rbind(cbs.nyt.20000723.repouts, cbs.nyt.20000723.demouts)
    wpct(cbs.nyt.20000723.outs$fav, as.numeric(cbs.nyt.20000723.outs$weight)) # 23.28
    length(cbs.nyt.20000723.outs$weight) # 371
    
    rm(cbs.nyt.20000723, 
       cbs.nyt.20000723.repins, cbs.nyt.20000723.demins,
       cbs.nyt.20000723.repouts, cbs.nyt.20000723.demouts,
       cbs.nyt.20000723.ins, cbs.nyt.20000723.outs)
  
    # October 12, 2001 ----
    cbs.nyt.20010312 <- read_ascii("Data/Surveys/c200103a.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_cards = c(3,2,2,2,2),
                                   var_positions = c(49,18,19,67,68),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.20010312.repins <- cbs.nyt.20010312 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.20010312.demins <- cbs.nyt.20010312 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20010312.repouts <- cbs.nyt.20010312 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20010312.demouts <- cbs.nyt.20010312 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.20010312.ins <- rbind(cbs.nyt.20010312.repins, cbs.nyt.20010312.demins)
    wpct(cbs.nyt.20010312.ins$fav, as.numeric(cbs.nyt.20010312.ins$weight)) # 89.70
    length(cbs.nyt.20010312.ins$weight) # 991
    
    cbs.nyt.20010312.outs <- rbind(cbs.nyt.20010312.repouts, cbs.nyt.20010312.demouts)
    wpct(cbs.nyt.20010312.outs$fav, as.numeric(cbs.nyt.20010312.outs$weight)) # 25.14
    length(cbs.nyt.20010312.outs$weight) # 966
    
    rm(cbs.nyt.20010312, 
       cbs.nyt.20010312.repins, cbs.nyt.20010312.demins,
       cbs.nyt.20010312.repouts, cbs.nyt.20010312.demouts,
       cbs.nyt.20010312.ins, cbs.nyt.20010312.outs)
  
    # June 18, 2001 ----
    cbs.nyt.20010618 <- read_ascii("Data/Surveys/c200106b.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_cards = c(3,1,1,2,2),
                                   var_positions = c(49,65,66,67,68),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.20010618.repins <- cbs.nyt.20010618 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.20010618.demins <- cbs.nyt.20010618 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20010618.repouts <- cbs.nyt.20010618 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20010618.demouts <- cbs.nyt.20010618 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.20010618.ins <- rbind(cbs.nyt.20010618.repins, cbs.nyt.20010618.demins)
    wpct(cbs.nyt.20010618.ins$fav, as.numeric(cbs.nyt.20010618.ins$weight)) # 84.65
    length(cbs.nyt.20010618.ins$weight) # 886
    
    cbs.nyt.20010618.outs <- rbind(cbs.nyt.20010618.repouts, cbs.nyt.20010618.demouts)
    wpct(cbs.nyt.20010618.outs$fav, as.numeric(cbs.nyt.20010618.outs$weight)) # 25.15
    length(cbs.nyt.20010618.outs$weight) # 875
    
    rm(cbs.nyt.20010618, 
       cbs.nyt.20010618.repins, cbs.nyt.20010618.demins,
       cbs.nyt.20010618.repouts, cbs.nyt.20010618.demouts,
       cbs.nyt.20010618.ins, cbs.nyt.20010618.outs)
  
    # July 16, 2002 ----
    cbs.nyt.20020716 <- read_ascii("Data/Surveys/jul02b.dat", # Good
                                   total_cards = 3, 
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_cards = c(3,2,2,2,2),
                                   var_positions = c(49,16,17,67,68),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.20020716.repins <- cbs.nyt.20020716 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.20020716.demins <- cbs.nyt.20020716 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20020716.repouts <- cbs.nyt.20020716 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20020716.demouts <- cbs.nyt.20020716 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.20020716.ins <- rbind(cbs.nyt.20020716.repins, cbs.nyt.20020716.demins)
    wpct(cbs.nyt.20020716.ins$fav, as.numeric(cbs.nyt.20020716.ins$weight)) # 86.50
    length(cbs.nyt.20020716.ins$weight) # 848
    
    cbs.nyt.20020716.outs <- rbind(cbs.nyt.20020716.repouts, cbs.nyt.20020716.demouts)
    wpct(cbs.nyt.20020716.outs$fav, as.numeric(cbs.nyt.20020716.outs$weight)) # 30.24
    length(cbs.nyt.20020716.outs$weight) # 825
    
    rm(cbs.nyt.20020716, 
       cbs.nyt.20020716.repins, cbs.nyt.20020716.demins,
       cbs.nyt.20020716.repouts, cbs.nyt.20020716.demouts,
       cbs.nyt.20020716.ins, cbs.nyt.20020716.outs)
  
    # October 5, 2002 ----
    cbs.nyt.20021005 <- read_ascii("Data/Surveys/oct02a.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_cards = c(3,2,2,2,2),
                                   var_positions = c(49,30,31,67,68),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.20021005.repins <- cbs.nyt.20021005 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.20021005.demins <- cbs.nyt.20021005 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20021005.repouts <- cbs.nyt.20021005 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20021005.demouts <- cbs.nyt.20021005 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.20021005.ins <- rbind(cbs.nyt.20021005.repins, cbs.nyt.20021005.demins)
    wpct(cbs.nyt.20021005.ins$fav, as.numeric(cbs.nyt.20021005.ins$weight)) # 85.65
    length(cbs.nyt.20021005.ins$weight) # 583
    
    cbs.nyt.20021005.outs <- rbind(cbs.nyt.20021005.repouts, cbs.nyt.20021005.demouts)
    wpct(cbs.nyt.20021005.outs$fav, as.numeric(cbs.nyt.20021005.outs$weight)) # 29.12
    length(cbs.nyt.20021005.outs$weight) # 573
    
    rm(cbs.nyt.20021005, 
       cbs.nyt.20021005.repins, cbs.nyt.20021005.demins,
       cbs.nyt.20021005.repouts, cbs.nyt.20021005.demouts,
       cbs.nyt.20021005.ins, cbs.nyt.20021005.outs)
  
    # October 31, 2002 ----
    cbs.nyt.20021031 <- read_ascii("Data/Surveys/oct02f.dat", # Good
                                   total_cards = 3,
                                   var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                                   var_cards = c(3,2,2,2,2),
                                   var_positions = c(49,15,16,67,68),
                                   var_widths = c(6,1,1,1,1))
    cbs.nyt.20021031.repins <- cbs.nyt.20021031 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    cbs.nyt.20021031.demins <- cbs.nyt.20021031 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20021031.repouts <- cbs.nyt.20021031 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    cbs.nyt.20021031.demouts <- cbs.nyt.20021031 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    cbs.nyt.20021031.ins <- rbind(cbs.nyt.20021031.repins, cbs.nyt.20021031.demins)
    wpct(cbs.nyt.20021031.ins$fav, as.numeric(cbs.nyt.20021031.ins$weight)) # 89.35
    length(cbs.nyt.20021031.ins$weight) # 870
    
    cbs.nyt.20021031.outs <- rbind(cbs.nyt.20021031.repouts, cbs.nyt.20021031.demouts)
    wpct(cbs.nyt.20021031.outs$fav, as.numeric(cbs.nyt.20021031.outs$weight)) # 30.01
    length(cbs.nyt.20021031.outs$weight) # 841
    
    rm(cbs.nyt.20021031, 
       cbs.nyt.20021031.repins, cbs.nyt.20021031.demins,
       cbs.nyt.20021031.repouts, cbs.nyt.20021031.demouts,
       cbs.nyt.20021031.ins, cbs.nyt.20021031.outs)


  # Princeton ----
    # January 12, 1997 ----
    princeton.19970112 <- read_ascii("Data/Surveys/pew97001.dat", # Good
                             total_cards = 5,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(61,25,24,33,34),
                             var_widths = c(4,1,1,1,1),
                             var_cards = c(5,3,3,4,4))
    princeton.19970112.repins <- princeton.19970112 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19970112.demins <- princeton.19970112 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19970112.repouts <- princeton.19970112 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19970112.demouts <- princeton.19970112 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19970112.ins <- rbind(princeton.19970112.repins, princeton.19970112.demins)
    wpct(princeton.19970112.ins$fav, as.numeric(princeton.19970112.ins$weight)) # 88.15
    length(princeton.19970112.ins$weight) # 1320
    
    princeton.19970112.outs <- rbind(princeton.19970112.repouts, princeton.19970112.demouts)
    wpct(princeton.19970112.outs$fav, as.numeric(princeton.19970112.outs$weight)) # 30.75
    length(princeton.19970112.outs$weight) # 1314
    
    rm(princeton.19970112, 
       princeton.19970112.repins, princeton.19970112.demins,
       princeton.19970112.repouts, princeton.19970112.demouts,
       princeton.19970112.ins, princeton.19970112.outs)
    
    # June 22, 1997 ----
    princeton.19970622 <- read_ascii("Data/Surveys/pew97006.dat", # Good
                             total_cards = 7,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(71,32,31,8,9),
                             var_widths = c(4,1,1,1,1),
                             var_cards = c(7,5,5,6,6))
    princeton.19970622.repins <- princeton.19970622 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19970622.demins <- princeton.19970622 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19970622.repouts <- princeton.19970622 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19970622.demouts <- princeton.19970622 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19970622.ins <- rbind(princeton.19970622.repins, princeton.19970622.demins)
    wpct(princeton.19970622.ins$fav, as.numeric(princeton.19970622.ins$weight)) # 88.38
    length(princeton.19970622.ins$weight) # 857
    
    princeton.19970622.outs <- rbind(princeton.19970622.repouts, princeton.19970622.demouts)
    wpct(princeton.19970622.outs$fav, as.numeric(princeton.19970622.outs$weight)) # 30.84
    length(princeton.19970622.outs$weight) # 847
    
    rm(princeton.19970622, 
       princeton.19970622.repins, princeton.19970622.demins,
       princeton.19970622.repouts, princeton.19970622.demouts,
       princeton.19970622.ins, princeton.19970622.outs)
  
    # August 10, 1997 ----
    princeton.19970810 <- read_ascii("Data/Surveys/aug97nii.dat", # Good
                             total_cards = 1,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(253,50,49,188,189),
                             var_widths = c(3,1,1,1,1))
    princeton.19970810.repins <- princeton.19970810 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19970810.demins <- princeton.19970810 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19970810.repouts <- princeton.19970810 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19970810.demouts <- princeton.19970810 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19970810.ins <- rbind(princeton.19970810.repins, princeton.19970810.demins)
    wpct(princeton.19970810.ins$fav, as.numeric(princeton.19970810.ins$weight)) # 83.46
    length(princeton.19970810.ins$weight) # 536
    
    princeton.19970810.outs <- rbind(princeton.19970810.repouts, princeton.19970810.demouts)
    wpct(princeton.19970810.outs$fav, as.numeric(princeton.19970810.outs$weight)) # 23.35
    length(princeton.19970810.outs$weight) # 525
    
    rm(princeton.19970810, 
       princeton.19970810.repins, princeton.19970810.demins,
       princeton.19970810.repouts, princeton.19970810.demouts,
       princeton.19970810.ins, princeton.19970810.outs)
  
    # August 11, 1997 ----
    princeton.19970811 <- read_ascii("Data/Surveys/p97resp.dat", # Good
                             total_cards = 1,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(191,60,59,106,107),
                             var_widths = c(4,1,1,1,1))
    princeton.19970811.repins <- princeton.19970811 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19970811.demins <- princeton.19970811 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19970811.repouts <- princeton.19970811 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19970811.demouts <- princeton.19970811 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19970811.ins <- rbind(princeton.19970811.repins, princeton.19970811.demins)
    wpct(princeton.19970811.ins$fav, as.numeric(princeton.19970811.ins$weight)) # 86.73
    length(princeton.19970811.ins$weight) # 2347
    
    princeton.19970811.outs <- rbind(princeton.19970811.repouts, princeton.19970811.demouts)
    wpct(princeton.19970811.outs$fav, as.numeric(princeton.19970811.outs$weight)) # 42.13
    length(princeton.19970811.outs$weight) # 2319
    
    rm(princeton.19970811, 
       princeton.19970811.repins, princeton.19970811.demins,
       princeton.19970811.repouts, princeton.19970811.demouts,
       princeton.19970811.ins, princeton.19970811.outs)
    
    # March 29, 1998 -----
    princeton.19980329 <- read_ascii("Data/Surveys/mar98nii.dat", # Good
                             total_cards = 1,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(252,90,89,188,189),
                             var_widths = c(3,1,1,1,1))
    princeton.19980329.repins <- princeton.19980329 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19980329.demins <- princeton.19980329 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19980329.repouts <- princeton.19980329 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19980329.demouts <- princeton.19980329 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19980329.ins <- rbind(princeton.19980329.repins, princeton.19980329.demins)
    wpct(princeton.19980329.ins$fav, as.numeric(princeton.19980329.ins$weight)) # 86.84
    length(princeton.19980329.ins$weight) # 1060
    
    princeton.19980329.outs <- rbind(princeton.19980329.repouts, princeton.19980329.demouts)
    wpct(princeton.19980329.outs$fav, as.numeric(princeton.19980329.outs$weight)) # 28.45
    length(princeton.19980329.outs$weight) # 1041
    
    rm(princeton.19980329, 
       princeton.19980329.repins, princeton.19980329.demins,
       princeton.19980329.repouts, princeton.19980329.demouts,
       princeton.19980329.ins, princeton.19980329.outs)
  
    # September 8, 1998 ----
    princeton.19980908 <- read_ascii("Data/Surveys/p98009pol.dat", # Good
                             total_cards = 6,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(71,62,63,29,32),
                             var_widths = c(4,1,1,1,1),
                             var_cards = c(6,2,2,4,4))
    princeton.19980908.repins <- princeton.19980908 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                       ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19980908.demins <- princeton.19980908 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19980908.repouts <- princeton.19980908 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19980908.demouts <- princeton.19980908 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19980908.ins <- rbind(princeton.19980908.repins, princeton.19980908.demins)
    wpct(princeton.19980908.ins$fav, as.numeric(princeton.19980908.ins$weight)) # 90.80
    length(princeton.19980908.ins$weight) # 1966
    
    princeton.19980908.outs <- rbind(princeton.19980908.repouts, princeton.19980908.demouts)
    wpct(princeton.19980908.outs$fav, as.numeric(princeton.19980908.outs$weight)) # 33.59
    length(princeton.19980908.outs$weight) # 1909
    
    rm(princeton.19980908, 
       princeton.19980908.repins, princeton.19980908.demins,
       princeton.19980908.repouts, princeton.19980908.demouts,
       princeton.19980908.ins, princeton.19980908.outs)
  
    # October 18, 1998 ----
    princeton.19981018 <- read_ascii("Data/Surveys/oct98nii.dat", # Good
                             total_cards = 4,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(71,32,33,16,17),
                             var_widths = c(3,1,1,1,1),
                             var_cards = c(4,2,2,3,3))
    princeton.19981018.repins <- princeton.19981018 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19981018.demins <- princeton.19981018 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19981018.repouts <- princeton.19981018 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19981018.demouts <- princeton.19981018 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19981018.ins <- rbind(princeton.19981018.repins, princeton.19981018.demins)
    wpct(princeton.19981018.ins$fav, as.numeric(princeton.19981018.ins$weight)) # 89.83
    length(princeton.19981018.ins$weight) # 1325
    
    princeton.19981018.outs <- rbind(princeton.19981018.repouts, princeton.19981018.demouts)
    wpct(princeton.19981018.outs$fav, as.numeric(princeton.19981018.outs$weight)) # 26.53
    length(princeton.19981018.outs$weight) # 1310
    
    rm(princeton.19981018, 
       princeton.19981018.repins, princeton.19981018.demins,
       princeton.19981018.repouts, princeton.19981018.demouts,
       princeton.19981018.ins, princeton.19981018.outs)
    
    # December 13, 1998 ----
    princeton.19981213 <- read_ascii("Data/Surveys/p199812nii.dat", # Good
                             total_cards = 1,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(133,80,79,112,113),
                             var_widths = c(4,1,1,1,1))
    princeton.19981213.repins <- princeton.19981213 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19981213.demins <- princeton.19981213 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19981213.repouts <- princeton.19981213 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19981213.demouts <- princeton.19981213 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19981213.ins <- rbind(princeton.19981213.repins, princeton.19981213.demins)
    wpct(princeton.19981213.ins$fav, as.numeric(princeton.19981213.ins$weight)) # 82.75
    length(princeton.19981213.ins$weight) # 1010
    
    princeton.19981213.outs <- rbind(princeton.19981213.repouts, princeton.19981213.demouts)
    wpct(princeton.19981213.outs$fav, as.numeric(princeton.19981213.outs$weight)) # 28.97
    length(princeton.19981213.outs$weight) # 987
    
    rm(princeton.19981213, 
       princeton.19981213.repins, princeton.19981213.demins,
       princeton.19981213.repouts, princeton.19981213.demouts,
       princeton.19981213.ins, princeton.19981213.outs)
  
    # January 17, 1999 ----
    princeton.19990117 <- read_ascii("Data/Surveys/jan99nii.dat", # Good
                             total_cards = 6,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(71,49,48,18,19),
                             var_widths = c(3,1,1,1,1),
                             var_cards = c(6,2,2,3,3))
    princeton.19990117.repins <- princeton.19990117 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19990117.demins <- princeton.19990117 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19990117.repouts <- princeton.19990117 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19990117.demouts <- princeton.19990117 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19990117.ins <- rbind(princeton.19990117.repins, princeton.19990117.demins)
    wpct(princeton.19990117.ins$fav, as.numeric(princeton.19990117.ins$weight)) # 84.35
    length(princeton.19990117.ins$weight) # 1032
    
    princeton.19990117.outs <- rbind(princeton.19990117.repouts, princeton.19990117.demouts)
    wpct(princeton.19990117.outs$fav, as.numeric(princeton.19990117.outs$weight)) # 23.35
    length(princeton.19990117.outs$weight) # 1008
    
    rm(princeton.19990117, 
       princeton.19990117.repins, princeton.19990117.demins,
       princeton.19990117.repouts, princeton.19990117.demouts,
       princeton.19990117.ins, princeton.19990117.outs)

    # February 21, 1999 ----
    princeton.19990221 <- read_ascii("Data/Surveys/feb99nii.dat", # Good
                             total_cards = 7,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(71,41,40,47,48),
                             var_widths = c(3,1,1,1,1),
                             var_cards = c(7,2,2,3,3))
    princeton.19990221.repins <- princeton.19990221 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19990221.demins <- princeton.19990221 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19990221.repouts <- princeton.19990221 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19990221.demouts <- princeton.19990221 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19990221.ins <- rbind(princeton.19990221.repins, princeton.19990221.demins)
    wpct(princeton.19990221.ins$fav, as.numeric(princeton.19990221.ins$weight)) # 84.95
    length(princeton.19990221.ins$weight) # 1050
    
    princeton.19990221.outs <- rbind(princeton.19990221.repouts, princeton.19990221.demouts)
    wpct(princeton.19990221.outs$fav, as.numeric(princeton.19990221.outs$weight)) # 23.97
    length(princeton.19990221.outs$weight) # 1038
    
    rm(princeton.19990221, 
       princeton.19990221.repins, princeton.19990221.demins,
       princeton.19990221.repouts, princeton.19990221.demouts,
       princeton.19990221.ins, princeton.19990221.outs)
    
    # September 9, 1999 ----
    princeton.19990909 <- read_ascii("Data/Surveys/typo99.dat", # Good
                             total_cards = 1,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(234,126,127,177,188),
                             var_widths = c(4,1,1,1,1))
    princeton.19990909.repins <- princeton.19990909 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.19990909.demins <- princeton.19990909 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19990909.repouts <- princeton.19990909 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.19990909.demouts <- princeton.19990909 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.19990909.ins <- rbind(princeton.19990909.repins, princeton.19990909.demins)
    wpct(princeton.19990909.ins$fav, as.numeric(princeton.19990909.ins$weight)) # 89.65
    length(princeton.19990909.ins$weight) # 3432
    
    princeton.19990909.outs <- rbind(princeton.19990909.repouts, princeton.19990909.demouts)
    wpct(princeton.19990909.outs$fav, as.numeric(princeton.19990909.outs$weight)) # 27.08
    length(princeton.19990909.outs$weight) # 3392
    
    rm(princeton.19990909, 
       princeton.19990909.repins, princeton.19990909.demins,
       princeton.19990909.repouts, princeton.19990909.demouts,
       princeton.19990909.ins, princeton.19990909.outs)
  
    # July 12, 2001 ----
    princeton.20010712 <- read_ascii("Data/Surveys/july01c.dat", # Good
                             total_cards = 1,
                             var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
                             var_positions = c(123,58,59,80,81),
                             var_widths = c(4,1,1,1,1))
    princeton.20010712.repins <- princeton.20010712 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    princeton.20010712.demins <- princeton.20010712 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.20010712.repouts <- princeton.20010712 %>%
      filter(pid == 1 | pid.lean == 1) %>%
      mutate(dem.fav = ifelse(dem.fav == 1 | dem.fav == 2, 1,
                              ifelse(dem.fav == 3 | dem.fav == 4, 2, 9))) %>%
      filter(dem.fav == 1 | dem.fav == 2) %>%
      rename(fav = dem.fav) %>%
      select(weight, fav)
    princeton.20010712.demouts <- princeton.20010712 %>%
      filter(pid == 2 | pid.lean == 2) %>%
      mutate(rep.fav = ifelse(rep.fav == 1 | rep.fav == 2, 1,
                              ifelse(rep.fav == 3 | rep.fav == 4, 2, 9))) %>%
      filter(rep.fav == 1 | rep.fav == 2) %>%
      rename(fav = rep.fav) %>%
      select(weight, fav)
    
    princeton.20010712.ins <- rbind(princeton.20010712.repins, princeton.20010712.demins)
    wpct(princeton.20010712.ins$fav, as.numeric(princeton.20010712.ins$weight)) # 90.12
    length(princeton.20010712.ins$weight) # 864
    
    princeton.20010712.outs <- rbind(princeton.20010712.repouts, princeton.20010712.demouts)
    wpct(princeton.20010712.outs$fav, as.numeric(princeton.20010712.outs$weight)) # 26.43
    length(princeton.20010712.outs$weight) # 855
    
    rm(princeton.20010712, 
       princeton.20010712.repins, princeton.20010712.demins,
       princeton.20010712.repouts, princeton.20010712.demouts,
       princeton.20010712.ins, princeton.20010712.outs)
    
    # September 10, 2000 ----
    # princeton.20000910 <- read_ascii("Data/Surveys/Typol00c.dat", # Weights don't work
    #                          total_cards = 1,
    #                          var_names = c("weight", "rep.fav", "dem.fav", "pid"),
    #                          var_positions = c(377,91,92,154),
    #                          var_widths = c(8,1,1,1))
    
    # January 7, 2000 ----
    # princeton.20000107 <- read_ascii("Data/Surveys/jannii01c.dat", # Weight not working
    #                          total_cards = 1,
    #                          var_names = c("weight", "rep.fav", "dem.fav", "pid", "pid.lean"),
    #                          var_positions = c(219,137,138,188,189),
    #                          var_widths = c(8,1,1,1,1))
  
# Wcalc Dataframe ----
  # ANES ----
  ins <- rbind(anes.ins)
  outs <- rbind(anes.outs)
  
  # CBS ----
  cbs.ins <- as.data.frame(matrix(c("CBS", "11-28-1994", 84.42, 971,
                                    "CBS", "08-28-1996", 89.71, 1231,
                                    "CBS", "01-04-1999", 84.36, 1003,
                                    "CBS", "01-13-1999", 84.42, 1895,
                                    "CBS", "02-07-1999", 85.19, 520,
                                    "CBS", "06-06-1999", 80.81, 823,
                                    "CBS", "09-18-1999", 86.58, 1127,
                                    "CBS", "08-06-2000", 88.77, 450,
                                    "CBS", "08-30-2001", 85.60, 730),
                                  byrow = T,
                                  nrow = 9, ncol = 4))
  colnames(cbs.ins) <- c("VARNAME", "DATE", "VALUE", "N")
  
  cbs.outs <- as.data.frame(matrix(c("CBS", "11-28-1994", 24.27, 946,
                                     "CBS", "08-28-1996", 23.25, 1204,
                                     "CBS", "01-04-1999", 23.16, 996,
                                     "CBS", "01-13-1999", 21.34, 1868,
                                     "CBS", "02-07-1999", 23.29, 517,
                                     "CBS", "06-06-1999", 24.14, 810,
                                     "CBS", "09-18-1999", 22.91, 1113,
                                     "CBS", "08-06-2000", 24.34, 439,
                                     "CBS", "08-30-2001", 21.67, 730),
                                   byrow = T,
                                   nrow = 9, ncol = 4))
  colnames(cbs.outs) <- c("VARNAME", "DATE", "VALUE", "N")
  
  # CBS/NYT ----
  cbs.nyt.ins <- as.data.frame(matrix(c("CBSNYT", "11-02-1984", 90.23, 1377,
                                        "CBSNYT", "01-15-1990", 88.36, 1326,
                                        "CBSNYT", "05-30-1992", 77.79, 1105,
                                        "CBSNYT", "06-20-1992", 82.46, 1119,
                                        "CBSNYT", "01-17-1994", 83.19, 927,
                                        "CBSNYT", "09-11-1994", 83.69, 1002,
                                        "CBSNYT", "11-01-1994", 85.19, 1240,
                                        "CBSNYT", "04-04-1995", 89.52, 939,
                                        "CBSNYT", "08-09-1995", 84.63, 1266,
                                        "CBSNYT", "04-02-1996", 79.84, 1096,
                                        "CBSNYT", "01-17-1997", 85.24, 1120,
                                        "CBSNYT", "10-28-1998", 86.66, 970,
                                        "CBSNYT", "12-15-1998", 84.01, 1410,
                                        "CBSNYT", "12-17-1998", 83.85, 1677,
                                        "CBSNYT", "02-01-1999", 83.70, 909,
                                        "CBSNYT", "02-14-2000", 88.18, 1047,
                                        "CBSNYT", "05-13-2000", 87.50, 625,
                                        "CBSNYT", "07-23-2000", 89.63, 377,
                                        "CBSNYT", "10-01-2000", 89.44, 1011,
                                        "CBSNYT", "06-18-2001", 84.65, 886,
                                        "CBSNYT", "10-12-2001", 89.70, 991,
                                        "CBSNYT", "07-16-2002", 86.50, 848,
                                        "CBSNYT", "10-05-2002", 85.65, 583,
                                        "CBSNYT", "10-31-2002", 89.35, 870),
                                      byrow = T,
                                      nrow = 24, ncol = 4))
  colnames(cbs.nyt.ins) <- c("VARNAME", "DATE", "VALUE", "N")
  
  cbs.nyt.outs <- as.data.frame(matrix(c("CBSNYT", "11-02-1984", 25.06, 1432,
                                         "CBSNYT", "01-15-1990", 43.94, 1359,
                                         "CBSNYT", "05-30-1992", 23.57, 1092,
                                         "CBSNYT", "06-20-1992", 28.03, 1099,
                                         "CBSNYT", "01-17-1994", 31.39, 916,
                                         "CBSNYT", "09-11-1994", 26.65, 974,
                                         "CBSNYT", "11-01-1994", 21.57, 1224,
                                         "CBSNYT", "04-04-1995", 18.92, 912,
                                         "CBSNYT", "08-09-1995", 25.68, 1260,
                                         "CBSNYT", "04-02-1996", 17.14, 1092,
                                         "CBSNYT", "01-17-1997", 28.71, 1096,
                                         "CBSNYT", "10-28-1998", 23.66, 955,
                                         "CBSNYT", "12-15-1998", 22.48, 1388,
                                         "CBSNYT", "12-17-1998", 21.48, 1650,
                                         "CBSNYT", "02-01-1999", 18.90, 903,
                                         "CBSNYT", "10-01-2000", 24.80, 988,
                                         "CBSNYT", "02-14-2000", 32.05, 1025,
                                         "CBSNYT", "05-13-2000", 26.09, 620,
                                         "CBSNYT", "07-23-2000", 23.28, 371,
                                         "CBSNYT", "10-12-2001", 25.14, 966,
                                         "CBSNYT", "06-18-2001", 25.15, 875,
                                         "CBSNYT", "07-16-2002", 30.24, 825,
                                         "CBSNYT", "10-05-2002", 29.12, 573,
                                         "CBSNYT", "10-31-2002", 30.01, 841),
                                       byrow = T,
                                       nrow = 24, ncol = 4))
  colnames(cbs.nyt.outs) <- c("VARNAME", "DATE", "VALUE", "N")
  
  # GALLUP 1 ----
  gallup.ins <- as.data.frame(matrix(c("GALLUP", "09-01-1996", 92.09, 928,
                                        "GALLUP", "10-29-1997", 90.08, 904,
                                        "GALLUP", "12-16-1998", 88.47, 914,
                                        "GALLUP", "02-13-1999", 83.18, 909,
                                        "GALLUP", "02-21-1999", 89.98, 886,
                                        "GALLUP", "05-02-1999", 87.00, 856,
                                        "GALLUP", "11-21-1999", 84.09, 857,
                                        "GALLUP", "12-20-1999", 90.18, 709,
                                        "GALLUP", "01-10-2000", 90.41, 1412,
                                        "GALLUP", "07-26-2000", 96.00, 896,
                                        "GALLUP", "08-05-2000", 94.06, 924,
                                        "GALLUP", "11-15-2000", 93.74, 916,
                                        "GALLUP", "01-14-2002", 93.31, 887,
                                        "GALLUP", "07-28-2002", 93.73, 851,
                                        "GALLUP", "11-10-2002", 89.71, 885,
                                        "GALLUP", "12-17-2002", 90.30, 889,
                                        "GALLUP", "01-04-2003", 91.36, 859,
                                        "GALLUP", "03-30-2003", 92.96, 859,
                                        "GALLUP", "01-05-2004", 87.45, 925,
                                        "GALLUP", "02-01-2004", 94.27, 925,
                                        "GALLUP", "07-21-2004", 89.11, 459,
                                        "GALLUP", "08-01-2004", 95.08, 1382,
                                        "GALLUP", "08-25-2004", 90.77, 419,
                                        "GALLUP", "09-05-2004", 93.99, 930,
                                        "GALLUP", "02-06-2005", 87.40, 918,
                                        "GALLUP", "02-27-2005", 90.70, 897,
                                        "GALLUP", "04-02-2005", 85.58, 925,
                                        "GALLUP", "07-24-2005", 90.90, 888,
                                        "GALLUP", "09-11-2005", 88.88, 866,
                                        "GALLUP", "12-18-2005", 84.17, 873,
                                        "GALLUP", "04-30-2006", 76.64, 896,
                                        "GALLUP", "07-30-2006", 90.15, 888,
                                        "GALLUP", "10-08-2006", 84.78, 878,
                                        "GALLUP", "04-15-2007", 92.88, 905,
                                        "GALLUP", "07-08-2007", 85.50, 869,
                                        "GALLUP", "11-04-2007", 93.48, 883,
                                        "GALLUP", "01-13-2008", 91.45, 1749,
                                        "GALLUP", "02-10-2008", 90.20, 921,
                                        "GALLUP", "04-20-2008", 84.33, 919,
                                        "GALLUP", "08-23-2008", 87.02, 904,
                                        "GALLUP", "08-31-2008", 91.07, 1808,
                                        "GALLUP", "09-07-2008", 89.69, 928,
                                        "GALLUP", "10-12-2008", 87.93, 1135,
                                        "GALLUP", "11-16-2008", 86.86, 904,
                                        "GALLUP", "05-31-2009", 81.97, 870,
                                        "GALLUP", "03-28-2010", 80.64, 929,
                                        "GALLUP", "05-25-2010", 73.54, 930,
                                        "GALLUP", "11-17-2010", 81.71, 903,
                                        "GALLUP", "01-16-2011", 87.31, 895,
                                        "GALLUP", "04-23-2011", 79.95, 876,
                                        "GALLUP", "02-05-2012", 84.21, 922,
                                        "GALLUP", "02-19-2012", 85.09, 880,
                                        "GALLUP", "08-22-2012", 82.12, 926,
                                        "GALLUP", "11-12-2012", 88.42, 890,
                                        "GALLUP", "06-04-2013", 82.07, 1322,
                                        "GALLUP", "10-06-2013", 77.36, 848,
                                        "GALLUP", "12-08-2013", 75.71, 864,
                                        "GALLUP", "04-30-2014", 75.23, 1303,
                                        "GALLUP", "11-09-2014", 76.33, 714,
                                        "GALLUP", "03-08-2015", 74.21, 851),
                                      byrow = T,
                                      nrow = 60, ncol = 4))
  colnames(gallup.ins) <- c("VARNAME", "DATE", "VALUE", "N")
  
  gallup.outs <- as.data.frame(matrix(c("GALLUP", "09-01-1996", 25.22, 909,
                                         "GALLUP", "10-29-1997", 22.12, 878,
                                         "GALLUP", "12-16-1998", 23.28, 890,
                                         "GALLUP", "02-13-1999", 21.37, 902,
                                         "GALLUP", "02-21-1999", 19.81, 864,
                                         "GALLUP", "05-02-1999", 22.02, 826,
                                         "GALLUP", "11-21-1999", 25.99, 845,
                                         "GALLUP", "12-20-1999", 18.83, 692,
                                         "GALLUP", "01-10-2000", 32.65, 1384,
                                         "GALLUP", "07-26-2000", 20.81, 850,
                                         "GALLUP", "08-05-2000", 24.05, 894,
                                         "GALLUP", "11-15-2000", 17.61, 888,
                                         "GALLUP", "01-14-2002", 37.13, 841,
                                         "GALLUP", "07-28-2002", 32.65, 817,
                                         "GALLUP", "11-10-2002", 25.83, 849,
                                         "GALLUP", "12-17-2002", 24.02, 857,
                                         "GALLUP", "01-04-2003", 25.62, 832,
                                         "GALLUP", "03-30-2003", 26.50, 830,
                                         "GALLUP", "01-05-2004", 20.37, 895,
                                         "GALLUP", "02-01-2004", 21.08, 898,
                                         "GALLUP", "07-21-2004", 16.29, 446,
                                         "GALLUP", "08-01-2004", 14.42, 1333,
                                         "GALLUP", "08-25-2004", 26.67, 410,
                                         "GALLUP", "09-05-2004", 19.37, 896,
                                         "GALLUP", "02-06-2005", 19.60, 900,
                                         "GALLUP", "02-27-2005", 21.24, 879,
                                         "GALLUP", "04-02-2005", 24.24, 917,
                                         "GALLUP", "07-24-2005", 21.06, 874,
                                         "GALLUP", "09-11-2005", 19.10, 849,
                                         "GALLUP", "12-18-2005", 21.48, 864,
                                         "GALLUP", "04-30-2006", 15.34, 911,
                                         "GALLUP", "07-30-2006", 13.66, 875,
                                         "GALLUP", "10-08-2006", 17.11, 861,
                                         "GALLUP", "04-15-2007", 15.41, 886,
                                         "GALLUP", "07-08-2007", 14.83, 866,
                                         "GALLUP", "11-04-2007", 15.54, 876,
                                         "GALLUP", "01-13-2008", 16.71, 1739,
                                         "GALLUP", "02-10-2008", 15.93, 895,
                                         "GALLUP", "04-20-2008", 19.22, 906,
                                         "GALLUP", "08-23-2008", 18.44, 891,
                                         "GALLUP", "08-31-2008", 12.19, 177,
                                         "GALLUP", "09-07-2008", 18.90, 922,
                                         "GALLUP", "10-12-2008", 16.68, 1138,
                                         "GALLUP", "11-16-2008", 13.64, 904,
                                         "GALLUP", "05-31-2009", 18.17, 872,
                                         "GALLUP", "03-28-2010", 12.69, 919,
                                         "GALLUP", "05-25-2010", 18.95, 938,
                                         "GALLUP", "11-17-2010", 14.93, 906,
                                         "GALLUP", "01-16-2011", 18.73, 871,
                                         "GALLUP", "04-23-2011", 21.02, 883,
                                         "GALLUP", "02-05-2012", 15.30, 918,
                                         "GALLUP", "02-19-2012", 14.15, 874,
                                         "GALLUP", "08-22-2012", 14.18, 928,
                                         "GALLUP", "11-12-2012", 14.98, 878,
                                         "GALLUP", "06-04-2013", 16.92, 1308,
                                         "GALLUP", "10-06-2013", 9.52, 852,
                                         "GALLUP", "12-08-2013", 10.47, 865,
                                         "GALLUP", "04-30-2014", 13.08, 1302,
                                         "GALLUP", "11-09-2014", 12.26, 712,
                                         "GALLUP", "03-08-2015", 16.30, 858),
                                       byrow = T,
                                       nrow = 60, ncol = 4))
  colnames(gallup.outs) <- c("VARNAME", "DATE", "VALUE", "N")
  
  # GALLUP 2 ----
  gallupb.ins <- as.data.frame(matrix(c("GALLUPB", "10-13-1980", 68.74, 1374,
                                        "GALLUPB", "10-20-1991", 71.05, 862,
                                        "GALLUPB", "12-06-1992", 62.97, 903,
                                        "GALLUPB", "11-29-1994", 71.86, 953,
                                        "GALLUPB", "11-15-1998", 59.17, 903,
                                        "GALLUPB", "12-13-1998", 59.24, 756,
                                        "GALLUPB", "01-17-1999", 58.95, 900),
                                      byrow = T,
                                      nrow = 7, ncol = 4))
  colnames(gallupb.ins) <- c("VARNAME", "DATE", "VALUE", "N")
  
  gallupb.outs <- as.data.frame(matrix(c("GALLUPB", "10-13-1980", 54.59, 1370,
                                         "GALLUPB", "10-20-1991", 50.24, 858,
                                         "GALLUPB", "12-06-1992", 45.72, 897,
                                         "GALLUPB", "11-29-1994", 36.30, 954,
                                         "GALLUPB", "11-15-1998", 39.06, 931,
                                         "GALLUPB", "12-13-1998", 38.01, 754,
                                         "GALLUPB", "01-17-1999", 39.96, 900),
                                       byrow = T,
                                       nrow = 7, ncol = 4))
  colnames(gallupb.outs) <- c("VARNAME", "DATE", "VALUE", "N")
  
  # PRINCETON ----
  princeton.ins <- as.data.frame(matrix(c("PRINCETON", "01-12-1997", 88.15, 1320,
                                          "PRINCETON", "06-22-1997", 88.38, 857,
                                          "PRINCETON", "08-10-1997", 83.46, 536,
                                          "PRINCETON", "08-11-1997", 86.73, 2347,
                                          "PRINCETON", "03-29-1998", 86.84, 1060,
                                          "PRINCETON", "09-08-1998", 90.80, 1966,
                                          "PRINCETON", "10-18-1998", 89.93, 1325,
                                          "PRINCETON", "12-13-1998", 82.75, 1010,
                                          "PRINCETON", "01-17-1999", 84.35, 1032,
                                          "PRINCETON", "02-21-1999", 84.95, 1050,
                                          "PRINCETON", "09-09-1999", 89.65, 3432,
                                          "PRINCETON", "07-12-2001", 90.12, 864),
                                        byrow = T,
                                        nrow = 12, ncol = 4))
  colnames(princeton.ins) <- c("VARNAME", "DATE", "VALUE", "N")
  
  princeton.outs <- as.data.frame(matrix(c("PRINCETON", "01-12-1997", 30.75, 1314,
                                           "PRINCETON", "06-22-1997", 30.84, 847,
                                           "PRINCETON", "08-10-1997", 23.25, 525,
                                           "PRINCETON", "08-11-1997", 42.13, 2319,
                                           "PRINCETON", "03-29-1998", 28.45, 1041,
                                           "PRINCETON", "09-08-1998", 33.59, 1909,
                                           "PRINCETON", "10-18-1998", 26.53, 1310,
                                           "PRINCETON", "12-13-1998", 28.97, 987,
                                           "PRINCETON", "01-17-1999", 23.35, 1008,
                                           "PRINCETON", "02-21-1999", 23.97, 1038,
                                           "PRINCETON", "09-09-1999", 27.08, 3392,
                                           "PRINCETON", "07-12-2001", 26.43, 855),
                                         byrow = T,
                                         nrow = 12, ncol = 4))
  colnames(princeton.outs) <- c("VARNAME", "DATE", "VALUE", "N")
  
  # Combine all surveys ----
  ins <- rbind(ins, cbs.ins, cbs.nyt.ins, 
               gallup.ins, gallupb.ins, princeton.ins)
  ins <- ins %>%
    mutate(VALUE = round(as.numeric(VALUE), 2))
  
  outs <- rbind(outs, cbs.outs, cbs.nyt.outs, 
               gallup.outs, gallupb.outs, princeton.outs)
  outs <- outs %>%
    mutate(VALUE = round(as.numeric(VALUE), 2))
  
  # Write DTAs ----
  ins$DATE <- as.Date(ins$DATE, "%m-%d-%Y")
  ins$N <- as.numeric(ins$N)
  write.dta(ins, file = "Data/InPartyFavorability-Marginals.dta") # take to STATA to save (for date conversion)
  
  outs$DATE <- as.Date(outs$DATE, "%m-%d-%Y")
  outs$N <- as.numeric(outs$N)
  write.dta(outs, file = "Data/OutPartyFavorability-Marginals.dta") # take to STATA to save (for date conversion)
  

  