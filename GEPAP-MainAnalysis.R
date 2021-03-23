# Gridlock, Elite Polarization, and Attitudes about the Parties
# Main Analysis
# Author: Max Allamong
# Created: Oct. 20th, 2020
# Last Updated: Oct. 20th, 2020

# READ ME

# Packages ----
  #install.packages(c("tidyverse", "readxl", "systemfit", "haven", "aTSA", "mice", "here", "conflicted"))

# Libraries ----
  library(tidyverse)
  library(readxl)
  library(systemfit)
  library(haven)
  library(aTSA)
  library(mice)
  library(here)
  library(conflicted)
    conflict_prefer("filter","dplyr")

# Working Directory ----
  setwd(here())
  #setwd()

# Load Data ----
  gridlock <- read_excel("Data/binder_gridlock_series_1947-2016_salient_issues.xlsx")
  infav <- read.csv("Data/Favorability/InPartyFavorability-Biennial.csv", header = F)
  outfav <- read.csv("Data/Favorability/OutPartyFavorability-Biennial.csv", header = F)
  polar <- read.csv("Data/dwnominate.csv", header = T)
  gdp <- read.csv("Data/GDP.csv")
  anes <- read_dta("Data/anes_timeseries_cdf.dta") 
  
# Clean Data ----
   
  # Gridlock ----
  gridlock <- gridlock %>% 
    rename(congress = Congress, cong.yearstart = 'Start of Congress', pct.gridlock = 'Percent salient issues in deadlock') %>%
    slice(1:35) %>%
    filter(cong.yearstart >= 1977) %>%
    mutate(year = cong.yearstart + 1)

  # Elite Polarization ----
  polar <- polar %>%
    filter(congress >= 95 & congress <= 114) %>%
    filter(party_name == "Democrat" | party_name == "Republican") %>%
    filter(chamber != "President") %>%
    select(congress, chamber, party_name, nominate_dim1_median) %>%
    group_by(party_name) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = party_name, values_from = nominate_dim1_median) %>%
    select(-row)
    
  house.polar <- polar %>%
    filter(chamber == "House") %>%
    mutate(house.polar = abs(Republican - Democrat)) %>%
    mutate(year = seq(1978, 2016, 2)) %>%
    select(year, house.polar)

  senate.polar <- polar %>%
    filter(chamber == "Senate") %>%
    mutate(senate.polar = abs(Republican - Democrat)) %>%
    mutate(year = seq(1978, 2016, 2)) %>%
    select(year, senate.polar)

  
  # Favorability ----
  infav <- infav %>%
    rename(year = V1, infav = V3) %>%
    select(year, infav)
  
  outfav <- outfav %>%
    rename(year = V1, outfav = V3) %>%
    select(year, outfav)
  
  # Controls ----
  # Divided Government
  divgov <- c(0,0,1,1,1,1,1,1,0,1,1,1,1,0,0,1,0,1,1,1)
  
  # ANES
  anes <- anes %>% 
    select(VCF0004, VCF0301, VCF0236) %>%
    rename(year = VCF0004, pid = VCF0301) %>%
    mutate(strong.partisans = ifelse(pid == 1 | pid == 7, 1, 0)) %>%
    group_by(year) %>%
    summarize(strong.partisans = 100*mean(strong.partisans, na.rm = T))
  
  # GDP
  gdp <- gdp %>% 
    mutate(group = rep(1:20, each = 2)) %>%
    group_by(group) %>%
    summarize(gdp = mean(GDP)) %>%
    mutate(year = seq(1978,2016,2)) %>%
    select(gdp,year)
  
  # Combine data ----
  # Create levels data frame
  mydata <- gridlock %>%
    left_join(infav, by = "year") %>%
    left_join(outfav, by = "year") %>%
    left_join(house.polar, by = "year") %>%
    left_join(senate.polar, by = "year") %>%
    left_join(anes, by = "year") %>%
    left_join(gdp, by = "year") %>%
    mutate(divgov = divgov) %>%
    mutate(house.polar = house.polar) %>%
    mutate(senate.polar = senate.polar)
  
  
  # Impute Pct. Strong Partisans
  mydata <- mydata %>%
    select(pct.gridlock, strong.partisans, year, 
           infav, outfav, house.polar, senate.polar, 
           gdp, divgov)
  temp <- mice(mydata, m=5, maxit = 50, meth = "pmm", seed = 500)
  mydata <- complete(temp, 1)  
  
  # Create changes data frame
  mydata.diff <- data.frame(d.infav = diff(mydata$infav), d.outfav = diff(mydata$outfav),
                            d.senpol = diff(mydata$senate.polar), d.housepol = diff(mydata$house.polar),
                            d.gridlock = diff(mydata$pct.gridlock), d.gdp = diff(mydata$gdp),
                            year = seq(1980,2016,2), d.strong.partisans = diff(mydata$strong.partisans),
                            strong.partisans = mydata$strong.partisans[-1], divgov = divgov[-1])

  

  
# Descriptive Plots ----
  
  # ANES ----
  
  ins <- ins %>%
    mutate(VARNAME = ifelse(VARNAME == "GALLUPB", "GALLUP 2", VARNAME))
  outs <- outs %>%
    mutate(VARNAME = ifelse(VARNAME == "GALLUPB", "GALLUP 2", VARNAME))
  ins$VARNAME <- paste0(ins$VARNAME, ", In-Party")
  outs$VARNAME <- paste0(outs$VARNAME, ", Out-Party")
  therms <- rbind(ins, outs)
  therms$DATE <- as.Date(therms$DATE, "%m-%d-%Y")
  
  pdf(height = 8, width = 16, "Figures/AllSurveys.pdf")
  therms %>%
    drop_na() %>%
    ggplot() +
    geom_line(aes(x = DATE, y = VALUE, group = VARNAME, colour = VARNAME), size = 1.5) +
    scale_color_discrete(name = "Survey, Variable") +
    scale_y_continuous(name = "Favorability", limits = c(0,100)) +
    scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size = 26),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          plot.margin = margin(1,1,1,1,"cm"),
          panel.border = element_rect(color="black", fill=NA), 
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          strip.background = element_rect(fill="white", color="black"))
  dev.off()
  
  # Gridlock ----
  pdf(height = 8, width = 12, "Figures/LevelsGridlock.pdf")
  mydata %>%
    ggplot() +
    geom_line(aes(x = year, y = pct.gridlock)) +
    #geom_smooth(aes(x = cong.yearstart, y = pct.gridlock), method = "lm", colour = "red", se = F) +
    scale_x_continuous(name = "Year", limits = c(1978,2016), breaks = seq(1980,2016,4)) +
    scale_y_continuous(name = "Percent of Gridlocked Issues", limits = c(0,100)) +
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size = 26),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          plot.margin = margin(1,1,1,1,"cm"),
          panel.border = element_rect(color="black", fill=NA), 
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          strip.background = element_rect(fill="white", color="black"))
  dev.off()
  
  pdf(height = 8, width = 12, "Figures/DifferencedGridlock.pdf")
  mydata.diff %>%
    ggplot() +
    geom_line(aes(x = year, y = d.gridlock)) +
    #geom_smooth(aes(x = cong.yearstart, y = pct.gridlock), method = "lm", colour = "red", se = F) +
    scale_x_continuous(name = "Year", limits = c(1978,2016), breaks = seq(1980,2016,4)) +
    scale_y_continuous(name = "Change in Percent of Gridlocked Issues", limits = c(-50,50)) +
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size = 26),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          plot.margin = margin(1,1,1,1,"cm"),
          panel.border = element_rect(color="black", fill=NA), 
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          strip.background = element_rect(fill="white", color="black"))
  dev.off()
  
  
  # Party Favorability ----
  pdf(height = 8, width = 14, "Figures/LevelsFavorability.pdf")
  mydata %>%
    select(year,infav,outfav) %>%
    pivot_longer(!year, names_to = "inout", values_to = "fav") %>%
    ggplot() +
    geom_line(aes(x = year, y = fav, group = inout, colour = inout), size = 2) +
    scale_color_discrete(name = "", labels = c("In-Party","Out-Party")) +
    scale_x_continuous(name = "Year", limits = c(1978,2016), breaks = seq(1980,2016,4)) +
    scale_y_continuous(name = "Party Favorability", limits = c(0,100)) +
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size = 26),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          plot.margin = margin(1,1,1,1,"cm"),
          panel.border = element_rect(color="black", fill=NA), 
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          strip.background = element_rect(fill="white", color="black"))
  dev.off()
  
  pdf(height = 8, width = 14, "Figures/DifferencedFavorability.pdf")
  mydata.diff %>%
    select(year, d.infav, d.outfav) %>%
    pivot_longer(!year, names_to = "inout", values_to = "fav") %>%
    ggplot() +
    geom_line(aes(x = year, y = fav, color = inout, group = inout), size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_discrete(name = "", labels = c("In-Party", "Out-Party")) +
    scale_x_continuous(name = "Year", limits = c(1980,2016), breaks = seq(1980,2016,4)) +
    scale_y_continuous(name = "Change in Party Favorability", limits = c(-6,6)) +
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size = 26),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          plot.margin = margin(1,1,1,1,"cm"),
          panel.border = element_rect(color="black", fill=NA), 
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          strip.background = element_rect(fill="white", color="black"))
  dev.off()

  mydata %>%
    group_by(midterm) %>%
    summarize(mean.infav = mean(infav), mean.outfav = mean(outfav))
  
  mydata.diff %>%
    group_by(midterm) %>%
    summarize(mean.infav = mean(d.infav), mean.outfav = mean(d.outfav))
  
  cor(mydata$house.polar, mydata$pct.gridlock) # 0.53
  cor(mydata$senate.polar, mydata$pct.gridlock) # 0.46
  
  cor(mydata.diff$d.housepol, mydata.diff$d.gridlock) # 0.05
  cor(mydata.diff$d.senpol, mydata.diff$d.gridlock) # -0.41
  
  # Elite Polarization ----
  pdf(height = 8, width = 14, "Figures/LevelsPolarization.pdf")
  mydata %>%
    select(year,house.polar,senate.polar) %>%
    pivot_longer(!year, names_to = "chamber", values_to = "polar") %>%
    ggplot() +
    geom_line(aes(x = year, y = polar, group = chamber, colour = chamber), size = 2) +
    scale_color_discrete(name = "", labels = c("House","Senate")) +
    scale_x_continuous(name = "Year", limits = c(1978,2016), breaks = seq(1980,2016,4)) +
    scale_y_continuous(name = "Elite Polarization") +
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size = 26),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          plot.margin = margin(1,1,1,1,"cm"),
          panel.border = element_rect(color="black", fill=NA), 
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          strip.background = element_rect(fill="white", color="black"))
  dev.off()
  
  pdf(height = 8, width = 14, "Figures/DifferencedPolarization.pdf")
  mydata.diff %>%
    select(year, d.housepol, d.senpol) %>%
    pivot_longer(!year, names_to = "chamber", values_to = "polar") %>%
    ggplot() +
    geom_line(aes(x = year, y = polar, color = chamber, group = chamber), size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_discrete(name = "", labels = c("House","Senate")) +
    scale_x_continuous(name = "Year", limits = c(1980,2016), breaks = seq(1980,2016,4)) +
    scale_y_continuous(name = "Change in Elite Polarization") +
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size = 26),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          plot.margin = margin(1,1,1,1,"cm"),
          panel.border = element_rect(color="black", fill=NA), 
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          strip.background = element_rect(fill="white", color="black"))
  dev.off()
  
  # Unit-root testing
  adf.test(mydata$pct.gridlock)
  adf.test(mydata$house.polar)
  adf.test(mydata$senate.polar)
  
  
# Analysis ----
  # Levels ----
  # In
  house.in <- lm(infav ~ pct.gridlock + house.polar + divgov + strong.partisans, mydata.imp)
  summary(house.in)
  
  senate.in <- lm(infav ~ pct.gridlock + senate.polar + divgov + strong.partisans, mydata.imp)
  summary(senate.in)
  
  # Out
  house.out <- lm(outfav ~ pct.gridlock + house.polar + divgov + strong.partisans, mydata.imp)
  summary(house.out)
  
  senate.out <- lm(outfav ~ pct.gridlock + senate.polar + divgov + strong.partisans, mydata.imp)
  summary(senate.out)

  
  # Changes ----
  # House
  house.in.diff <- lm(d.infav ~ d.gridlock + d.housepol + divgov + strong.partisans, data = mydata.diff.imp)
  summary(house.in.diff)  
  senate.in.diff <- lm(d.infav ~ d.gridlock + d.senpol + divgov + strong.partisans , data = mydata.diff.imp)
  summary(senate.in.diff)  
  
  # Senate
  house.out.diff <- lm(d.outfav ~ d.gridlock + d.housepol + divgov + strong.partisans, data = mydata.diff.imp)
  summary(house.out.diff)  
  senate.out.diff <- lm(d.outfav ~ d.gridlock + d.senpol + divgov + strong.partisans, data = mydata.diff.imp)
  summary(senate.out.diff)  
  
  
  # SUR ----
    # House (Differenced) ----
    # Simple Model
    h1 <- d.infav ~ d.gridlock + d.housepol
    h2 <- d.outfav ~ d.gridlock + d.housepol
    h.sur <- systemfit(list(inreg = h1, outreg = h2), data = mydata.diff)
    summary(h.sur)
    t.h.sur <- h.sur$coefficients / sqrt(diag(vcov(h.sur))) # save t-value
    p.h.sur <- ifelse(t.h.sur < 0, pt(t.h.sur, 16, lower = TRUE), pt(t.h.sur, 16, lower = FALSE)) # calculate p-value
    
    # Full Model
    h1.f <- d.infav ~ d.gridlock + d.housepol + divgov + d.strong.partisans
    h2.f <- d.outfav ~ d.gridlock + d.housepol + divgov + d.strong.partisans
    h.f.sur <- systemfit(list(inreg = h1.f, outreg = h2.f), data = mydata.diff)
    summary(h.f.sur)
    t.h.f <- h.f.sur$coefficients / sqrt(diag(vcov(h.f.sur))) # save t-value
    p.h.f <- ifelse(t.h.f < 0, pt(t.h.f, 16, lower = TRUE), pt(t.h.f, 16, lower = FALSE)) # calculate p-value
    
    # Full Model w/ Interaction
    h1.fi <- d.infav ~ d.gridlock * d.housepol + divgov + d.strong.partisans
    h2.fi <- d.outfav ~ d.gridlock * d.housepol + divgov + d.strong.partisans
    h.fi.sur <- systemfit(list(inreg = h1.fi, outreg = h2.fi), data = mydata.diff)
    summary(h.fi.sur)
    t.hi.f <- h.fi.sur$coefficients / sqrt(diag(vcov(h.fi.sur))) # save t-value
    p.hi.f <- ifelse(t.hi.f < 0, pt(t.hi.f, 16, lower = TRUE), pt(t.hi.f, 16, lower = FALSE)) # calculate p-value
    
    # Full Model w/ GDP
    h1.fgdp <- d.infav ~ d.gridlock + d.housepol + divgov + d.strong.partisans + d.gdp
    h2.fgdp <- d.outfav ~ d.gridlock + d.housepol + divgov + d.strong.partisans + d.gdp
    h.fgdp.sur <- systemfit(list(inreg = h1.fgdp, outreg = h2.fgdp), data = mydata.diff)
    summary(h.fgdp.sur)
    t.h.fgdp <- h.fgdp.sur$coefficients / sqrt(diag(vcov(h.fgdp.sur))) # save t-value
    p.h.fgdp <- ifelse(t.h.fgdp < 0, pt(t.h.fgdp, 16, lower = TRUE), pt(t.h.fgdp, 16, lower = FALSE)) # calculate p-value
    
    # Senate (Differenced) ----
    # Simple Model
    s1 <- d.infav ~ d.gridlock + d.senpol
    s2 <- d.outfav ~ d.gridlock + d.senpol
    s.sur <- systemfit(list(inreg = s1, outreg = s2), data = mydata.diff)
    summary(s.sur)
    t.s.sur <- s.sur$coefficients / sqrt(diag(vcov(s.sur))) # save t-value
    p.s.sur <- ifelse(t.s.sur < 0, pt(t.s.sur, 16, lower = TRUE), pt(t.s.sur, 16, lower = FALSE)) # calculate p-value
    
    # Full Model
    s1.f <- d.infav ~ d.gridlock + d.senpol + divgov + d.strong.partisans
    s2.f <- d.outfav ~ d.gridlock + d.senpol + divgov + d.strong.partisans
    s.f.sur <- systemfit(list(inreg = s1.f, outreg = s2.f), data = mydata.diff)
    summary(s.f.sur)
    t.s.f <- s.f.sur$coefficients / sqrt(diag(vcov(s.f.sur))) # save t-value
    p.s.f <- ifelse(t.s.f < 0, pt(t.s.f, 16, lower = TRUE), pt(t.s.f, 16, lower = FALSE)) # calculate p-value
    
    # Full Model w/ Interaction
    s1.fi <- d.infav ~ d.gridlock * d.senpol + divgov + d.strong.partisans
    s2.fi <- d.outfav ~ d.gridlock * d.senpol + divgov + d.strong.partisans
    s.fi.sur <- systemfit(list(inreg = s1.fi, outreg = s2.fi), data = mydata.diff)
    summary(s.fi.sur)
    t.s.fi <- s.fi.sur$coefficients / sqrt(diag(vcov(s.fi.sur))) # save t-value
    p.s.fi <- ifelse(t.s.fi < 0, pt(t.s.fi, 16, lower = TRUE), pt(t.s.fi, 16, lower = FALSE)) # calculate p-value
    
    # Full Model w/ GDP
    s1.fgdp <- d.infav ~ d.gridlock + d.senpol + divgov + d.strong.partisans + d.gdp
    s2.fgdp <- d.outfav ~ d.gridlock + d.senpol + divgov + d.strong.partisans + d.gdp
    s.fgdp.sur <- systemfit(list(inreg = s1.fgdp, outreg = s2.fgdp), data = mydata.diff)
    summary(s.fgdp.sur)
    t.s.fgdp <- s.fgdp.sur$coefficients / sqrt(diag(vcov(s.fgdp.sur))) # save t-value
    p.s.fgdp <- ifelse(t.s.fgdp < 0, pt(t.s.fgdp, 16, lower = TRUE), pt(t.s.fgdp, 16, lower = FALSE)) # calculate p-value
    
  # Substantive effects ----
  # House Pol, In-Party, Simple
  mean(abs(mydata.diff$d.housepol)) # typical change in d.housepol = 0.0183
  h.sur$coefficients[3]*mean(abs(mydata.diff$d.housepol)) # typical change in d.housepol produces 0.739 positive change in in-fav
  mean(abs(mydata.diff$d.infav)) # typical change in d.infav = 1.88
  (h.sur$coefficients[3]*mean(abs(mydata.diff$d.housepol))) / mean(abs(mydata.diff$d.infav)) # 40% of typical change in in-fav due to d.housepol
  (h.sur$coefficients[3]*mean(abs(mydata.diff$d.housepol))) / sd(mydata.diff$d.infav) # 0.31 std. deviations
    
  # House Pol, Out-Party, Full
  mean(abs(mydata.diff$d.housepol)) # typical change in d.housepol = 0.0183
  h.f.sur$coefficients[8]*mean(abs(mydata.diff$d.housepol)) # typical change in d.housepol produces 0.745 negative change in out-fav
  mean(abs(mydata.diff$d.outfav)) # typical change in d.outfav = 1.77
  (h.f.sur$coefficients[8]*mean(abs(mydata.diff$d.housepol))) / mean(abs(mydata.diff$d.outfav)) # 42% of typical change in out-fav due to d.housepol
  (h.f.sur$coefficients[8]*mean(abs(mydata.diff$d.housepol))) / sd(mydata.diff$d.outfav) # 0.34 std. deviations
  
  # Senate Pol, Out-Party, Full
  mean(abs(mydata.diff$d.senpol)) # typical change in d.senpol = 0.0159
  s.f.sur$coefficients[8]*mean(abs(mydata.diff$d.senpol)) # typical change in d.senpol produces 0.808 negative change in out-fav
  mean(abs(mydata.diff$d.outfav)) # typical change in d.outfav = 1.77
  (s.f.sur$coefficients[8]*mean(abs(mydata.diff$d.senpol))) / mean(abs(mydata.diff$d.outfav)) # 46% of typical change in in-fav due to d.senpol
  (s.f.sur$coefficients[8]*mean(abs(mydata.diff$d.senpol))) / sd(mydata.diff$d.outfav) # 0.36 std. deviations
  
  # House Gridlock, Out-Party, Full
  mean(abs(mydata.diff$d.gridlock)) # typical change in d.gridlock = 13.94
  h.f.sur$coefficients[7]*mean(abs(mydata.diff$d.gridlock)) # typical change in d.gridlock produces 1.047 negative change in out-fav
  mean(abs(mydata.diff$d.outfav)) # typical change in d.outfav = 1.77
  (h.f.sur$coefficients[7]*mean(abs(mydata.diff$d.gridlock))) / mean(abs(mydata.diff$d.outfav)) # 60% of typical change in in-fav due to d.housepol
  (h.f.sur$coefficients[7]*mean(abs(mydata.diff$d.gridlock))) / sd(mydata.diff$d.outfav) # 0.47 std. deviations

  # Senate Gridlock, In-Party, Full
  mean(abs(mydata.diff$d.gridlock)) # typical change in d.gridlock = 13.94
  s.f.sur$coefficients[2]*mean(abs(mydata.diff$d.gridlock)) # typical change in d.gridlock produces 0.55 negative change in in-fav
  mean(abs(mydata.diff$d.infav)) # typical change in d.outfav = 1.88
  (s.f.sur$coefficients[2]*mean(abs(mydata.diff$d.gridlock))) / mean(abs(mydata.diff$d.infav)) # 29% of typical change in in-fav due to d.housepol
  (s.f.sur$coefficients[2]*mean(abs(mydata.diff$d.gridlock))) / sd(mydata.diff$d.infav) # 0.23 std. deviations
  
  # Senate Gridlock, Out-Party, Full
  mean(abs(mydata.diff$d.gridlock)) # typical change in d.gridlock = 13.94
  s.f.sur$coefficients[7]*mean(abs(mydata.diff$d.gridlock)) # typical change in d.gridlock produces 1.26 negative change in out-fav
  mean(abs(mydata.diff$d.outfav)) # typical change in d.outfav = 1.77
  (s.f.sur$coefficients[7]*mean(abs(mydata.diff$d.gridlock))) / mean(abs(mydata.diff$d.outfav)) # 71% of typical change in out-fav due to d.housepol
  (s.f.sur$coefficients[7]*mean(abs(mydata.diff$d.gridlock))) / sd(mydata.diff$d.outfav) # 0.57 std. deviations
  
  # Marginal Effects ----
  
  # House, In-Party
  Hinfav.hats <- h.fi.sur$coefficients[1:6] # beta hats (house, infav)
  Hinfav.cov <- h.fi.sur$coefCov[1:6, 1:6] # variance-covariance (house, infav)
  
  z <- seq(min(mydata.diff$d.housepol), max(mydata.diff$d.housepol), length.out = 1000)
  dy.dx <- Hinfav.hats["inreg_d.gridlock"] + Hinfav.hats["inreg_d.gridlock:d.housepol"]*z
  se.dy.dx <- sqrt(Hinfav.cov["inreg_d.gridlock", "inreg_d.gridlock"] + 
                     z^2*Hinfav.cov["inreg_d.gridlock:d.housepol", "inreg_d.gridlock:d.housepol"] + 
                     2*z*Hinfav.cov["inreg_d.gridlock", "inreg_d.gridlock:d.housepol"])
  upr <- dy.dx + 1.645*se.dy.dx
  lwr <- dy.dx - 1.645*se.dy.dx
  
  pdf(height = 6, width = 8, "Figures/ME-Gridlock-InParty-House.pdf")
  par(family="serif",bty="l",mar=c(5,5.5,2,2), cex = 1.5)
  plot(x=z, y=dy.dx,type="n",xlim=c(min(z),max(z)),
       ylim=c(1.2*min(lwr), 1.2*max(upr)),
       xlab = TeX("$\\Delta$ Elite Polarization"),
       ylab = TeX("ME of $\\Delta$Gridlock on $\\Delta$In-Party Favorability"))
  lines(z, dy.dx, lwd = 3)
  lines(z, lwr)
  lines(z, upr)
  abline(h=0,lty=2)
  dev.off()
  
  # House, Out-Party
  Houtfav.hats <- h.fi.sur$coefficients[7:12] # beta hats (house, outfav)
  Houtfav.cov <- h.fi.sur$coefCov[7:12, 7:12] # variance-covariance (house, outfav)
  
  z <- seq(min(mydata.diff$d.housepol), max(mydata.diff$d.housepol), length.out = 1000)
  dy.dx <- Houtfav.hats["outreg_d.gridlock"] + Houtfav.hats["outreg_d.gridlock:d.housepol"]*z
  se.dy.dx <- sqrt(Houtfav.cov["outreg_d.gridlock", "outreg_d.gridlock"] + 
                     z^2*Houtfav.cov["outreg_d.gridlock:d.housepol", "outreg_d.gridlock:d.housepol"] + 
                     2*z*Houtfav.cov["outreg_d.gridlock", "outreg_d.gridlock:d.housepol"])
  upr <- dy.dx + 1.645*se.dy.dx
  lwr <- dy.dx - 1.645*se.dy.dx
  
  pdf(height = 6, width = 8, "Figures/ME-Gridlock-OutParty-House.pdf")
  par(family="serif",bty="l",mar=c(5,5.5,2,2), cex = 1.5)
  plot(x=z, y=dy.dx,type="n",xlim=c(min(z),max(z)),
       ylim=c(1.2*min(lwr), 1.2*max(upr)),
       xlab = TeX("$\\Delta$ Elite Polarization"),
       ylab = TeX("ME of $\\Delta$Gridlock on $\\Delta$Out-Party Favorability"))
  lines(z, dy.dx, lwd = 3)
  lines(z, lwr)
  lines(z, upr)
  abline(h=0,lty=2)
  dev.off()
  
  # Senate, In-Party
  Sinfav.hats <- s.fi.sur$coefficients[1:6] # beta hats (house, infav)
  Sinfav.cov <- s.fi.sur$coefCov[1:6, 1:6] # variance-covariance (house, infav)
  
  z <- seq(min(mydata.diff$d.senpol), max(mydata.diff$d.senpol), length.out = 1000)
  dy.dx <- Sinfav.hats["inreg_d.gridlock"] + Sinfav.hats["inreg_d.gridlock:d.senpol"]*z
  se.dy.dx <- sqrt(Sinfav.cov["inreg_d.gridlock", "inreg_d.gridlock"] + 
                     z^2*Sinfav.cov["inreg_d.gridlock:d.senpol", "inreg_d.gridlock:d.senpol"] + 
                     2*z*Sinfav.cov["inreg_d.gridlock", "inreg_d.gridlock:d.senpol"])
  upr <- dy.dx + 1.645*se.dy.dx
  lwr <- dy.dx - 1.645*se.dy.dx
  
  pdf(height = 6, width = 8, "Figures/ME-Gridlock-InParty-Senate.pdf")
  par(family="serif",bty="l",mar=c(5,5.5,2,2), cex = 1.5)
  plot(x=z, y=dy.dx,type="n",xlim=c(min(z),max(z)),
       ylim=c(1.2*min(lwr), 1.2*max(upr)),
       xlab = TeX("$\\Delta$ Elite Polarization"),
       ylab = TeX("ME of $\\Delta$Gridlock on $\\Delta$In-Party Favorability"))
  lines(z, dy.dx, lwd = 3)
  lines(z, lwr)
  lines(z, upr)
  abline(h=0,lty=2)
  dev.off()
  
  # Senate, Out-Party
  Soutfav.hats <- s.fi.sur$coefficients[7:12] # beta hats (house, outfav)
  Soutfav.cov <- s.fi.sur$coefCov[7:12, 7:12] # variance-covariance (house, outfav)
  
  z <- seq(min(mydata.diff$d.senpol), max(mydata.diff$d.senpol), length.out = 1000)
  dy.dx <- Soutfav.hats["outreg_d.gridlock"] + Soutfav.hats["outreg_d.gridlock:d.senpol"]*z
  se.dy.dx <- sqrt(Soutfav.cov["outreg_d.gridlock", "outreg_d.gridlock"] + 
                     z^2*Soutfav.cov["outreg_d.gridlock:d.senpol", "outreg_d.gridlock:d.senpol"] + 
                     2*z*Soutfav.cov["outreg_d.gridlock", "outreg_d.gridlock:d.senpol"])
  upr <- dy.dx + 1.645*se.dy.dx
  lwr <- dy.dx - 1.645*se.dy.dx
  
  pdf(height = 6, width = 8, "Figures/ME-Gridlock-OutParty-Senate.pdf")
  par(family="serif",bty="l",mar=c(5,5.5,2,2), cex = 1.5)
  plot(x=z, y=dy.dx,type="n",xlim=c(min(z),max(z)),
       ylim=c(1.2*min(lwr), 1.2*max(upr)),
       xlab = TeX("$\\Delta$ Elite Polarization"),
       ylab = TeX("ME of $\\Delta$Gridlock on $\\Delta$Out-Party Favorability"))
  lines(z, dy.dx, lwd = 3)
  lines(z, lwr)
  lines(z, upr)
  abline(h=0,lty=2)
  dev.off()
  