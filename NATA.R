# Code for downloading, reading, and manipulating NATA data

# National Air Toxics Assessment 

# Import and pre-process 2014 NATA data.
# Source: https://www.epa.gov/national-air-toxics-assessment/2014-nata-assessment-results
# Technical documentation: https://www.epa.gov/national-air-toxics-assessment/2014-nata-technical-support-document
# Variable explanations: https://www.epa.gov/sites/production/files/2018-08/documents/explanation_of_data_elements_in_2014_nata_files.pdf

#make a data folder in your repo
setwd("/Users/Mike/Documents/GitHub/enviro-health-data-utils")
dir.create("./data")

#download the excel files from the EPA website
#by pollution source groups
download.file("https://www.epa.gov/sites/production/files/2018-08/nata2014v2_national_cancerrisk_by_tract_srcgrp.xlsx",
              destfile = "data/NATA14_cancer_bysrcgrp.xlsx", mode="wb")
#by chemical
download.file("https://www.epa.gov/sites/production/files/2018-08/nata2014v2_national_cancerrisk_by_tract_poll.xlsx",
              destfile = "data/NATA14_cancer_poll.xlsx", mode="wb")

#read them into your R environment using readxl
install.packages("readxl")
library(readxl)

#load one into your workplace
nata  <- read_excel("data/NATA14_cancer_bysrcgrp.xlsx")

# make sure that our FIPs codes are in the right format
nata$GEOID <- nata$Tract

#What sources are most affecting Toxics Cancer Risk in CT? 
#pull out the state row
library(dplyr)
library(tidyr)
library(plotly)
ct_summary <- nata %>% filter(GEOID == "09000000000")
#reformat
names(ct_summary)
ct_summary <- ct_summary %>% select(8:45) %>% gather("Source", "Cancer Risk")

#very simple visualization
plot_ly(ct_summary, x = ~`Cancer Risk`, y = ~Source) %>% 
  layout(margin = list(
             l = 400
             # r = 50,
             # b = 100,
             # t = 100,
             # pad = 4
           ))


#lets make a map 

#get the shapefile 
install.packages("tigris")
library(tigris)

#pick a state
ct <- tracts(state = "ct")

#join nata with our state
library(sp)
ct <- sp::merge(ct, nata, by = "GEOID", all.x = T)

#remove tracts without population
ct <- subset(ct, ct$Population > 0)

#make a map
install.packages("tmap")
library(tmap)

nata_cancer_risk_map <- tm_shape(ct) +
  tm_fill(col = "Total Cancer Risk (per million)",
          title = "Total Cancer Risk (per million)",
          alpha = 1) +
  tm_borders() +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("2014 National Air Toxics Assessment Cancer Risk - Connecticut",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .13,
            legend.title.size = 1)

nata_cancer_risk_map


