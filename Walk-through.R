
#walk through

#mac - install xcode
#follow the first set of instructions that respond to this post
#https://apple.stackexchange.com/questions/254380/why-am-i-getting-an-invalid-active-developer-path-when-attempting-to-use-git-a

#pc - install Rtools from this website
#https://cran.r-project.org/bin/windows/Rtools/

#install our packages (possibly the hardest part)

install.packages("readxl", dependencies = T)
install.packages("dplyr", dependencies = T)
install.packages("readr", dependencies = T)
install.packages("tidycensus", dependencies = T)
install.packages("tidyr", dependencies = T)

#trouble loading some packages? (no-zero exist status means it failed to load)

# ask for help before moving forward. 

#Once installed, read them into the work space

library(tidycensus)
library(tidyr)
library(dplyr)
library(readxl)
library(readr)

#note your working directory (folder that you are saving things too)
getwd()

#create a data folder in your working directory
dir.create("./data")

#NATA Respiratory Hazard, NATA Cancer Risk

#For census data, let’s use %minority, income, and % poverty.

#NATA download cancer and respiratory
download.file("https://www.epa.gov/sites/production/files/2018-08/nata2014v2_national_cancerrisk_by_tract_srcgrp.xlsx",
              destfile = "data/NATA14_cancer_bysrcgrp.xlsx", mode="wb")
download.file("https://www.epa.gov/sites/production/files/2018-08/nata2014v2_national_resphi_by_tract_srcgrp.xlsx",
              destfile = "data/NATA14_resp_bysrcgrp.xlsx", mode="wb")

#read into R
nata_c  <- read_excel("data/NATA14_cancer_bysrcgrp.xlsx")
nata_r  <- read_excel("data/NATA14_resp_bysrcgrp.xlsx")

#select the State
mystate <- "CT"

#cut to state and total risk variable
nata_c_st <- nata_c %>% filter(State == mystate) %>% select(Tract, `Total Cancer Risk (per million)`)
nata_r_st <- nata_r %>% filter(State == mystate) %>% select(Tract, `Total Respiratory (hazard quotient)`)

#join
nata_st <- left_join(nata_c_st, nata_r_st)

#get 2014 census data

#set API key
census_api_key("3b7f443116b03bdd7ce2f1ff3f2b117cfff19e69")

acsYear <- get_acs(geography = "tract", 
                   state = mystate,
                   variables = c(medincome = "B19013_001",
                                 population = "B01001_001",
                                 white_pop = "B01001A_001",
                                 poverty_status_total = "B17001_001",
                                 poverty_below_total = "B17001_002"))
# change data to long format
acsYear1 <- acsYear %>% select(-moe) %>% spread(variable, estimate) %>% distinct()

#get percentage vars                 
acsYear2 <- acsYear1 %>% mutate(Minority_Per = (population - white_pop)/population,
                                Poverty_Per = poverty_below_total/poverty_status_total)

acsYear3 <- acsYear2 %>% select(GEOID, medincome, population, Poverty_Per, Minority_Per)

#merge with NATA
mydata <- left_join(acsYear3, nata_st, by = c("GEOID" = "Tract"))


#get health data for smoking and premature death by county
  year = 2014
  url <- paste0("https://www.countyhealthrankings.org/sites/default/files/analytic_data", year, ".csv")
  chr <- read_csv(url, skip=1)
  chr$GEOID <- chr$fipscode
  smoke <- chr %>% select(fipscode, county, year, v009_rawvalue, v001_numerator, v001_denominator)
  smoke$adultSmokeRate <- smoke$v009_rawvalue
  smoke$prematureDeathRate <- smoke$v001_numerator/smoke$v001_denominator
  smoke$GEOID <- smoke$fipscode
  smoke <- smoke %>% select(GEOID, county, adultSmokeRate, prematureDeathRate,year)

#Merge with NATA and CENSUS by making a county fips code
mydata$county_fip <- substr(mydata$GEOID, 1,5)
mydata <- left_join(mydata, smoke, by = c("county_fip" = "GEOID"))

#save to your computer for qgis step
write.csv(mydata, paste("Data_", mystate, Sys.Date(), ".csv"))



