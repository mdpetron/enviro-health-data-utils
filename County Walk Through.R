
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

setwd("C:/Users/Mike Petroni/Documents/GitHub/enviro-health-data-utils")

#create a data folder in your working directory
dir.create("./data")

#NATA Respiratory Hazard, NATA Cancer Risk

#For census data, lets use %minority, income, and % poverty.

#NATA download cancer and respiratory
download.file("https://www.epa.gov/sites/production/files/2018-08/nata2014v2_national_cancerrisk_by_tract_srcgrp.xlsx",
              destfile = "data/NATA14_cancer_bysrcgrp.xlsx", mode="wb")
download.file("https://www.epa.gov/sites/production/files/2018-08/nata2014v2_national_resphi_by_tract_srcgrp.xlsx",
              destfile = "data/NATA14_resp_bysrcgrp.xlsx", mode="wb")
download.file("https://www.epa.gov/sites/production/files/2018-08/nata2014v2_national_immuhi_by_tract_srcgrp.xlsx",
              destfile = "data/NATA14_immuhi_bysrcgrp.xlsx", mode="wb")

#read into R
nata_c  <- read_excel("data/NATA14_cancer_bysrcgrp.xlsx")
nata_r  <- read_excel("data/NATA14_resp_bysrcgrp.xlsx")
nata_i  <- read_excel("data/NATA14_immuhi_bysrcgrp.xlsx")

stateabv <- "CT"

download_it <- function(stateabv) {
   
#dplyr::select the State
mystate <- stateabv

#cut to state and total risk variable
nata_c_st <- nata_c %>% filter(State == mystate, County != "Entire State",
                               County != "Entire US") %>% dplyr::select(Tract, FIPS,  `Total Cancer Risk (per million)`,
                                                                        `PT-StationaryPoint Cancer Risk (per million)`)
nata_r_st <- nata_r %>% filter(State == mystate, County != "Entire State",
                               County != "Entire US") %>% dplyr::select(Tract, FIPS,  `Total Respiratory (hazard quotient)`,
                                                                        `PT-StationaryPoint Respiratory (hazard quotient)`)
nata_i_st <- nata_i %>% filter(State == mystate, County != "Entire State",
                               County != "Entire US") %>% dplyr::select(Tract, FIPS,  `Total Immunological (hazard quotient)`,
                                                                        `PT-StationaryPoint Immunological (hazard quotient)`)

#join
nata_st <- left_join(nata_c_st, nata_r_st)
nata_st <- left_join(nata_st, nata_i_st)

#get 2014 census data

#set API key
census_api_key("3b7f443116b03bdd7ce2f1ff3f2b117cfff19e69")

acsYear <- get_acs(geography = "tract", 
                   state = mystate,
                   year = 2014,
                   variables = c(medincome = "B19013_001",
                                 population = "B01001_001",
                                 white_pop = "B01001A_001",
                                 black_pop = "B01001B_001",
                                 hispanic_pop = "B01001I_001",
                                 Native_American = "B02001_004",
                                 Asian_alone = "B02001_005",
                                 other_alone = "B02001_007",
                                 two_or_more = "B02001_008",
                                 rentasPercageofIncome = "B25071_001",
                                 poverty_status_total = "B17001_001",
                                 poverty_below_total = "B17001_002",
                                 labor_force = "B23025_001",
                                 Not_in_labor_force = "B23025_007", 
                                 pov_60_75 = "B17020_015",
                                 pov_75_85 = "B17020_016",
                                 pov_85_plus = "B17020_017",
                                 Male_age_60_63 = "B01001_018",
                                 Male_age_63_65 = "B01001_019",
                                 Male_age_65_66 = "B01001_020",
                                 Male_age_67_69 = "B01001_021",
                                 Male_age_70_74 = "B01001_022",
                                 Male_age_75_79 = "B01001_023",
                                 Male_age_80_84 = "B01001_024",
                                 Male_age_85_plus = "B01001_025",
                                 F_age_60_63 = "B01001_042",
                                 F_age_63_65 = "B01001_043",
                                 F_age_65_66 = "B01001_044",
                                 F_age_67_69 = "B01001_045",
                                 F_age_70_74 = "B01001_046",
                                 F_age_75_79 = "B01001_047",
                                 F_age_80_84 = "B01001_048",
                                 F_age_85_plus = "B01001_049"))
# change data to long format
acsYear1 <- acsYear %>% dplyr::select(-moe) %>% spread(variable, estimate) %>% distinct()

#get percentage vars                 
acsYear2 <- acsYear1 %>% mutate(Minority_Per = (population - white_pop)/population,
                                Poverty_Per = poverty_below_total/poverty_status_total,
                                White_per = white_pop/population,
                                Black_Per = black_pop/population,
                                Hispanic_Per = hispanic_pop/population,
                                Native_American_Per = Native_American/population,
                                not_employed_per = Not_in_labor_force/labor_force,
                                Over60_Per = (Male_age_60_63 + Male_age_63_65 + Male_age_65_66 + Male_age_67_69 +Male_age_70_74 +
                                                Male_age_75_79 + Male_age_80_84 + Male_age_85_plus +
                                                F_age_60_63 + F_age_63_65 + 
                                                F_age_65_66 + F_age_67_69 + F_age_70_74 +
                                                F_age_75_79 + F_age_80_84 + F_age_85_plus)/population,
                                Over60_pov_Per = (pov_60_75 + pov_75_85 + pov_85_plus)/(Male_age_60_63 + Male_age_63_65 + Male_age_65_66 + Male_age_67_69 +Male_age_70_74 +
                                                                                          Male_age_75_79 + Male_age_80_84 + Male_age_85_plus +
                                                                                          F_age_60_63 + F_age_63_65 +
                                                                                        F_age_65_66 + F_age_67_69 + F_age_70_74 +
                                                                                          F_age_75_79 + F_age_80_84 + F_age_85_plus)
                                )

acsYear3 <- acsYear2 %>% dplyr::select(GEOID, medincome, population, Poverty_Per, Minority_Per,
                                       White_per, Black_Per, Hispanic_Per, Native_American_Per,
                                       not_employed_per, Over60_Per, Over60_pov_Per)

#merge with NATA
mydata <- left_join(acsYear3, nata_st, by = c("GEOID" = "Tract"))


#save to your computer for qgis step
write.csv(mydata, paste0("data/Data_Tract_", mystate, Sys.Date(), ".csv"))

}


lapply(state.abb, function (x) download_it(x))
