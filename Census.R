# CENSUS

# welcome to tidycensus
# basic tutorial - https://walker-data.com/tidycensus/articles/basic-usage.html
# install.packages("tidycensus")
library(tidycensus)
library(tidyr)
library(dplyr)

# get a census API Key - https://api.census.gov/data/key_signup.html
census_api_key("3b7f443116b03bdd7ce2f1ff3f2b117cfff19e69")

#investigate the acs variables 
# v17 <- load_variables(2009, "acs5", cache = TRUE)

#here is a handy function with some data picked out
censusGrab <- function(Year, state) {
  #this will grab all of our ACS data by year
  acsYear <- get_acs(geography = "tract", 
                     state = state,
                     variables = c(medincome = "B19013_001",
                                   population = "B01001_001",
                                   black_pop = "B01001B_001",
                                   hispanic_pop = "B01001I_001",
                                   poverty_status_total = "B17001_001",
                                   poverty_below_total = "B17001_002",
                                   median_house_value = "B25077_001",
                                   Total_Education = "B15003_001",
                                   overHS1 = "B15003_017",
                                   overHS2 = "B15003_018",
                                   overHS3 = "B15003_019",
                                   overHS4 = "B15003_020",
                                   overHS5 = "B15003_021",
                                   overHS6 = "B15003_022",
                                   overHS7 = "B15003_023",
                                   overHS8 = "B15003_024",
                                   overHS9 = "B15003_025",
                                   housing_units = "B25001_001",
                                   owner_occupied = "B25002_002",
                                   Male_age_65_66 = "B01001_020",
                                   Male_age_67_69 = "B01001_021",
                                   Male_age_70_74 = "B01001_022",
                                   Male_age_75_79 = "B01001_023",
                                   Male_age_80_84 = "B01001_024",
                                   Male_age_85_plus = "B01001_025",
                                   F_age_65_66 = "B01001_044",
                                   F_age_67_69 = "B01001_045",
                                   F_age_70_74 = "B01001_046",
                                   F_age_75_79 = "B01001_047",
                                   F_age_80_84 = "B01001_048",
                                   F_age_85_plus = "B01001_049"),
                     year = Year)
  
  # change data to long format
  acsYear1 <- acsYear %>% select(-moe) %>% spread(variable, estimate) %>% distinct()
  
  # add year var
  acsYear1$year <- rep(Year, nrow(acsYear1))
  
  # get our percentage variables 
  acsYear2 <- acsYear1 %>% mutate(Black_Per = black_pop/population,
                                  Hispanic_Per = hispanic_pop/population,
                                  Poverty_Per = poverty_below_total/poverty_status_total,
                                  Under_HS_Per = 1-((overHS1 + overHS2 + overHS3 + overHS4 + overHS5 + overHS6 +
                                                       overHS7 + overHS8 +overHS9)/Total_Education),
                                  Owner_Occ_Per = owner_occupied/housing_units,
                                  Over64_M_Per = (Male_age_65_66 + Male_age_67_69 +Male_age_70_74 +
                                                    Male_age_75_79 + Male_age_80_84 + Male_age_85_plus)/population,
                                  Over64_F_Per = (F_age_65_66 + F_age_67_69 + F_age_70_74 +
                                                    F_age_75_79 + F_age_80_84 + F_age_85_plus)/population)
  # cut it to what we want
  acsYear3 <- acsYear2 %>% select(GEOID, NAME, year, median_house_value, medincome, Black_Per,
                                  Hispanic_Per, population, Poverty_Per, Under_HS_Per,
                                  Owner_Occ_Per, Over64_F_Per, Over64_M_Per)
  
  return(acsYear3)
}

ct14 <- censusGrab(2014, "CT")

# combine with our shapefile from NATA.R

#join nata with our state
library(sp)
ct14 <- sp::merge(ct, ct14, by = "GEOID", all.x = T)
names(ct14)

#make a dataframe
ct14_df <- ct14@data
#remove NA
ct14_df <- ct14_df %>% filter(complete.cases(ct14_df) == T)

#poverty vs NATA
library(plotly)
plot_ly(ct14_df, x = ~Poverty_Per, y = ~`Total Cancer Risk (per million)`)

#correlation 
cor(ct14_df$Poverty_Per, ct14_df$`Total Cancer Risk (per million)`)
cor.test(ct14_df$Poverty_Per, ct14_df$`Total Cancer Risk (per million)`)

#we have a positive and significant correlation between poverty and cancer risk in CT. 

# other analysis ideas 
# https://link.springer.com/content/pdf/10.1007/s40615-014-0018-2.pdf

