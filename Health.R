#Health data

# County Health Rankings

# Import and pre-process additional county level health data
# Source: https://www.countyhealthrankings.org/sites/default/files/media/document/Trends%20documentation%202020.pdf
county_trends <- read_csv("https://www.countyhealthrankings.org/sites/default/files/media/document/CHR_trends_csv_2020.csv")
county_trends <- county_trends %>% select(statecode, countycode, county, measurename, rawvalue)
county_trends <- county_trends %>% mutate(GEOID_state =  ifelse(nchar(statecode) == 1, paste0("0", statecode), statecode),
                                          GEOID_county = ifelse(nchar(countycode) == 1, paste0("00", countycode),
                                                                ifelse(nchar(countycode) == 2, paste0("0", countycode),countycode)),
                                          GEOID = paste0(GEOID_state, GEOID_county))
county_trends <- county_trends %>% filter(countycode != 0)
county_trends <- county_trends %>% group_by(GEOID, measurename) %>%
  summarise(rawvalue_mean_all_years = mean(rawvalue, na.rm = T)) 
county_trends <- county_trends %>% group_by(GEOID) %>% spread(measurename,
                                                              rawvalue_mean_all_years,
                                                              sep = "_mean_all_years_") %>% ungroup()

# Function to grab and pre-process smoking data for a given year.
# Source: https://www.countyhealthrankings.org/sites/default/files/DataDictionary_2014.pdf
smokeGrab <- function(year) {
  
  url <- paste0("https://www.countyhealthrankings.org/sites/default/files/analytic_data", year, ".csv")
  chr <- read_csv(url, skip=1)
  chr$GEOID <- chr$fipscode
  smoke <- chr %>% select(fipscode, county, year, v009_rawvalue, v001_rawvalue)
  smoke$adultSmokeRate <- smoke$v009_rawvalue
  smoke$prematureDeathRate <- smoke$v001_rawvalue
  smoke$GEOID <- smoke$fipscode
  smoke <- smoke %>% select(GEOID, county, adultSmokeRate, year, prematureDeathRate)
  return(smoke)
}

# Pull smoking data from 2010-2018.
smokelist <- lapply(2010:2018, function (x)  smokeGrab(x))
smoking_data <- smokelist[[1]]
for (i in 2:length(smokelist)) smoking_data <- bind_rows(smoking_data, smokelist[[i]])
smoking_data <- smoking_data %>% group_by(GEOID) %>% summarise( meanSmokeRate = mean(adultSmokeRate, na.rm = T) )


#CDC 500 cities 
download.file("https://chronicdata.cdc.gov/api/views/6vp6-wxuq/rows.csv?accessType=DOWNLOAD", 
              destfile = "data/cdc500_1617.csv")
cdc1314 <- read_csv("data/cdc500_1314.csv")

