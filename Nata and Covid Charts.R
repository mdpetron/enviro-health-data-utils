# NEI Explorer

#NEI 2017 
# https://www.epa.gov/air-emissions-inventories/2017-national-emissions-inventory-nei-data

#get point source file from
#ftp://newftp.epa.gov/air/nei/2017/doc/supporting_data/point/

library(readr)
emis_sum_fac_9861 <- read_csv("C:/Users/Mike Petroni/Downloads/2017nei_August2019_facility/emis_sum_fac_9861.csv")

#How many pollutants? 
library(dplyr)
n_distinct(emis_sum_fac_9861$`pollutant code`)

#how many are measured in tons?
crit <- emis_sum_fac_9861 %>% filter(`emissions uom` == "TON")

sum(crit$`total emissions`)

#this number needs some more investigation into double counting 

#how about HAPS

haps <- emis_sum_fac_9861 %>% filter(`pollutant type(s)` == "HAP")
sum(haps$`total emissions`)


 emish <- read_csv("~/Nata_aermod_2014_county_groups_regions_6_10.txt")
emish2 <- read_csv("~/Nata_aermod_2014_county_groups_regions_1_5.txt")

emish <- bind_rows(emish, emish2)

ace <- emish %>% filter(NEI_Poll_Code == 75070)

ace_source <- ace %>% group_by(Source_Category) %>% summarise(tons_by_source_US = sum(`NEI_emis_(tons)`)) %>% ungroup()

ace_anthro <- ace_source %>% filter(Source_Category %in% c("OR",
                                                           "NR",
                                                           "NP",
                                                           "PT",
                                                           "FIRES-Ag_CMAQModeled",
                                                           "FIRES-Prescribed_CMAQModeled"))  

fig <- plot_ly(ace_anthro, labels = ~Source_Category, values = ~tons_by_source_US, type = 'pie')
fig

ace_state_PT <- ace %>% group_by(State_or_Tribe) %>% filter(Source_Category == "PT") %>% summarise(tons_by_source_US = sum(`NEI_emis_(tons)`)) %>% ungroup()

fig <- plot_ly(ace_state_PT, labels = ~State_or_Tribe, values = ~tons_by_source_US, type = 'pie')
fig

nap <- emish %>% filter(NEI_Poll_Desc == "Naphthalene")

nap_source <- nap %>% group_by(Source_Category) %>% summarise(tons_by_source_US = sum(`NEI_emis_(tons)`)) %>% ungroup()

library(plotly)

fig <- plot_ly(ace_source, labels = ~Source_Category, values = ~tons_by_source_US, type = 'pie')
fig

#what county has the most pollutants 

count <- emish %>% group_by(County_or_Tribe, State_or_Tribe) %>% summarise(contaminents = n_distinct(NEI_Poll_Code))


# what about respiratory HAPs

setwd("C:/Users/Mike Petroni/Documents/GitHub/Covid19-HAPs/")
NataRespHazBySource  <- read_excel("github_data/nata2014v2_national_resphi_by_tract_srcgrp.xlsx")
NataRespHazByChem    <- read_excel("github_data/nata2014v2_national_resphi_by_tract_poll.xlsx")
nata2 <- NataRespHazBySource
nata2 <- left_join(nata2, NataRespHazByChem)
# view(nata2)

## Reformat NATA data to be by county ##
nata2$FIPS  = as.character(nata2$FIPS)
nata2$Tract = as.character(nata2$Tract)
nata2$FIPS_TRACT = stri_pad_right(nata2$FIPS, 11, 0)
nata2  <- nata2 [which(nata2$Tract == nata2$FIPS_TRACT ),]
nata2$Population <- as.numeric(as.character( gsub(",", "", nata2$Population) ))
nata2$GEOID <- nata2$FIPS


#get a long version 

chemlong <- nata2 %>% select(1,3,4,5,7,Population,89,90,46:88)

library(tidyr)
chemlong2 <- chemlong %>% group_by(State, County, FIPS, Tract, `Total Respiratory (hazard quotient)`, GEOID, FIPS_TRACT, Population) %>% gather(Chemical, Concentration, 8:50)

#remove the 0s

chemlong3 <- chemlong2 %>% filter(Concentration > 0)

countycount <- chemlong3 %>% group_by(State, County, FIPS, Tract, `Total Respiratory (hazard quotient)`, FIPS_TRACT, Population) %>% summarise(chems = n_distinct(Chemical))

nyt_deaths <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

nyt_deaths <- nyt_deaths %>% filter(date == "2020-10-20")

dat <- left_join(countycount, nyt_deaths, by = c("FIPS" = "fips"))

dat$deathrate_per10k <- dat$deaths/(dat$Population/10000)

#lets look at just south

dats <- dat %>% filter(state %in% c("Georgia", "Mississippi", "Louisiana", "Alabama"))
dat_louis <- dat %>% filter(state %in% c("Louisiana"))

plot(dat$deathrate ~ dat$`Total Respiratory (hazard quotient)`)
points(dats$deathrate ~ dats$`Total Respiratory (hazard quotient)`, col = "blue")
abline(lm(dat$deathrate_per10k ~ dat$`Total Respiratory (hazard quotient)`), col = "red")
legend("topright", c("US Counties", "GA, MS, LA, AL"), col=c("black", "blue"), pch = 1, cex=0.8)
summary(lm(dat$deathrate ~ dat$`Total Respiratory (hazard quotient)`))

summary(dat$deathrate_per10k)

plot(dats$deathrate ~ dats$`Total Respiratory (hazard quotient)`)
abline(lm(dats$deathrate ~ dats$`Total Respiratory (hazard quotient)`))

south_dat = dat %>% filter(state %in% c("Georgia", "Mississippi", "Louisiana", "Alabama"))
other_dat = dat %>% filter(!state %in% c("Georgia", "Mississippi", "Louisiana", "Alabama"))

fit <- lm(dat$deathrate_per10k ~ dat$`Total Respiratory (hazard quotient)`)
fv <- dat %>% filter(!is.na(deathrate_per10k)) %>% lm(deathrate_per10k ~ `Total Respiratory (hazard quotient)`,.) %>% fitted.values()

plot_ly() %>% 
  add_trace(data = other_dat,
        y = ~deathrate_per10k, x = ~`Total Respiratory (hazard quotient)`, text = ~paste(County,State, sep = ", "),
        marker = list(color = "#bbbbbb", opacity = .8, size = 9), name = "All Other US Counties") %>%
  add_trace(inherit = F, data = south_dat,
            y = ~deathrate_per10k, x = ~`Total Respiratory (hazard quotient)`, text = ~paste(County,State, sep = ", "),
            marker = list(color = "#00578a", opacity = .8, size = 9), name = "Counties in GA, MS, LA, and AL", color = "blue") %>%
  add_trace(data = dat %>% filter(!is.na(deathrate_per10k)), y = c(min(fv), max(fv)), 
            x = ~c(min(`Total Respiratory (hazard quotient)`), max(`Total Respiratory (hazard quotient)`)), mode = "lines",
            name = "Correlation Between Covid-19 Mortality<br>and Toxic Respiratory HAP Levels") %>% 
  add_trace(y = c(50,0), text = "NATA Risk Screening Threshold",
            x = ~c(1, 1), mode = "lines", line = list(color = "#7F0000", dash = "dash"),
            name = "NATA Acceptable Risk Screening Level") %>% 
  layout(xaxis = list(title = "Average Level of Hazardous Air Pollutants (Adusted by Respiratory Toxicity)"),
         yaxis = list(title = "Covid-19 Mortality per 10,000 Population as of October 20, 2020"), 
         legend = list(x = .56, 
                       y = .92,
                       font = list(color= "black"),
                       bgcolor = "white",
                       bordercolor = "#323232",
                       borderwidth = 2))



dat_export <- dat %>% ungroup() %>%select(State, County, `Total Respiratory (hazard quotient)`, deathrate_per10k, deaths, Population)
getwd()
write.csv(dat_export, "data/Conversation_Data_Export.csv")












