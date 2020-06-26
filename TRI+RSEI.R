# Code for downloading, reading, and manipulating TRI data

# Toxics Release Inventory 

# Source: https://www.epa.gov/toxics-release-inventory-tri-program
# Data: https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2018?  
# Variable explanations: https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-guide
# Interactive data tools:
# https://www.epa.gov/toxics-release-inventory-tri-program/tri-p2-industry-profile
# https://edap.epa.gov/public/extensions/EasyRSEI/EasyRSEI.html

# a function for loading basic data files by year and state 
# install.packages("readr")
library(readr)
TRIload <- function(capitalized_state_abv, year){
  url <- paste0("https://enviro.epa.gov/enviro/efservice/MV_TRI_BASIC_DOWNLOAD/st/=/",
                capitalized_state_abv, "/year/=/", year, "/fname/TRI_2018_",
                capitalized_state_abv, ".csv/CSV")
  df <- read_csv(url)
  return(df)
}

tri_ct18 <- TRIload("CT", "2018")

#for multiple years
tri_ct90to18 <- lapply(1990:2018, function (x)  TRIload("CT", x))
tri_ct90to18_df <- tri_ct90to18[[1]]
for (i in 2:length(tri_ct90to18)) tri_ct90to18_df <- rbind(tri_ct90to18_df, tri_ct90to18[[i]])

#industry time series 
#wrangle so that you have industry by year
names(tri_ct90to18_df)
topinds <- tri_ct90to18_df %>% group_by(`19. INDUSTRY SECTOR`) %>%
  summarise(Releases = sum(`100. TOTAL RELEASES`),
            Waste_Managed = sum(`112. PRODUCTION WSTE (8.1-8.7)`)) %>%
  mutate(rank = dense_rank(-Releases)) %>% filter(rank < 11)
ind_ts <- tri_ct90to18_df %>% group_by(`19. INDUSTRY SECTOR`, `1. YEAR`) %>%
  summarise(Releases = sum(`100. TOTAL RELEASES`),
            Waste_Managed = sum(`112. PRODUCTION WSTE (8.1-8.7)`)) %>%
  filter(`19. INDUSTRY SECTOR` %in% topinds$`19. INDUSTRY SECTOR`,
         `1. YEAR` %in% c(1991:2018)) %>% ungroup()

#simple time series plot of top 10 industries 
library(plotly)
plot_ly(ind_ts, x =~`1. YEAR`, y =~Releases, color = ~`19. INDUSTRY SECTOR`, mode = "lines")
plot_ly(ind_ts, x =~`1. YEAR`, y =~Waste_Managed, color = ~`19. INDUSTRY SECTOR`, mode = "lines")

#add facilities to our NATA map 
topfacs <- tri_ct90to18_df %>% filter(`1. YEAR` == 2014) %>% group_by(`2. TRIFD`, `4. FACILITY NAME`, `12. LATITUDE`, `13. LONGITUDE`) %>%
  summarise(Releases = sum(`100. TOTAL RELEASES`),
            Waste_Managed = sum(`112. PRODUCTION WSTE (8.1-8.7)`))

library(leaflet)
pal = colorNumeric("RdYlBu", domain = topfacs$Releases, reverse = T)
pal2 = colorNumeric("YlOrRd", domain = ct$`PT-StationaryPoint Cancer Risk (per million)`)
leaflet(data = topfacs) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = ct, stroke = F, color = ~pal2(ct$`PT-StationaryPoint Cancer Risk (per million)`),
              smoothFactor = 0.2, fillOpacity = .7) %>% 
  addCircles(lng = ~`13. LONGITUDE`, lat = ~`12. LATITUDE`,  col = ~pal(Releases), opacity = 0.9,
             popup = ~`4. FACILITY NAME`) %>% 
  addLegend(pal = pal, values = ~Releases) %>% 
  setView(lng = -72.67, 41.76, zoom = 10) %>% 
  addMiniMap()

#RSEI - see easy RSEI

#are decline slopes dependent on EJ factors?
