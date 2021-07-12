
#here we will make a tract level estimation of the
# increase of RSEI hazard in St. James 

#get the 2018 RSEI data from ABT:
# https://docs.google.com/document/d/1lB6pktVtfxJgtLPgddBWSFiZfesB10HPikFscXaY1Xc/edit?usp=sharing

#unzip file
setwd("C:/Users/Mike Petroni/Documents/GitHub/enviro-health-data-utils/data/")
library(R.utils) 
library(readr)
gunzip("censusmicrozipcode2018_2018_core01.csv.gz")
gunzip("censusmicrotracts2018_1993_aggregated.csv.gz")
rsei18tr <- read_csv("censusmicrotracts2018_2018_core_aggregated.csv",
                     col_names = F)
rsei93tr <- read_csv("censusmicrotracts2018_1993_aggregated.csv",
                     col_names = F)
rsei18zip <- read_csv("censusmicrozipcode2018_2018_core_aggregated.csv",
                     col_names = F)
rsei18zip <- read_csv("censusmicrozipcode2018_2018_core_aggregated.csv",
                      col_names = F)
rsei18zip_all <- read_csv("censusmicrozipcode2018_2018_core01.csv",
                      col_names = F)
chem_dat <- read_csv("chemical_data_rsei_v238.csv")

#see what's in there
rseinames <- c("GEOID",
  "NUMFACS",
  "NUMRELEASES",
  'NUMCHEMS',
  "TOXCONC",
  'SCORE',
  "POP",
  "CSCORE",
  "NCSCORE")

names(rsei18tr) <- rseinames 
names(rsei93tr) <- rseinames 
names(rsei18zip)  <- rseinames 
names(rsei18zip_all)  <- c("GEOID10",
                           "ReleaseNumber",
                           "ChemicalNumber",
                           'FacilityNumber',
                           "Media",
                           'Conc',
                           "ToxConc",
                           "Score",
                           "CSCORE",
                           "NCSCORE",
                           "POP")

#bring in tract shapfiles 
library(tigris)

LAtr <- tracts(state = "LA")
LAzip <- zctas(state = "LA")
LAbg <- block_groups(state = "LA")

LAzip <- readOGR("./tl_2018_us_zcta510", "tl_2018_us_zcta510")


#bring in formosa modeling 
fpcc <- readRDS("C:/Users/Mike Petroni/Documents/RSEI Propublica/FPCC_Correction_102119.rds")
fpcc.raw <- fpcc
#now lets aggregate the TOXCONC for FPCC at each receptor 

#lets do a big over spatial join 
library(rgeos)
library(geosphere)
library(gdistance)

projection(LAzip)
coordinates(fpcc) <- c("CLONG", "CLAT")
CRS.new <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(fpcc) <- CRS.new 

#we may have a problem with AK and HI

#here is the spatial intersection
fpcc <- over(fpcc, LAzip) 
fpcc.df <- as.data.frame(fpcc)
fpcc.df <- dplyr::bind_cols(fpcc.raw, fpcc.df)

#now we calculate the tract contribution by averaging the values per zip
#this is a bit of a shortcut but for these purposes, its just fine 
fpcc_zip_chem <- fpcc.df %>% group_by(GEOID10, Chemical) %>%
  summarise(TOXCONC = mean(ToxConc, na.rm = T)) %>% ungroup()

#whats the LA average for toxconc? 
library(plotly)

box <- plot_ly(data =rsei18zip %>% filter(grepl("^70", GEOID10)), y = ~TOXCONC, type = "box")
      #median is 4.5k ish

rsei19zip_la <- rsei18zip %>% filter(grepl("^70", GEOID10))

rsei19zip_70086 <- rsei18zip %>% filter(grepl("^70086", GEOID10))


#ok lets make the map 

fpcc_zip <- fpcc_zip_chem %>% group_by(GEOID10) %>%
  summarise(TocConc_FPCC = sum(TOXCONC, na.rm = T),
            FPCC_chems = n_distinct(Chemical))

fpcc_rsei1 <- left_join(rsei19zip_la, fpcc_zip)

fpcc_rsei <- fpcc_rsei1 %>% filter(!is.na(TocConc_FPCC))
mapit2 <- merge(LAzip, rsei18zip, by = "GEOID10")
mapit <- merge(LAzip, fpcc_rsei, by = "GEOID10")

mapit1 <- mapit[!is.na(mapit$TocConc_FPCC), ]

mapit2 <- mapit2[grepl("^70", mapit2$GEOID10) | grepl("^71", mapit2$GEOID10), ]

#whats the USA average for toxconc by zip? 

#what the zipcode that get ths highest contribution from formosa?
fpcc_zip <- fpcc_zip_chem %>% group_by(GEOID10) %>%
  summarise(TOXCONC_formosa = sum(TOXCONC, na.rm = T)) %>% ungroup() 

#what percetage increase is it? 

fpcc_zip_rsei <- left_join(fpcc_zip, rsei18zip)

#what about chemical by chemical additions 

fpcc_zip_chem_70086 <- fpcc_zip_chem %>% filter(GEOID10 == 70086)

fpcc_zip_eto_70086 <- fpcc_zip_chem_70086 %>% filter(Chemical == "Ethylene oxide")

fpcc_zip_other_70086 <- fpcc_zip_chem_70086 %>%
  filter(Chemical != "Ethylene oxide") %>% summarise(TOXCONC = sum(TOXCONC, na.rm =T))


#lets make the bar chart 

ynames = c("USA Zipcode Average",
           "Lousiana Zipcode Average",
           "Zipcode 70086 in St. James Parish")


Values1 = c(mean(rsei18zip$TOXCONC),
            mean(rsei19zip_la$TOXCONC, na.rm =T),
            rsei19zip_70086$TOXCONC)

Values2 = c(0,0, fpcc_zip_eto_70086$TOXCONC)

Values3 = c(0,0, fpcc_zip_other_70086$TOXCONC)

bardf <- data.frame(ynames, Values1, Values2)

#plot 

xform <- list(title = "",
              categoryorder = "array",
              categoryarray = c("USA Zipcode Average",
                                "Lousiana Zipcode Average",
                                "Zipcode 70086 in St. James Parish"))

bar <- plot_ly(data =bardf, x = ~ynames, y = ~Values1, type = "bar",
               name = "2018 Baseline",
               text = ~round(Values1, 0), textposition = 'top',
               marker = list(color = 'rgb(158,202,225)'
                             ))
bar %>%   add_trace(y = ~Values3, name = "Formosa Other Chemical Emissions",
                    text = ~round(Values3, 0), textposition = 'top',
                    marker = list(color = 'F6C700'
                    )) %>%
  add_trace(y = ~Values2, name = "Formosa Ethylene Oxide Emissions",
                  text = ~round(Values2, 0), textposition = 'top',
                  marker = list(color = 'rgb(165, 42, 42)'
                                )) %>%
  layout(yaxis = list(title = 'Toxic Concentration of Industrial Chemicals'),
         xaxis = xform,
         barmode = 'stack')

#can we plot the county and zipcode? 
  
#what about concentrations of ETO? 
tri_zip_eto_70086 <- rsei18zip_all %>% filter(ChemicalNumber == 293, GEOID10 == 70086)
sum(tri_zip_eto_70086$Conc)
fpcc_zip_chem <- fpcc.df %>% group_by(GEOID10, Chemical) %>%
  summarise(TOXCONC = mean(ToxConc, na.rm = T),
            Conc = mean(Conc, na.rm =T)) %>% ungroup()
fpcc_zip_chem_70086 <- fpcc_zip_chem %>%
  filter(GEOID10 == 70086, Chemical == "Ethylene oxide")
est_zip_eto_avg <- sum(tri_zip_eto_70086$Conc) + fpcc_zip_chem_70086$Conc
eto <- chem_dat %>% filter(ChemicalNumber == 293)
eto$RfCInhale


#make a chart 
ynames = c("Zipcode 70086 in St. James Parish")
Values1 = c(sum(tri_zip_eto_70086$Conc))
Values2 = c(fpcc_zip_chem_70086$Conc)
bardf2 <- data.frame(ynames, Values1, Values2)

#chart
hline <- function(y = eto$RfCInhale, color = '#228B22') {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}

hline2 <- function(y = 1, color = 'rgb(165, 42, 42)') {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}

xform <- list(title = "",
              categoryorder = "array",
              categoryarray = c("Zipcode 70086 in St. James Parish"))
bar <- plot_ly(data =bardf2, x = ~ynames, y = ~Values1, type = "bar",
               name = "Estimated Ethylene Oxide Concentration from TRI facilities 2018",
               text = ~round(Values1, 6), textposition = 'auto',
               marker = list(color = 'rgb(158,202,225)'
               ))
bar %>% add_trace(y = ~Values2, name = "Estimated Ethylene Oxide Concentration Addition from Formosa",
                    text = ~round(Values2, 6), textposition = 'auto',
                    marker = list(color = 'F6C700'
                    )) %>%
  layout(yaxis = list(title = 'Estimated Average Zipcode Air Concentration ??g/m3'),
         xaxis = xform,
         shapes = list(hline(0.003), hline2(1)),
         barmode = 'stack')

#what the max? 

fpcc_eto_max <- fpcc.df %>% group_by(GEOID10, Chemical) %>%
  summarise(TOXCONC = mean(ToxConc, na.rm = T),
            Conc = max(Conc, na.rm =T)) %>% ungroup() %>%
  filter(GEOID10 == 70086, Chemical == "Ethylene oxide")

#map of zip codes changes in concentrations 

library(broom)
plot(mapit1)
spdf_fortified <- tidy(mapit1, region = "GEOID10")

library(ggplot2)
ggplot() +
  geom_polygon(data = spdf_fortified$,
               aes( x = long, y = lat, group = group, fill = nb_equip), fill="white", color="grey") 

library(leaflet)
mapit1$toxchange <- mapit1$TocConc_FPCC+mapit1$TOXCONC

pal <- colorQuantile(
  n = 8,
  palette = "YlOrRd",
  domain = mapit1$toxchange
)
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(gdp_md_est)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~gdp_md_est,
            title = "Est. GDP (2010)",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1
  )

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = mapit2$TOXCONC
)

pal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = mapit1$toxchange
)

pal3 <- colorNumeric(
  palette = "YlOrRd",
  domain = mapit1$TocConc_FPCC
)

leaflet(mapit2) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = ~paste(round(TOXCONC,2)),
              fillColor = ~pal(TOXCONC),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = mapit2$TOXCONC,
            title = "TOXCONC",
            opacity = 1)

leaflet(mapit1) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = ~paste(round(toxchange,2)),
              fillColor = ~pal2(toxchange),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal2, values = mapit1$toxchange,
            title = "TOXCONC with FPCC",
            opacity = 1)
  
leaflet(mapit1) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = ~paste(round(TocConc_FPCC,2)),
              fillColor = ~pal3(TocConc_FPCC),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal3, values = mapit1$TocConc_FPCC,
            title = "TOXCONC added by FPCC",
            opacity = 1)



library(viridis)
p <- ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = nb_equip,
                                          x = long, y = lat,
                                          group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", breaks=c(1,5,10,20,50,100),
                     name="Number of restaurant",
                     guide = guide_legend( keyheight = unit(3, units = "mm"),
                                           keywidth=unit(12, units = "mm"),
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow=1) ) +
  labs(
    title = "South of France Restaurant concentration",
    subtitle = "Number of restaurant per city district",
    caption = "Data: INSEE | Creation: Yan Holtz | r-graph-gallery.com"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22,
                              hjust=0.01,
                              color = "#4e4d47",
                              margin = margin(b = -0.1,
                                              t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17,
                                 hjust=0.01,
                                 color = "#4e4d47",
                                 margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12,
                                 color = "#4e4d47",
                                 margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  ) +
  coord_map()
p


#cancer alley RSEI
rsei18tr <- read_csv("censusmicrotracts2018_2018_core_aggregated.csv",
                     col_names = F)
LAtr <- tracts(state = "LA")
mapit <- merge(LAtr, rsei18tr, by = "GEOID", all.x = T)

#get the cancer alley shapfile 
library(rgdal)
alley <- readOGR("./cancer_alley", "cancer_alley_msriv_10mi")

alley <- spTransform(alley, CRS("+proj=longlat +datum=WGS84"))

library(leaflet)
pal <- colorNumeric(
  palette = "Reds",
  domain = mapit$TOXCONC
)

leaflet(mapit) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = ~paste(round(TOXCONC,2)),
              fillColor = ~pal(TOXCONC),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = alley, color = "#000000", weight = 3, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0,
              popup = ~paste("cancer alley"),
              #fillColor = "#000000",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = mapit$TOXCONC,
            title = "TOXCONC_2018",
            opacity = 1)


#RSEI 93

mapit93 <- merge(LAtr, rsei93tr, by = "GEOID", all.x = T)

library(leaflet)
pal93 <- colorNumeric(
  palette = "Reds",
  domain = mapit93$TOXCONC
)

leaflet(mapit93) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = ~paste(round(TOXCONC,2)),
              fillColor = ~pal93(TOXCONC),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = alley, color = "#000000", weight = 3, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0,
              popup = ~paste("cancer alley"),
              #fillColor = "#000000",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal93, values = mapit93$TOXCONC,
            title = "TOXCONC_1993",
            opacity = 1)


#and with the NATA cancer risk

NataLA <- read_csv("cancer_alley/Cancer Risk LA.txt")
NataLA$GEOID <- NataLA$Tract
names(NataLA$`Total Cancer Risk (per million)`)
library(dplyr)
NataLA_totcan <- NataLA %>% group_by(GEOID) %>%
  summarise(canrisk_permil = sum(`Total Cancer Risk (per million)`, na.rm =T))

mapit2 <- merge(LAtr, NataLA_totcan, by = "GEOID", all.x = T)

pal <- colorNumeric(
  palette = "Reds",
  domain = mapit2$canrisk_permil
)

leaflet(mapit2) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = ~paste(round(canrisk_permil,2)),
              fillColor = ~pal(canrisk_permil),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = alley, color = "#000000", weight = 3, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0,
              popup = ~paste("cancer alley"),
              #fillColor = "#000000",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = mapit2$canrisk_permil,
            title = "NATA 14 Cancer Risk Per Million",
            opacity = 1)


# one page maps 

# isolate TRI facilities and Tracts 

#get full TRI dataset


library(readr)
TRIload <- function(year){
  url <- paste0("https://enviro.epa.gov/enviro/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/",
                year, "/fname/TRI_", year, "_US.csv/CSV")
  df <- read_csv(url)
  return(df)
}

tst <- TRIload(2019)

tri_90to18 <- lapply(1987:2019, function (x)  TRIload(x))
tri_90to18_df <- tri_90to18[[1]]
for (i in 2:length(tri_90to18)) tri_90to18_df <- rbind(tri_90to18_df, tri_90to18[[i]])

steri <- tri_90to18_df %>% filter(grepl("STERIGENICS", `4. FACILITY NAME`) |
                                    grepl("STERIGENICS", `15. PARENT CO NAME`))

steri_2 <- tri_90to18_df %>% filter(grepl("STERI", `4. FACILITY NAME`),
                                    !grepl("STERIS", `4. FACILITY NAME`)) %>%
  filter(`34. CHEMICAL` == "Ethylene oxide")
unique(par$`17. STANDARD PARENT CO NAME`)
par <- steri_2 %>% group_by(`17. STANDARD PARENT CO NAME`, `1. YEAR`) %>%
  summarise(avg_eto_stack = mean(`46. 5.2 - STACK AIR`, na.rm =T)) %>%
  filter(`17. STANDARD PARENT CO NAME` %in% c("ALTAIR CORP",
                                              "STERIGENICS US LLC",
                                              "MIDWEST STERILIZATION CORP")) %>%
  filter(`1. YEAR` > 1995)

plot_ly(par, x = ~`1. YEAR`,
        y = ~avg_eto_stack,
        color =~`17. STANDARD PARENT CO NAME`,
        mode = "lines+markers") %>%
  layout(xaxis = list(range = c(2005, 2020),
                      tickvals = c(2004:2019)))

unique(steri$`34. CHEMICAL`)
steri_eto <- steri %>% filter(`34. CHEMICAL` == "Ethylene oxide")

plot_ly(steri_eto, x = ~`1. YEAR`,
        y = ~`46. 5.2 - STACK AIR`+`45. 5.1 - FUGITIVE AIR`,
        color =~`4. FACILITY NAME`,
        mode = "lines") %>%
  layout(xaxis = list(range = c(2005, 2019)))

#facs in Lousiana 
names(tri_90to18_df)
trila <- tri_90to18_df %>% filter(`8. ST` == "LA") %>% 
  dplyr::select(2,3,4,5,6,7,12,13, 42) %>% distinct()
names(trila)
#now lets grab the facilities in the buffer

polygon.contains 

coordinates(trila) <- c("13. LONGITUDE", "12. LATITUDE")
CRS.new <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(trila) <- CRS.new 

pi <- rgeos::intersect(trila, alley)

myfacs <- as.data.frame(pi)

#ok here we go 

#usa vs cancer alley 


tri <- tri_90to18_df$`46. 5.2 - STACK AIR` %>% mutate(cancer_alley = ifelse(`2. TRIFD` %in% myfacs$X2..TRIFD,
                                                      "Cancer Alley", "Rest of USA"))

tri <- tri %>% rowwise() %>%
  mutate(Tot_Air = sum(`45. 5.1 - FUGITIVE AIR`, `46. 5.2 - STACK AIR`, na.rm = T)) %>% ungroup()

rest_of_us <- 2288834777


tri_ts <- tri %>% filter(`1. YEAR` > 2004) %>% group_by(cancer_alley, `1. YEAR`) %>%
  summarise(Air_Emissions = sum(Tot_Air)) %>% ungroup()

tri_ts <- tri_ts %>% group_by(cancer_alley) %>% mutate(firstyear = Air_Emissions[1],
Lastyears = lag(Air_Emissions),
percentchangefrom2005 = (Air_Emissions - firstyear)/firstyear)

library(plotly)

plot_ly(data = tri_ts, x = ~`1. YEAR`,
        y = ~percentchangefrom2005,
        color = ~cancer_alley, mode = "lines") %>%
  layout(xaxis = list(showgrid = F,
                      title = "Year"),
         yaxis = list(tickformat = "%",
                      showgrid = F,
                      title = "Change in TRI Air Emissions from 2005"))


#bubbles of the facilities 

tri_facs <- tri %>% filter(cancer_alley == "Cancer Alley") %>%
  group_by(`2. TRIFD`, `12. LATITUDE`, `13. LONGITUDE`) %>% 
  summarise(Air_Emissions = sum(Tot_Air)) %>% ungroup()

leaflet() %>% addTiles() %>%
  addCircleMarkers(data = tri_facs, lat = ~`12. LATITUDE`, lng = ~`13. LONGITUDE`,
             radius = ~Air_Emissions/3000000, weight = 0, fillColor = "red",
             label = NULL,
             ) %>%
  addPolygons(data = alley, color = "#000000", weight = 1, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0,
              popup = ~paste("cancer alley"),
              #fillColor = "#000000",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) 



#EJscore
names(EJscreen)
EJscreen <- read_csv("data/EJscreen/EJSCREEN_2019_USPR.csv")
rsei_block <- read_csv("data/censusmicroblockgroup2018_2018.csv",
                       col_names = F)
> rsei_fac <- read_csv("D:/Downloads Overflow/RSEIv238_spring_Public_Release_Data/facility_data_rsei_v238.csv") 
names(rsei_block)  <- c("GEOID10",
                        "ReleaseNumber",
                        "ChemicalNumber",
                        'FacilityNumber',
                        "Media",
                        'Conc',
                        "ToxConc",
                        "Score",
                        "CSCORE",
                        "NCSCORE",
                        "POP")
myej <- EJscreen %>% dplyr::select(VULEOPCT, ID)
myej$ID <- as.character(myej$ID)
rsei_block$GEOID10 <- as.character(rsei_block$GEOID10)
EJblock <- left_join(rsei_block, myej,  by = c("GEOID10" = "ID"))

EJblock$ejscore <- EJblock$Score * EJblock$VULEOPCT

facsej <- EJblock %>% group_by(FacilityNumber) %>%
  summarise(ejscore_fac = sum(ejscore, nna.rm = T),
            score = sum(Score, na.rm =T)) 

facsej <- facsej %>% ungroup()

dfej<- left_join(facsej, rsei_fac)

top100 <- dfej %>% top_n(100, ejscore_fac)


leaflet() %>% addTiles() %>%
  addCircleMarkers(data = top100, lat = ~Latitude, lng = ~Longitude,
                   radius = ~ejscore_fac/300000, weight = 0, fillColor = "red",
                   label = NULL,
  ) %>%
  addPolygons(data = alley, color = "#000000", weight = 1, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0,
              popup = ~paste("cancer alley"),
              #fillColor = "#000000",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) 


dfej <- dfej %>% mutate(cancer_alley = ifelse(FacilityID %in% myfacs$X2..TRIFD,
                             "Cancer Alley", "Rest of USA"))

sumchart <- dfej %>% group_by(cancer_alley) %>% summarise(mean_ej)

plot_ly(data = dfej, y = ~ejscore_fac,
        color = ~cancer_alley, type = "box",
        boxpoints = "all",
        jitter = 0.3) %>%
  layout(xaxis = list(showgrid = F,
                      title = "Year"),
         yaxis = list(type = "log",
                      showgrid = F,
                      title = "2018 RSEI Score * EJSCREEN Vulnerable Population Index"))


#ECHO fac file 

echo <- read_csv("~/GitHub/enviro-health-data-utils/data/cancer_alley/ECHO_EXPORTER.csv")
names(echo)
echo_raw <- echo
echo <- echo %>% filter(!is.na(FAC_LONG), !is.na(FAC_LAT))
coordinates(echo) <- c("FAC_LONG", "FAC_LAT")
CRS.new <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(echo) <- CRS.new 

pi <- rgeos::intersect(echo, alley)

myecho <- as.data.frame(pi)

pal <- colorFactor( palette = "YlOrRd", domain = myecho$EJSCREEN_FLAG_US)

myecho$EJSCREEN_FLAG_US

#eco map
leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data = myecho, lat = ~FAC_LAT, lng = ~FAC_LONG, radius = 3,
                   #radius = ~ejscore_fac/300000,
                   weight = .1, fillColor = ~pal(EJSCREEN_FLAG_US), fillOpacity = .7,
                   label = NULL,
  ) %>%
  addPolygons(data = alley, color = "#000000", weight = 1, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0,
              popup = ~paste("cancer alley"),
              #fillColor = "#000000",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = myecho$EJSCREEN_FLAG_US,
            title = "Indicates facilities located in Census block groups in the 80th or higher national percentile of one of the primary environmental justice (EJ) indexes of EJSCREEN, EPA's screening tool for EJ concerns. EPA screens areas in this way to identify geographic areas that may warrant further consideration or analysis. Note that use of this field does not designate an area as an "EJ community" or "EJ facility."",
            opacity = 1
  )

#All air facilities 
air <- read_csv("data/POLL_RPT_COMBINED_EMISSIONS.csv")

#combine with echo fac file 
head(air)
adder <- echo_raw %>% dplyr::select(FAC_LONG, FAC_LAT, REGISTRY_ID) %>% distinct() %>%
  group_by(REGISTRY_ID) %>% summarise(FAC_LONG = FAC_LONG[1],
                                      FAC_LAT = FAC_LAT[1]) %>% ungroup()

air <- left_join(air, adder)

air$cancer_alley <- ifelse(air$REGISTRY_ID %in% myecho$REGISTRY_ID, "Cancer Alley", "Rest of USA")

air_chem_year <- air %>% group_by(POLLUTANT_NAME, UNIT_OF_MEASURE,REPORTING_YEAR, cancer_alley) %>% 
  summarise(ANNUAL_EMISSION = sum(ANNUAL_EMISSION, na.rm =T))

air_chem <- air %>% group_by(POLLUTANT_NAME, UNIT_OF_MEASURE,cancer_alley, PGM_SYS_ACRNM) %>% 
  summarise(ANNUAL_EMISSION = sum(ANNUAL_EMISSION, na.rm =T))

tstchems <- c(
  "Hexane",
  "Ammonia",
  "Ethylene",
  "Methanol",
  "Propylene",
  "Benzene"
)

air_chem_tst <- air_chem_year %>% 
  filter(POLLUTANT_NAME == "Sulfur dioxide") %>%
  mutate(pol_loc = paste(POLLUTANT_NAME, cancer_alley))


plot_ly(data = air_chem_tst, x = ~REPORTING_YEAR, y = ~ANNUAL_EMISSION,
        color = ~pol_loc, mode = "line")


#cancer data
library(readxl)
cancer_df <- read_excel("~/GitHub/enviro-health-data-utils/data/cancer_alley/Copy of census report 2021_tables for appendix_final.xlsx", 
                           skip = 1)


cancer_df <- cancer_df %>% filter(!is.na(`Census Tract`))
cancer_df <- cancer_df %>% filter(`Census Tract` != "Column2")
cancer_df$GEOID <- cancer_df$`Census Tract`


#tract ejscreen in Lou
names(EJscreen)
head(EJscreen$ID)
EJscreen$GEOID = substr(EJscreen$ID, 1, 11)

latr_ej <- EJscreen %>% filter(GEOID %in% cancer_df$GEOID) %>%
  group_by(GEOID) %>%
  summarise(ACSTOTPOP = sum(ACSTOTPOP, na.rm = T),
            MINORPOP = sum(MINORPOP, na.rm = T),
            LOWINCOME = sum(LOWINCOME, na.rm =T),
            CANCER = CANCER[1]) %>%
  mutate(MINORPOP_PCT = (MINORPOP/ACSTOTPOP)*100,
         LOWINCOME_PCT = (LOWINCOME/ACSTOTPOP)*100) %>% ungroup()

cancer_df <- left_join(cancer_df, latr_ej)
cancer_df <- cancer_df %>%
  mutate(CANCER = as.numeric(CANCER),
         Rate = as.numeric(`Rate*`))

#Whats the relationship between cancer risk and minority status in Lousiana? 

#whats the relationship between cancer risk and cancer rates in lousiana?

library(plotly)
fv <- cancer_df %>% filter(!is.na(Rate), !is.na(CANCER)) %>%
  lm(CANCER ~ Rate,.) %>% fitted.values()
plot_ly(cancer_df %>% filter(!is.na(Rate), !is.na(CANCER)), y = ~CANCER,
        x = ~Rate, type = "scatter", name = "Tract") %>%
  add_trace(x = ~Rate, y = fv, mode = "lines", name = "Correlation")

summary(lm(cancer_df$Rate ~ cancer_df$CANCER))

#what about a general cancer to cancer risk estimate? 

library(plotly)
fv <- cancer_df %>% filter(!is.na(MINORPOP_PCT), !is.na(CANCER)) %>%
  lm(CANCER ~ MINORPOP_PCT,.) %>% fitted.values()
plot_ly(cancer_df %>% filter(!is.na(MINORPOP_PCT), !is.na(CANCER)), y = ~CANCER,
        x = ~MINORPOP_PCT, type = "scatter", name = "Tract",
        text = ~paste(GEOID)) %>%
  add_trace(x = ~MINORPOP_PCT, y = fv, mode = "lines", name = "Correlation")

summary(lm(cancer_df$MINORPOP_PCT ~ cancer_df$CANCER))

#can use EJSCREEN to upscale to tracts 

fv <- cancer_df %>% filter(!is.na(LOWINCOME_PCT), !is.na(CANCER)) %>%
  lm(CANCER ~ LOWINCOME_PCT,.) %>% fitted.values()
plot_ly(cancer_df %>% filter(!is.na(LOWINCOME_PCT), !is.na(CANCER)), y = ~CANCER,
        x = ~LOWINCOME_PCT, type = "scatter", name = "Tract",
        text = ~paste(GEOID)) %>%
  add_trace(x = ~LOWINCOME_PCT, y = fv, mode = "lines", name = "Correlation")

summary(lm(cancer_df$LOWINCOME_PCT ~ cancer_df$CANCER))

#maps

cancer_tr <- merge(LAtr, cancer_df, by = "GEOID")

cancer_tr$Rate = as.numeric(cancer_tr$`Rate*`)

library(leaflet)
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = cancer_tr$Rate
)

leaflet(cancer_tr) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = ~paste(round(Rate,2)),
              fillColor = ~pal(Rate),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  # addPolygons(data = alley, color = "#000000", weight = 3, smoothFactor = 0.5,
  #             opacity = 1, fillOpacity = 0,
  #             popup = ~paste("cancer alley"),
  #             #fillColor = "#000000",
  #             highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                 bringToFront = TRUE)) %>%
  addPolygons(data = alley_5, color = "#222222", weight = 3, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0,
              popup = ~paste("cancer alley 5m"),
              fillColor = "#222222",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(pal = pal, values = cancer_tr$Rate,
            title = "All Site Cancer Rate 2008-2017", position = "topright",
            opacity = 1)


library(leaflet)
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = cancer_tr$LOWINCOME_PCT
)

leaflet(cancer_tr) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = ~paste(round(LOWINCOME_PCT,2)),
              fillColor = ~pal(LOWINCOME_PCT),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = alley, color = "#000000", weight = 3, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0,
              popup = ~paste("cancer alley"),
              #fillColor = "#000000",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(pal = pal, values = cancer_tr$LOWINCOME_PCT,
            title = "LOWINCOME_PCT", position = "topright",
            opacity = 1)

#
library(rgdal)
alley_5 <- readOGR("./cancer_alley/cancer_alley_msriv_5_10mi_buffer", "cancer_alley_msriv_5mi")
alley_5 <- spTransform(alley_5, CRS("+proj=longlat +datum=WGS84"))

library(rgdal)
alley_5_cities <- readOGR("./cancer_alley/cancer_alley_msriv_5_10mi_buffer", "cancer_alley_msriv_5mi_cities")
alley_5_cities <- spTransform(alley_5_cities, CRS("+proj=longlat +datum=WGS84"))

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = cancer_tr$CANCER
)

leaflet(cancer_tr) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = ~paste(round(CANCER,2)),
              fillColor = ~pal(CANCER),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  # addPolygons(data = alley, color = "#000000", weight = 3, smoothFactor = 0.5,
  #             opacity = 1, fillOpacity = 0,
  #             popup = ~paste("cancer alley"),
  #             #fillColor = "#000000",
  #             highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                 bringToFront = TRUE)) %>%
  addPolygons(data = alley_5_cities, color = "#222222", weight = 3, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0,
              popup = ~paste("cancer alley 5m"),
              fillColor = "#222222",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(pal = pal, values = cancer_tr$CANCER,
            title = "All Site Cancer Risk 2014", position = "topright",
            opacity = 1)


#here is the task 
#grab data for these census blocks via ejscreen and RSEI 
ca <- read_excel("~/GitHub/enviro-health-data-utils/data/tl_2019_22_tablock_ca5mi_full.xlsx")

ca$ID <- substr(ca$GEOID10, 1, 12)

#ejscreen data 
EJscreen$CANCER <- as.numeric(EJscreen$CANCER)
EJscreen$DSLPM <- as.numeric(EJscreen$DSLPM)
EJscreen$RESP <- as.numeric(EJscreen$RESP)
EJscreen$OZONE <- as.numeric(EJscreen$OZONE)
EJscreen$PM25 <- as.numeric(EJscreen$PM25)

EJscreen$place <- ifelse(EJscreen$ID %in% ca$ID, "Industrial Corridor", "Rest of USA")

comp <- EJscreen %>% group_by(place) %>%
  summarise_at(.vars = 3:37, .funs = mean, na.rm = T)

#TRI 
TRI_2019_US_1_ <- read_csv("C:/Users/Mike Petroni/Downloads/TRI_2019_US (1).csv")

alley <- subset(LAbg, LAbg$GEOID %in% ca$ID)
trila <- TRI_2019_US_1_ %>% filter(`8. ST` == "LA", !is.na(`13. LONGITUDE`)) %>% 
  dplyr::select(2,3,4,5,6,7,12,13, 42) %>% distinct()
coordinates(trila) <- c("13. LONGITUDE", "12. LATITUDE")
CRS.new <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(trila) <- CRS.new 
pi <- rgeos::intersect(trila, alley)
myfacs <- as.data.frame(pi)

TRI_2019_US_1_$place <- ifelse(TRI_2019_US_1_$`2. TRIFD` %in% myfacs$X2..TRIFD, "Industrial Corridor", "Rest of USA")

compTRI <- TRI_2019_US_1_ %>% group_by(place) %>%
  summarise_at(.vars = c(101, 113), .funs = sum, na.rm = T)

#ECHO
echo <- read_csv("~/GitHub/enviro-health-data-utils/data/cancer_alley/ECHO_EXPORTER.csv")
names(echo)
echo_raw <- echo
echo <- echo %>% filter(!is.na(FAC_LONG), !is.na(FAC_LAT))
coordinates(echo) <- c("FAC_LONG", "FAC_LAT")
CRS.new <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(echo) <- CRS.new 

pi <- rgeos::intersect(echo, alley)

myecho <- as.data.frame(pi)


echo_raw$place <- ifelse(echo_raw$REGISTRY_ID %in% myecho$REGISTRY_ID, "Industrial Corridor", "Rest of USA")
names(echo_raw)
compecho <- echo_raw %>% group_by(place) %>%
  summarise_at(.vars = c(44, 40, 41, 33), .funs = mean, na.rm = T)

#combine and export 

compdf <- left_join(comp, compTRI)
compdf <- left_join(compdf, compecho)
ejalley <- EJscreen %>% filter(place == "Industrial Corridor")
write.csv(compdf, "comparing LA industrial to USA.csv")
write.csv(ejalley, "EJscreen LA industrial.csv")


sum(ejalley$ACSTOTPOP)
sum(EJscreen$ACSTOTPOP) - sum(ejalley$ACSTOTPOP)


head(myecho)

write.csv(myecho, "echoexporter_LA_industrial_corridor.csv")

            