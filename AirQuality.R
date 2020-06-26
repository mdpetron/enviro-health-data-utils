#Air Quality Monitoring

#Source https://www.epa.gov/outdoor-air-quality-data
#Descriptions: https://aqs.epa.gov/aqsweb/airdata/FileFormats.html

#grab the 2019 annual concentration file by monitor
download.file("https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_2019.zip",
              destfile = "data/annual_conc_by_monitor_2019.zip", mode = "wb")
unzip("data/annual_conc_by_monitor_2019.zip")

#read in
monitors <- read_csv("annual_conc_by_monitor_2019.csv")

# get monitors in LA
unique(monitors$`State Name`)
monitors_la <- monitors %>% filter(`State Name` == "Louisiana")

#get Chloroprene
unique(monitors_la$`Parameter Name`)
monitors_la_chloro <- monitors_la %>% filter(`Parameter Name` == "Chloroprene",
                                             `Observation Percent` == 100)

#Map them

library(leaflet)
pal = colorNumeric("RdYlBu", domain = monitors_la_chloro$`Arithmetic Mean`, reverse = T)
leaflet(data = monitors_la_chloro) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude,  col = ~pal(`Arithmetic Mean`), opacity = 0.9,
             popup = ~`Local Site Name`, radius =100) %>% 
  addLegend(pal = pal, values = ~`Arithmetic Mean`, title = "Mean Chloroprene PPB, 2019") %>% 
  setView(lng = -90.48, 30.06, zoom = 12) %>% 
  addMiniMap()


monitors_la_pm <- monitors_la %>% filter(`Parameter Name` == "PM2.5 - Local Conditions",
                                         `Metric Used` == "Daily Mean")

#Map them

library(leaflet)
pal = colorNumeric("RdYlBu", domain = monitors_la_pm$`Arithmetic Mean`, reverse = T)
leaflet(data = monitors_la_pm) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude,  col = ~pal(`Arithmetic Mean`), opacity = 0.9,
             popup = ~`Local Site Name`, radius =5000) %>% 
  addLegend(pal = pal, values = ~`Arithmetic Mean`, title = "Mean PM2.5 Micrograms/cubic meter (LC), 2019") %>% 
  setView(lng = -90.48, 30.06, zoom = 8) %>% 
  addMiniMap()

