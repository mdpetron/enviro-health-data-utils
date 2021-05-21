# survey of manufactures waste management costs in four industries 

# grab data 
ASM_17 <- read_csv("C:/Users/Mike Petroni/Downloads/ASMAREA2017.AM1831BASIC01_2020-10-28T081656/ASMAREA2017.AM1831BASIC01_data_with_overlays_2020-10-22T113112.csv")
ASM_16 <- read_excel("C:/Users/Mike Petroni/Downloads/ASM_2016_31GS101_with_ann (1).xlsx")
ASM_15 <- read_excel("C:/Users/Mike Petroni/Downloads/ASM_2015_31GS101_with_ann.xlsx")
ASM_14 <- read_excel("C:/Users/Mike Petroni/Downloads/ASM_2014_31GS101_with_ann.xlsx")
ASM_13 <- read_excel("C:/Users/Mike Petroni/Downloads/ASM_2013_31GS101_with_ann.xlsx")

#reduce
ASM_17_short <- ASM_17 %>% select(NAME, PCHRFUS, NAICS2017, NAICS2017_LABEL, YEAR, RCPTOT)
ASM_16_short <- ASM_16 %>% select(`Geographic area name` , `Refuse removal (including hazardous waste) services ($1,000)`, 2, 3, 4, 43)
ASM_15_short <- ASM_15 %>% select(`Geographic area name` , `Refuse removal (including hazardous waste) services ($1,000)`, 2, 3, 4, 43)
ASM_14_short <- ASM_14 %>% select(`Geographic area name` , `Refuse removal (including hazardous waste) services ($1,000)`, 2, 3, 4, 43)
ASM_13_short <- ASM_13 %>% select(`Geographic area name` , `Refuse removal (including hazardous waste) services ($1,000)`, 2, 3, 4, 43)                                

#rename
names(ASM_17_short) <- names(ASM_16_short)
summary(ASM_17_short)

#join
asm12to18 <- rbind(ASM_17_short,
                       ASM_16_short,
                       ASM_15_short,
                       ASM_14_short,
                       ASM_13_short)

#grab the ones we want

sectors <- asm12to18 %>% filter(`2012 NAICS code` %in% c("325",
                                                         "334",
                                                         "332",
                                                         "336"))
sectors <- sectors %>% mutate(Waste_Cost_Per_Shipment_Value = as.numeric(`Refuse removal (including hazardous waste) services ($1,000)`)/
                                as.numeric(`Total value of shipments and receipts for services ($1,000)`))  
  
  
plot_ly() %>% 
  add_trace(data = sectors,
            y = ~Waste_Cost_Per_Shipment_Value, x = ~as.numeric(Year), mode = 'lines', color = ~`Meaning of 2012 NAICS code`, 
            marker = list(color = "#bbbbbb", opacity = .8, size = 9)) 
  layout(
    # xaxis = list(title = "Average Level of Hazardous Air Pollutants (Adusted by Respiratory Toxicity)"),
    #      yaxis = list(title = "Covid-19 Mortality per 10,000 Population as of October 20, 2020"), 
         legend = list(x = .56, 
                       y = .92,
                       font = list(color= "black"),
                       bgcolor = "white",
                       bordercolor = "#323232",
                       borderwidth = 2))  
  
  #what about a simple bar chart
  library(viridis)
  plot_ly() %>% 
    add_trace(data = sectors %>% filter(Year == "2018"),
              x = ~as.numeric(`Refuse removal (including hazardous waste) services ($1,000)`)*1000,
              y = ~`Meaning of 2012 NAICS code`, mode = 'bar', colors = viridis_pal(option = "D")(4),
              color = ~`Meaning of 2012 NAICS code`, orientation = "h") %>%
  layout(
     xaxis = list(title = "2018 Cost for Refuse Removal Services (Including Hazardous Waste)"),
          yaxis = list(title = "Manufacturing Sector"),
                       showlegend = F, 

    legend = list(x = .56, 
                  y = .92,
                  font = list(color= "black"),
                  bgcolor = "white",
                  bordercolor = "#323232",
                  borderwidth = 2)) 



