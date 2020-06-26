#ECHO + EJSCREEN
#need a computer with available RAM for these datasets, at least 8GB

#EJSCREEN
# Homepage: https://www.epa.gov/ejscreen
# Mapping tool: https://ejscreen.epa.gov/mapper/
#   Data download and documentation portal: https://www.epa.gov/ejscreen/download-ejscreen-data

download.file("ftp://newftp.epa.gov/EJSCREEN/2019/EJSCREEN_2019_USPR.csv.zip",
              destfile = "data/EJSCREEN_2019_USPR.csv.zip")

unzip("data/EJSCREEN_2019_USPR.csv.zip", exdir="./data")
ejScreen <- read_csv("data/EJSCREEN_2019_USPR.csv")

#ECHO
# Homepage: https://echo.epa.gov/
# Example Facility Report: https://echo.epa.gov/detailed-facility-report?fid=110067396669
# Data download and documentation portal: https://echo.epa.gov/tools/data-downloads#exporter

download.file("https://echo.epa.gov/files/echodownloads/echo_exporter.zip",
              destfile = "data/echo_exporter.zip")
unzip("data/echo_exporter.zip", exdir="./data")
echo <- read_csv("data/ECHO_EXPORTER.CSV")

#do facilities in minority areas have more violations? 
names(echo)
unique(echo$EJSCREEN_FLAG_US)

#to make this easy we are going to use the EJSCREEN Flag from ECHO

ej_flag <- echo %>% filter(EJSCREEN_FLAG_US == "Y")
noej <- echo %>% filter(EJSCREEN_FLAG_US == "N")

#check the means
mean(ej_flag$FAC_PENALTY_COUNT, na.rm = T)
mean(noej$FAC_PENALTY_COUNT, na.rm = T)

#perform a Welch Two Sample t-test
# https://uc-r.github.io/t_test
t.test(ej_flag$FAC_PENALTY_COUNT,
       noej$FAC_PENALTY_COUNT)

# the means are not equal, and ej areas have higher penalty counts...
# of course, we need to check to see if we don't violate assumptions


