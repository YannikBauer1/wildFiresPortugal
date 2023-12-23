#introduction to RNOAA package: http://spatialecology.weebly.com/r-code--data/34

library('rnoaa')
library(tidyverse)
library(lubridate)
require(devtools)
options(noaakey = "mqEuOSuAUjyuGlTjVjxxCpzRlbrooRnr")
#To gain access to NCDC CDO Web Services, you must obtain a token using this link and following the directions given. http://www.ncdc.noaa.gov/cdo-web/token

# Get available stations
#station_data <- ghcnd_stations() # Takes a while to run and you can load form the available R object 
#save(station_data,file="station_data.RData")

load("station_data.Rdata")




#define the GPS coordinates of a fire event 
df <- data.frame(
  id = c("Porto"), 
  latitude = c(41.755673 ),
  longitude = c(-8.601734),
  stringsAsFactors = FALSE
)


#Get nearby stations that can provide the mean average temperature (TAVG)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = df,
                                          station_data = station_data, radius = 1000, 
                                          var = c("TAVG"),
                                          year_min = 2014, year_max = 2015)
nearby_stations <- nearby_stations$Porto %>% select(id,name,latitude,longitude,distance)
#Get TAVG data
weather_data <- ghcnd_search(nearby_stations$id[1], var = c("TAVG") , date_min = "2014-01-01", date_max = "2015-12-31")
weather_data

#Get nearby stations that can provide the Maximum temperature (TMAX)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = df,
                                          station_data = station_data, radius = 1000, 
                                          var = c("TMAX"),
                                          year_min = 2014, year_max = 2015)

print(nearby_stations)

#Get TMAX data
weather_data <- ghcnd_search(nearby_stations[[1]]$id[1], var = c("TMAX") , date_min = "2014-01-01", date_max = "2015-12-31")

#n <- nearby_stations$Porto %>% select(name,latitude,longitude)
n <- nearby_stations$Porto %>% filter(latitude < 42.2,latitude>36.8, longitude> -9.6,longitude< -6.1)
n2 <- n %>% slice(c(1,2,3,5,7,8,11))
print(n,n=300)
#print(nearby_stations,n=200)

#11-4

