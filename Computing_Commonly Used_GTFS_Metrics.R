# Extract Transit Metrics from GTFS
# Xilei Zhao
# 05/20/2019

library(lubridate)
library(tictoc)

# Clean memory
rm(list = ls())
cat("\014")
# Read Data
setwd("/Users/xzhao404/Dropbox/Cindy, Xilei and Jacob/Transportation data/GTFS/google_transit")
cal = read.csv("calendar.csv")
route = read.csv("routes.csv")
trip = read.csv("trips.csv")
shape = read.csv("shapes.csv")
stop_time = read.csv("stop_times.csv")
trip$route_id = as.character(trip$route_id)

stop_time$arrival_time1 = as.numeric(substr(as.character(stop_time$arrival_time), 1, 2)) + 
  as.numeric(substr(as.character(stop_time$arrival_time), 4, 5))/60 + 
  as.numeric(substr(as.character(stop_time$arrival_time), 7, 8))/3600

stop_time$departure_time1 = as.numeric(substr(as.character(stop_time$departure_time), 1, 2)) + 
  as.numeric(substr(as.character(stop_time$departure_time), 4, 5))/60 + 
  as.numeric(substr(as.character(stop_time$departure_time), 7, 8))/3600


#stop_time$arrival_time = strptime(stop_time$arrival_time, format = "%H:%M:%S") # Change time format of the data
#stop_time$departure_time = strptime(stop_time$departure_time, format = "%H:%M:%S") # Change time format of the data

#service_id_sat = cal[cal$saturday == 1 & cal$monday == 0 & cal$tuesday == 0 
#                      & cal$wednesday == 0 & cal$thursday == 0 & cal$friday == 0,]$service_id
#service_id_sun = cal[cal$sunday == 1 & cal$monday == 0 & cal$tuesday == 0 
#                     & cal$wednesday == 0 & cal$thursday == 0 & cal$friday == 0,]$service_id
#service_id_weekday = unique(cal[!(cal$service_id %in% c(service_id_sat,service_id_sun)),]$service_id)

service_id_mon = cal[cal$monday == 1 ,]$service_id
service_id_tues = cal[cal$tuesday == 1 ,]$service_id
service_id_wed = cal[cal$wednesday == 1 ,]$service_id
service_id_thur = cal[cal$thursday == 1 ,]$service_id
service_id_fri = cal[cal$friday == 1 ,]$service_id

route_id = as.character(route$route_id)
no_trip_per_route = matrix(data = 0, nrow = length(route_id), ncol = 7)

for (i in 1:length(route_id)){
  no_trip_per_route[i,1] = route_id[i]
  no_trip_per_route[i,2] = nrow(trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_mon,])
  no_trip_per_route[i,3] = nrow(trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_tues,])
  no_trip_per_route[i,4] = nrow(trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_wed,])
  no_trip_per_route[i,5] = nrow(trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_thur,])
  no_trip_per_route[i,6] = nrow(trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_fri,])
  no_trip_per_route[i,7] = mean(as.numeric(as.character(no_trip_per_route[i,2:6])))
}

colnames(no_trip_per_route) <- c("Route", "Mon", "Tues", "Wed", "Thur", "Fri", "Avg")

#write.csv(no_trip_per_route, "results/no_trips_per_route.csv")

route_id = route_id[-c(12,18,107)]   # Remove routes that are not operating

opr_hr_per_route = matrix(data = 0, nrow = length(route_id), ncol = 7)

for (i in 1:length(route_id)){
  opr_hr_per_route[i,1] = route_id[i]
  
  # Monday
  trip.id <- trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_mon,]$trip_id
  time = 0
  for (j in 1:length(trip.id)){
    timetable = stop_time[stop_time$trip_id == trip.id[j],]
    time <- time + timetable$departure_time1[nrow(timetable)] - timetable$arrival_time1[1]
  }
  opr_hr_per_route[i,2] = time

# Tuesday
trip.id <- trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_tues,]$trip_id
time = 0
for (j in 1:length(trip.id)){
  timetable = stop_time[stop_time$trip_id == trip.id[j],]
  time <- time + timetable$departure_time1[nrow(timetable)] - timetable$arrival_time1[1]
}
opr_hr_per_route[i,3] = time
  
  # Wednesday
trip.id <- trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_wed,]$trip_id
time = 0
for (j in 1:length(trip.id)){
  timetable = stop_time[stop_time$trip_id == trip.id[j],]
  time <- time + timetable$departure_time1[nrow(timetable)] - timetable$arrival_time1[1]
}
opr_hr_per_route[i,4] = time
  
  # Thursday
trip.id <- trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_thur,]$trip_id
time = 0
for (j in 1:length(trip.id)){
  timetable = stop_time[stop_time$trip_id == trip.id[j],]
  time <- time + timetable$departure_time1[nrow(timetable)] - timetable$arrival_time1[1]
}
opr_hr_per_route[i,5] = time
  
  # Friday
trip.id <- trip[trip$route_id == route_id[i] & trip$service_id %in% service_id_fri,]$trip_id
time = 0
for (j in 1:length(trip.id)){
  timetable = stop_time[stop_time$trip_id == trip.id[j],]
  time <- time + timetable$departure_time1[nrow(timetable)] - timetable$arrival_time1[1]
}
opr_hr_per_route[i,6] = time
  
  opr_hr_per_route[i,7] = mean(as.numeric(as.character(opr_hr_per_route[i,2:6])))
  
}

colnames(opr_hr_per_route) <- c("Route", "Mon", "Tues", "Wed", "Thur", "Fri", "Avg")

write.csv(opr_hr_per_route, "results/no_hr_per_route.csv")

