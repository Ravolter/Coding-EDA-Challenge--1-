
library(openxlsx)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(lubridate)
library(sqldf)
library(reshape2)

setwd("E:/All online courses/Oolala Cabs")
dataset_raw <- read.csv("dataset.csv", stringsAsFactors = F)
summary(dataset_raw)
dim(dataset_raw)
names(dataset_raw)

# Data Cleaning
## Removing Duplicates
dataset <- unique(dataset_raw)

## Removing canceled orders with drop_time not NUll and completed order that have a cancelletion
dataset <- dataset[(which(dataset$status == 5 & dataset$drop_time != 'NULL'))*-1,]
dataset <- dataset[(which(dataset$status == 4 & dataset$cancelled_by != 'NULL'))*-1,]

## Change cols to date
dataset$pickup_time <- as.POSIXct(dataset$pickup_time,format = '%d-%m-%Y %H:%M')
dataset$start_time <- as.POSIXct(dataset$start_time,format = '%d-%m-%Y %H:%M')
dataset$drop_time <- as.POSIXct(dataset$drop_time,format = '%d-%m-%Y %H:%M')

dataset$time_taken <- as.numeric(difftime(dataset$drop_time, dataset$start_time, units = "days"))
dataset$time_taken <- ifelse(is.na(dataset$time_taken),0,dataset$time_taken)
dataset$Pickup_hour <- hour(dataset$pickup_time)
# Creating Part of the day Column
# Morning: 4am - 9am
# Afternoon: 10am - 3pm
# Evening: 4pm - 9pm
# Night: 10pm - 3am
dataset$Pickup_part_of_day <- 'to be set'
dataset$Pickup_part_of_day <- ifelse(dataset$Pickup_hour >= 4 & dataset$Pickup_hour <= 9,'Morning',dataset$Pickup_part_of_day)
dataset$Pickup_part_of_day <- ifelse(dataset$Pickup_hour >= 10 & dataset$Pickup_hour <= 15,'Afternoon',dataset$Pickup_part_of_day)
dataset$Pickup_part_of_day <- ifelse(dataset$Pickup_hour >= 16 & dataset$Pickup_hour <= 21,'Evening',dataset$Pickup_part_of_day)
dataset$Pickup_part_of_day <- ifelse(dataset$Pickup_hour >= 22 | 
                                       dataset$Pickup_hour == 0 | 
                                       dataset$Pickup_hour == 1 | 
                                       dataset$Pickup_hour == 2 | 
                                       dataset$Pickup_hour == 3,'Night',dataset$Pickup_part_of_day)
dataset$Pickup_week <- weekdays(dataset$pickup_time)
## Date Column
dataset$date <- date(dataset$pickup_time)
## Removing Start time and drop time
dataset <- subset(dataset, select = -c(start_time,drop_time))
## Converting status to text
dataset$status <- as.character(dataset$status)
summary(dataset)
dim(dataset)

#1. Overall Highest Cancelling Category
Cancelled_by_freq <- as.data.frame(dataset %>% filter(status == '5') %>%
                                     group_by(cancelled_by) %>% 
                                     summarise(Frequency = n()) %>%  
                                     arrange(desc(Frequency)))
Cancelled_by_freq$cancelled_by <- factor(Cancelled_by_freq$cancelled_by, 
                                         levels = Cancelled_by_freq$cancelled_by[order(-Cancelled_by_freq$Frequency)])

ggplotly(ggplot(Cancelled_by_freq, aes(x = cancelled_by, y = Frequency)) +
           geom_bar(fill = "#0073C2FF", stat = "identity"))



  

#2. Highest Cancelling category for Airport drops
Airport_Cancelled_by_freq <- as.data.frame(dataset %>% filter(status == '5' & drop_loc == 'Airport') %>%
                                             group_by(cancelled_by) %>% 
                                             summarise(Frequency = n()) %>%  
                                             arrange(desc(Frequency)))
Airport_Cancelled_by_freq$cancelled_by <- factor(Airport_Cancelled_by_freq$cancelled_by, 
                                                 levels = Airport_Cancelled_by_freq$cancelled_by[order(-Airport_Cancelled_by_freq$Frequency)])

ggplotly(ggplot(Airport_Cancelled_by_freq[1:10,], aes(x = cancelled_by, y = Frequency)) +
           geom_bar(fill = "#0073C2FF", stat = "identity"))




#2.1 Proportion of driver cancelling among total categories for each drop location
Driver_Cancelled_by_freq <- as.data.frame(dataset %>% filter(status == '5' & cancelled_by == 'driver') %>%
                                            group_by(drop_loc) %>% 
                                            summarise(Frequency = n()) %>%  
                                            arrange(desc(Frequency)))
Driver_Cancelled_by_freq$drop_loc <- factor(Driver_Cancelled_by_freq$drop_loc, 
                                            levels = Driver_Cancelled_by_freq$drop_loc[order(-Driver_Cancelled_by_freq$Frequency)])
All_drop_loc_by_cancel_freq <- as.data.frame(dataset %>% filter(status == '5') %>%
                                               group_by(drop_loc) %>% 
                                               summarise(Frequency = n()) %>%  
                                               arrange(desc(Frequency)))
Driver_proportion_Cancelled_by_freq <- left_join(Driver_Cancelled_by_freq, All_drop_loc_by_cancel_freq,by = c("drop_loc" = "drop_loc"))
Driver_proportion_Cancelled_by_freq$driver_cancel_proportion <- paste0(round(Driver_proportion_Cancelled_by_freq$Frequency.x*100/Driver_proportion_Cancelled_by_freq$Frequency.y,0),'%')
Driver_proportion_Cancelled_by_freq <- Driver_proportion_Cancelled_by_freq[,c('drop_loc','driver_cancel_proportion')]

ggplotly(ggplot(Driver_proportion_Cancelled_by_freq[1:10,], aes(x = drop_loc, y = driver_cancel_proportion)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("Proportion of trips cancelled by drivers"))

ggplotly(ggplot(Driver_Cancelled_by_freq[1:10,], aes(x = drop_loc, y = Frequency)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("Absolute no. of trips cancelled by drivers"))





#3. Airport drop_loc Cancelled by driver - pickup_loc wise frequency

Airport_Cancelled_by_driver_freq <- as.data.frame(dataset %>% 
                                                    filter(status == '5' & drop_loc == 'Airport' & cancelled_by == 'driver') %>%
                                                    group_by(pickup_loc) %>% 
                                                    summarise(Frequency = n()) %>%  
                                                    arrange(desc(Frequency)))
Airport_Cancelled_by_driver_freq$pickup_loc <- factor(Airport_Cancelled_by_driver_freq$pickup_loc, 
                                                      levels = Airport_Cancelled_by_driver_freq$pickup_loc[order(-Airport_Cancelled_by_driver_freq$Frequency)])

ggplotly(ggplot(Airport_Cancelled_by_driver_freq[1:10,], aes(x = pickup_loc, y = Frequency)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("Pickup Location wise - Airport drop cancellations"))




#4. Airport drop_loc Cancelled by driver - pickup_loc wise avg Travel Distance

Airport_Cancelled_by_driver_Avg_dist <- as.data.frame(dataset %>% 
                                                        filter(status == '5' & drop_loc == 'Airport' & cancelled_by == 'driver') %>%
                                                        group_by(pickup_loc) %>% 
                                                        summarise(Avg_dist = mean(travel_distance)) %>%  
                                                        arrange(desc(Avg_dist)))
Airport_Cancelled_by_driver_Avg_dist$pickup_loc <- factor(Airport_Cancelled_by_driver_Avg_dist$pickup_loc, 
                                                          levels = Airport_Cancelled_by_driver_Avg_dist$pickup_loc[order(-Airport_Cancelled_by_driver_Avg_dist$Avg_dist)])

ggplotly(ggplot(Airport_Cancelled_by_driver_Avg_dist[1:10,], aes(x = pickup_loc, y = Avg_dist)) +
           geom_bar(fill = "#0073C2FF", stat = "identity")+ggtitle("Airport Cancelled by driver Avg_dist"))

Airport_fulfilled_by_driver_Avg_dist <- as.data.frame(dataset %>% 
                                                        filter(status == '4' & drop_loc == 'Airport') %>%
                                                        group_by(pickup_loc) %>% 
                                                        summarise(Avg_dist = mean(travel_distance)) %>%  
                                                        arrange(desc(Avg_dist)))
Airport_fulfilled_by_driver_Avg_dist$pickup_loc <- factor(Airport_fulfilled_by_driver_Avg_dist$pickup_loc, 
                                                          levels = Airport_fulfilled_by_driver_Avg_dist$pickup_loc[order(-Airport_fulfilled_by_driver_Avg_dist$Avg_dist)])
ggplotly(ggplot(Airport_fulfilled_by_driver_Avg_dist[1:10,], aes(x = pickup_loc, y = Avg_dist)) +
           geom_bar(fill = "#0073C2FF", stat = "identity")+ggtitle("Airport fulfilled by driver Avg_dist"))





#5. Airport drop_loc - pickup_loc wise avg Time Taken

Airport_Avg_time <- as.data.frame(dataset %>% 
                                    filter(status == '4' & drop_loc == 'Airport') %>%
                                    group_by(pickup_loc) %>% 
                                    summarise(Avg_time = mean(time_taken)) %>%  
                                    arrange(desc(Avg_time)))
Airport_Avg_time$pickup_loc <- factor(Airport_Avg_time$pickup_loc, 
                                      levels = Airport_Avg_time$pickup_loc[order(-Airport_Avg_time$Avg_time)])

ggplotly(ggplot(Airport_Avg_time[1:10,], aes(x = pickup_loc, y = Avg_time)) +
           geom_bar(fill = "#0073C2FF", stat = "identity"))





#6. Airport drop_loc - Part of the day wise Requests and cancellations

Airport_prt_day_req <- as.data.frame(dataset %>% 
                                       filter(drop_loc == 'Airport') %>%
                                       group_by(Pickup_part_of_day) %>% 
                                       summarise(Frequency = n()) %>%  
                                       arrange(desc(Frequency)))
Airport_prt_day_req$Pickup_part_of_day <- factor(Airport_prt_day_req$Pickup_part_of_day, 
                                                 levels = Airport_prt_day_req$Pickup_part_of_day[order(-Airport_prt_day_req$Frequency)])

ggplotly(ggplot(Airport_prt_day_req[1:10,], aes(x = Pickup_part_of_day, y = Frequency)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("Part of the day wise Requests"))

Airport_prt_day_cancel <- as.data.frame(dataset %>% 
                                          filter(status == '5' & drop_loc == 'Airport' & cancelled_by == 'driver') %>%
                                          group_by(Pickup_part_of_day) %>% 
                                          summarise(Frequency = n()) %>%  
                                          arrange(desc(Frequency)))
Airport_prt_day_cancel$Pickup_part_of_day <- factor(Airport_prt_day_cancel$Pickup_part_of_day, 
                                                    levels = Airport_prt_day_cancel$Pickup_part_of_day[order(-Airport_prt_day_cancel$Frequency)])

ggplotly(ggplot(Airport_prt_day_cancel[1:10,], aes(x = Pickup_part_of_day, y = Frequency)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("Part of the day wise cancellations"))






#6.2 Airport drop_loc - weekday wise cancellations

Airport_prt_week <- as.data.frame(dataset %>% 
                                    filter(status == '5' & drop_loc == 'Airport' & cancelled_by == 'driver') %>%
                                    group_by(Pickup_week) %>% 
                                    summarise(cancelled_pickups_Frequency = n()) %>%  
                                    arrange(desc(cancelled_pickups_Frequency)))
Airport_prt_week$Pickup_week <- factor(Airport_prt_week$Pickup_week, 
                                       levels = Airport_prt_week$Pickup_week[order(-Airport_prt_week$cancelled_pickups_Frequency)])

ggplotly(ggplot(Airport_prt_week[1:10,], aes(x = Pickup_week, y = cancelled_pickups_Frequency)) +
           geom_bar(fill = "#0073C2FF", stat = "identity")+ ggtitle("Weekday wise Airport drop Cancellations comparision"))

Airport_prt_week_1 <- as.data.frame(dataset %>% 
                                      filter(status == '5' & drop_loc == 'Airport' & cancelled_by == 'driver') %>%
                                      group_by(Pickup_week,Pickup_part_of_day) %>% 
                                      summarise(cancelled_pickups_Frequency = n()) %>%  
                                      arrange(desc(cancelled_pickups_Frequency)))

ggplotly(ggplot(Airport_prt_week_1, aes(fill=Pickup_part_of_day, y=cancelled_pickups_Frequency, x=Pickup_week)) + 
           geom_bar(position="dodge", stat="identity")+ ggtitle("Weekday-part_of_day Airport drop Cancellations comparision"))


total_pickups_weekday <- as.data.frame(dataset %>% 
                                         filter(drop_loc == 'Airport') %>%
                                         group_by(Pickup_week) %>% 
                                         summarise(total_pickups_Frequency = n()) %>%  
                                         arrange(desc(total_pickups_Frequency)))
total_fulfilled_pickups_weekday <- as.data.frame(dataset %>% 
                                                   filter(drop_loc == 'Airport' & status == '4') %>%
                                                   group_by(Pickup_week) %>% 
                                                   summarise(fulfilled_pickups_Frequency = n()) %>%  
                                                   arrange(desc(fulfilled_pickups_Frequency)))
total_cancelled_by_driver_pickups_weekday <- Airport_prt_week

aggregated_total <- left_join(total_pickups_weekday,total_fulfilled_pickups_weekday)
aggregated_total <- left_join(aggregated_total,total_cancelled_by_driver_pickups_weekday)

aggregated_total <- melt(aggregated_total, id="Pickup_week")


ggplotly(ggplot(aggregated_total,aes(Pickup_week,value,fill=variable))+
           geom_bar(stat="identity",position="dodge") + ggtitle("Total Airport pickups Vs. fulfilled Vs. Cancelled"))



#6.3. Deep diving in to Afternoon cancellations
Afternoon_cancelling_drivers <- sqldf('select order_id, date, driver_id, Pickup_part_of_day, drop_loc, time_taken, travel_distance, cancelled_by, status
                                       from dataset
                                       where driver_id in 
                                            (select distinct driver_id
                                             from dataset
                                             where drop_loc = "Airport" and status = "5" and cancelled_by = "driver" and driver_id <> "NULL" and 
                                             Pickup_part_of_day = "Afternoon")')

Afternoon_cancelling_drivers_null <- sqldf('select order_id, date, driver_id, Pickup_part_of_day, drop_loc, time_taken, travel_distance, cancelled_by, status
                                            from dataset
                                            where driver_id = "NULL" and 
                                            drop_loc = "Airport" and 
                                            status = "5" and 
                                            cancelled_by = "driver" and 
                                            Pickup_part_of_day = "Afternoon"')

Afternoon_cancelling_drivers_all_orders <- rbind(Afternoon_cancelling_drivers,Afternoon_cancelling_drivers_null)

Afternoon_cancelling_drivers_all_orders$airport_drop_or_not <- ifelse(Afternoon_cancelling_drivers_all_orders$drop_loc == "Airport","Airport_Drop","Non_Airport_Drop")

Afternoon_cancelling_Airport_prt_day <- as.data.frame(Afternoon_cancelling_drivers_all_orders %>% 
                                                        group_by(Pickup_part_of_day) %>% 
                                                        summarise(Frequency = n()) %>%  
                                                        arrange(desc(Frequency)))
Afternoon_cancelling_Airport_prt_day$Pickup_part_of_day <- factor(Afternoon_cancelling_Airport_prt_day$Pickup_part_of_day, 
                                                                  levels = Afternoon_cancelling_Airport_prt_day$Pickup_part_of_day[order(-Afternoon_cancelling_Airport_prt_day$Frequency)])

ggplotly(ggplot(Afternoon_cancelling_Airport_prt_day[1:10,], aes(x = Pickup_part_of_day, y = Frequency)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("Pick up order Frequency for drivers who cancel afternoon Airport Trips"))

Afternoon_cancelling_Airport_prt_day_dist <- as.data.frame(Afternoon_cancelling_drivers_all_orders %>% 
                                                             filter(Pickup_part_of_day == "Afternoon") %>%                         
                                                             group_by(airport_drop_or_not) %>% 
                                                             summarise(Avg_Dist = mean(travel_distance)) %>%  
                                                             arrange(desc(Avg_Dist)))
Afternoon_cancelling_Airport_prt_day_dist$airport_drop_or_not <- factor(Afternoon_cancelling_Airport_prt_day_dist$airport_drop_or_not, 
                                                                        levels = Afternoon_cancelling_Airport_prt_day_dist$airport_drop_or_not[order(-Afternoon_cancelling_Airport_prt_day_dist$Avg_Dist)])

ggplotly(ggplot(Afternoon_cancelling_Airport_prt_day_dist[1:10,], aes(x = airport_drop_or_not, y = Avg_Dist)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("Afternoon - Average Distance Airport Vs. Non Airport Drop"))


Afternoon_cancelling_Airport_prt_day_time <- as.data.frame(Afternoon_cancelling_drivers_all_orders %>% 
                                                             filter(Pickup_part_of_day == "Afternoon" & cancelled_by != "driver") %>%                         
                                                             group_by(airport_drop_or_not) %>% 
                                                             summarise(Avg_time = mean(time_taken)) %>%  
                                                             arrange(desc(Avg_time)))
Afternoon_cancelling_Airport_prt_day_time$airport_drop_or_not <- factor(Afternoon_cancelling_Airport_prt_day_time$airport_drop_or_not, 
                                                                        levels = Afternoon_cancelling_Airport_prt_day_time$airport_drop_or_not[order(-Afternoon_cancelling_Airport_prt_day_time$Avg_time)])

ggplotly(ggplot(Afternoon_cancelling_Airport_prt_day_time[1:10,], aes(x = airport_drop_or_not, y = Avg_time)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("Afternoon - Time Airport Vs. Non Airport Drop"))





#6.4. Deep diving into all the Afternoon cancelling drivers at other parts of the day i.e Morning, Evening and Night
Afternoon_cancelling_drivers <- sqldf('select order_id, date, driver_id, Pickup_part_of_day, drop_loc, time_taken, travel_distance, cancelled_by, status
                                       from dataset
                                       where driver_id in 
                                            (select distinct driver_id
                                             from dataset
                                             where drop_loc = "Airport" and status = "5" and cancelled_by = "driver" and driver_id <> "NULL" and 
                                             Pickup_part_of_day = "Afternoon")')

Afternoon_cancelling_drivers_null <- sqldf('select order_id, date, driver_id, Pickup_part_of_day, drop_loc, time_taken, travel_distance, cancelled_by, status
                                            from dataset
                                            where driver_id = "NULL" and 
                                            drop_loc = "Airport" and 
                                            status = "5" and 
                                            cancelled_by = "driver" and 
                                            Pickup_part_of_day = "Afternoon"')

Afternoon_cancelling_drivers_all_orders <- rbind(Afternoon_cancelling_drivers,Afternoon_cancelling_drivers_null)

Afternoon_cancelling_drivers_all_orders$airport_drop_or_not <- ifelse(Afternoon_cancelling_drivers_all_orders$drop_loc == "Airport","Airport_Drop","Non_Airport_Drop")



Afternoon_cancelling_Airport_prt_day_dist_non_noon <- as.data.frame(Afternoon_cancelling_drivers_all_orders %>% 
                                                                      filter(Pickup_part_of_day != "Afternoon") %>%                         
                                                                      group_by(airport_drop_or_not) %>% 
                                                                      summarise(Avg_Dist = mean(travel_distance)) %>%  
                                                                      arrange(desc(Avg_Dist)))
Afternoon_cancelling_Airport_prt_day_dist_non_noon$airport_drop_or_not <- factor(Afternoon_cancelling_Airport_prt_day_dist_non_noon$airport_drop_or_not, 
                                                                                 levels = Afternoon_cancelling_Airport_prt_day_dist_non_noon$airport_drop_or_not[order(-Afternoon_cancelling_Airport_prt_day_dist_non_noon$Avg_Dist)])

ggplotly(ggplot(Afternoon_cancelling_Airport_prt_day_dist_non_noon[1:10,], aes(x = airport_drop_or_not, y = Avg_Dist)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("non_Afternoon - Average Distance"))


Afternoon_cancelling_Airport_prt_day_time_non_noon <- as.data.frame(Afternoon_cancelling_drivers_all_orders %>% 
                                                                      filter(Pickup_part_of_day != "Afternoon" & cancelled_by != "driver") %>%                         
                                                                      group_by(airport_drop_or_not) %>% 
                                                                      summarise(Avg_time = mean(time_taken)) %>%  
                                                                      arrange(desc(Avg_time)))
Afternoon_cancelling_Airport_prt_day_time_non_noon$airport_drop_or_not <- factor(Afternoon_cancelling_Airport_prt_day_time_non_noon$airport_drop_or_not, 
                                                                                 levels = Afternoon_cancelling_Airport_prt_day_time_non_noon$airport_drop_or_not[order(-Afternoon_cancelling_Airport_prt_day_time_non_noon$Avg_time)])

ggplotly(ggplot(Afternoon_cancelling_Airport_prt_day_time_non_noon[1:10,], aes(x = airport_drop_or_not, y = Avg_time)) +
           geom_bar(fill = "#0073C2FF", stat = "identity") + ggtitle("non_Afternoon - Average Time Taken"))





####################################################### Additional Analysis ####################################################

#7.no.of trips cancelling drivers vs. non cancelling ones to airport
dataset$date <- date(dataset$pickup_time)



C_A_Driver<- sqldf('select order_id, date, driver_id, cancelled_by
                          from dataset
                          where driver_id in (select distinct driver_id
                                              from dataset
                                              where drop_loc = "Airport" and status = "5" and cancelled_by = "driver" and driver_id <> "NULL")')
C_A_null <- sqldf('select order_id, date, driver_id, cancelled_by
                   from dataset
                   where drop_loc = "Airport" and status = "5" and cancelled_by = "driver" and driver_id = "NULL"')
C_A_Driver <- rbind(C_A_Driver,C_A_null)

NC_A_Driver<- sqldf('select order_id, date, driver_id, cancelled_by
                    from dataset
                    where driver_id not in (select distinct driver_id
                                        from dataset
                                        where drop_loc = "Airport" and status = "5" and cancelled_by = "driver" and driver_id <> "NULL")')

cancelled_driver_pickup_orders <- C_A_Driver %>% group_by(date) %>% summarise(Pickup_orders_for_cancelled_drivers = n()/length(unique(driver_id)),)
non_cancelled_driver_pickup_orders <- NC_A_Driver %>% group_by(date) %>% summarise(Pickup_orders_for_non_cancelled_drivers = n()/length(unique(driver_id)))

total_avg_pickup_orders <- left_join(cancelled_driver_pickup_orders,non_cancelled_driver_pickup_orders)
total_avg_pickup_orders <- melt(total_avg_pickup_orders, id="date")

ggplotly(ggplot(data=total_avg_pickup_orders,
                aes(x=date, y=value, colour=variable)) + geom_line() + ggtitle("total_avg_pickup_orders")
         + geom_hline(yintercept=mean(cancelled_driver_pickup_orders$Pickup_orders_for_cancelled_drivers), linetype="dashed", color = "red")
         + geom_hline(yintercept=mean(non_cancelled_driver_pickup_orders$Pickup_orders_for_non_cancelled_drivers), linetype="dashed", color = "blue"))

C_A_Driver_ful<- sqldf('select order_id, date, driver_id
                          from dataset
                          where driver_id in (select distinct driver_id
                                              from dataset
                                              where drop_loc = "Airport" and status = "5" and cancelled_by = "driver" and driver_id <> "NULL") and
                   cancelled_by = "NULL"')
NC_A_Driver_ful<- sqldf('select order_id, date, driver_id
                    from dataset
                    where driver_id not in (select distinct driver_id
                                        from dataset
                                        where drop_loc = "Airport" and status = "5" and cancelled_by = "driver" and driver_id <> "NULL") and
                    cancelled_by = "NULL"')

cancelled_driver_pickup_orders_ful <- C_A_Driver_ful %>% group_by(date) %>% summarise(Pickup_orders_for_cancelled_drivers = n()/length(unique(driver_id)),)
non_cancelled_driver_pickup_orders_ful <- NC_A_Driver_ful %>% group_by(date) %>% summarise(Pickup_orders_for_non_cancelled_drivers = n()/length(unique(driver_id)))

total_fulfilled_avg_pickup_orders <- left_join(cancelled_driver_pickup_orders_ful,non_cancelled_driver_pickup_orders_ful)
total_fulfilled_avg_pickup_orders <- melt(total_fulfilled_avg_pickup_orders, id="date")

ggplotly(ggplot(data=total_fulfilled_avg_pickup_orders,
                aes(x=date, y=value, colour=variable)) + geom_line() + ggtitle("total_fulfilled_avg_pickup_orders")
         + geom_hline(yintercept=mean(cancelled_driver_pickup_orders_ful$Pickup_orders_for_cancelled_drivers), linetype="dashed", color = "red")
         + geom_hline(yintercept=mean(non_cancelled_driver_pickup_orders_ful$Pickup_orders_for_non_cancelled_drivers), linetype="dashed", color = "blue"))



#8. Airport drop_loc - Part of the day wise avg time taken

Airport_prt_day_Avg_time <- as.data.frame(dataset %>% 
                                            filter(drop_loc == 'Airport' & status ==  '4') %>%
                                            group_by(Pickup_part_of_day) %>% 
                                            summarise(Avg_time = mean(time_taken)) %>%  
                                            arrange(desc(Avg_time)))

ggplotly(ggplot(Airport_prt_day_Avg_time[1:10,], aes(x = Pickup_part_of_day, y = Avg_time)) +
           geom_bar(fill = "#0073C2FF", stat = "identity"))
