#Individual household electric power consumption

#Load the data
data_file <- "household_power_consumption.txt"
power_consumption_db <- read.table(file = data_file, sep = ";", 
                                   header = T, stringsAsFactors = F)

#load libraries to clean up the data
library(dplyr); library(tidyr)
library(lubridate)

#convert ? to NA's
power_consumption_db[power_consumption_db == "?"] <- NA

#transform dates

power_consumption_db <- power_consumption_db %>% 
    mutate(Date = dmy(Date),
           Year = year(Date),
           Month = month(Date),
           Day = day(Date))

#filter the data
power_consumption <- power_consumption_db %>% 
    filter(Year == 2007,
           Month == 2,
           Day %in% c(1L, 2L)) %>% 
    select(-Year, -Month)

#Convert variables that were read as characters to numbers
power_consumption[,3:9] <- sapply(power_consumption[,3:9],
                                  as.numeric)

power_consumption$Time <- strptime(paste(power_consumption$Date,
                                         power_consumption$Time),
                                   format = "%Y-%m-%d %H:%M:%S")

#Plot 4: Four plots
par(mfrow = c(2, 2), mar = c(4, 4, 3, 3))

with(power_consumption,{
    plot(Time, Global_active_power, type = "l",
         ylab = "Global Active Power", xlab = "")
    plot(Time, Voltage, type = "l",
         xlab = "datetime", ylab = "Voltage")
    plot(Time, Sub_metering_1, type = "n",
         xlab = "", ylab = "Energy sub metering")
    points(Time, Sub_metering_1, type = "l", col = "black")
    points(Time, Sub_metering_2, type = "l", col = "red")
    points(Time, Sub_metering_3, type = "l", col = "blue")
    legend("topright", col = c("black", "blue", "red"),
           legend = c("Sub_metering_1",
                      "Sub_metering_2",
                      "sub_metering_3"), 
           lwd = 1, cex = 0.43, pt.cex = .8, bty = "n")
    plot(Time, Global_reactive_power, type = "l",
         xlab = "datetime")
})

##Save it to a png file
dev.copy(png, "plot4.png")
dev.off()