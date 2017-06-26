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

#Plot 1: Global Active Power Histogram
with(power_consumption,
     hist(Global_active_power, col = "red",
          main = "Global Active Power",
          xlab = "Global Active Power (kilowatts)"))

##Save it to a png file
dev.copy(png, "plot1.png")
dev.off()