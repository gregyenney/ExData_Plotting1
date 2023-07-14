library(data.table)
library(dplyr)
library(lubridate)

data_name <- "household_power_consumption"
zip_file_name <- paste0(data_name, ".zip")
data_file_name <- paste0(data_name, ".txt")

rawDataURL <-
  paste0("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2F", 
         zip_file_name)

if(!file.exists(zip_file_name)){
  download.file(rawDataURL, destfile = zip_file_name, method = "curl")
}

if (!file.exists(data_file_name)) {
  unzip(zipfile = zip_file_name, files = data_file_name)
}

hpc_df <- fread(data_file_name, na.strings=c("?"))

hpc_df <- hpc_df %>% mutate( Datetime = 
                             as_datetime( paste(hpc_df$Date, hpc_df$Time),
                                          format = "%d/%m/%Y %H:%M:%S"))

sub_df <- subset(hpc_df, subset=(Date == "1/2/2007" | Date == "2/2/2007"))

# plot 1: histogram of Global Active Power

# draw on the screen
hist(sub_df$Global_active_power, 
     col="red", 
     main="Global Active Power", 
     xlab="Global Active Power (kilowatts)")

# write to png file
png(filename="plot1.png", width=480, height=480)
hist(sub_df$Global_active_power, 
     col="red", 
     main="Global Active Power", 
     xlab="Global Active Power (kilowatts)")
dev.off()
