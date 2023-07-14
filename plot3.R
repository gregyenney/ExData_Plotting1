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


# Create plot 3: line graph of sub metering 1,2 and 2
my_plot <- function(X) {
  
  with(X, {
      plot( Datetime 
           ,Sub_metering_1 
           ,type="l" 
           ,ylab="Energy sub metering"
           ,xlab=""
           ,xaxt="n")
    
      d <- as.POSIXct(round(range(Datetime), "days"))
      axis.POSIXct(1, Datetime, at=seq(d[1], d[2], by="day"), format = "%a")    
    
      lines(  Datetime
             ,Sub_metering_2
             ,type = "l"
             ,col="red")
    

      lines(  Datetime
             ,Sub_metering_3
             ,type = "l"
             ,col="blue")
          
      legend(  "topright"
               ,legend=c(  "Sub_metering_1"
                           ,"Sub_metering_2"
                           ,"Sub_metering_3"
               ) 
               ,lty=c(1,1,1)
               ,lwd=c(1,1,1)
               ,col=c( "black"
                       ,"red"
                       ,"blue"))
      
    }
  )
}

# draw on the screen
my_plot(sub_df)

# write to png file
png(filename="plot3.png", width=480, height=480)
my_plot(sub_df)
dev.off()
