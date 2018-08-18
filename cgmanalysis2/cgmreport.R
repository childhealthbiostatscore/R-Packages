#' Generate AGP
#' 
#' This function takes a directory of cleaned CGM data and generates two 
#' aggregate AGPs for all of the files combined. One aggregate AGP is produced 
#' using Tukey smoothing, and the other uses ggplot's built in LOESS smoothing 
#' function. The function also produces an AGP plot with each unique subject 
#' plotted individually on the same graph (using LOESS smoothing).
#' 
#' @param inputdirectory The directory containing all cleaned, formatted CGM 
#' data to be analyzed.
#' @param outputdirectory The directory where plot PDF files should be written.
#' @import ggplot2,lubridate
#' @usage 
#' @examples 
#' @return 
#' @export

cgmreport <- function(inputdirectory,
                      outputdirectory = tempdir()){
  
# Set locale to read all characters.    
  Sys.setlocale('LC_ALL','C')
  
# Get file list.  
  files <- base::list.files(path = inputdirectory,full.names = TRUE)

# Create data frame to store all glucose values from all files (for aggregate 
# AGP). 
  aggregateAGPdata <- base::data.frame(matrix(ncol = 3,nrow = 0))
  colnames(aggregateAGPdata) <- c("subjectid","time","sensorglucose")

# Iterate through directory, combine all data.   
  for (f in 1:length(files)) {
    cgmdata <- read.csv(files[f],stringsAsFactors = FALSE,header = TRUE,skipNul = TRUE)
    id <- cgmdata$subjectid[1]
    cgmdata$subjectid <- id
    aggregateAGPdata <- rbind(cgmdata,aggregateAGPdata)
  }
  aggregateAGPdata$sensorglucose <- as.numeric(aggregateAGPdata$sensorglucose)
  
# Remove missing data.  
  aggregateAGPdata <- aggregateAGPdata[complete.cases(aggregateAGPdata),]
  
# Remove dates, so aggregate AGP data can be sorted by time of day.    
  dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                      "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                      "Ydm HM","Ydm HMS","ydm HM","ydm HMS")
  aggregateAGPdata$timestamp <- as.POSIXct(parse_date_time(aggregateAGPdata$timestamp,
                                                           dateparseorder,tz = "UTC"))
  aggregateAGPdata$hour <- round_date(aggregateAGPdata$timestamp,"hour")
  aggregateAGPdata$time <- as.POSIXlt(strftime(aggregateAGPdata$timestamp,format = "%H:%M"),format = "%H:%M")
  aggregateAGPdata$hourmin <- round_date(aggregateAGPdata$timestamp,"10 minutes")
  aggregateAGPdata$hourmin <- as.POSIXlt(strftime(aggregateAGPdata$hourmin,format = "%H:%M"),format = "%H:%M")
 
# Find 25%ile, median, and 75%ile blood glucose for each hour, store in new table.
  quartiles <- data.frame(matrix(nrow = length(unique(aggregateAGPdata$hourmin)),ncol = 6))
  colnames(quartiles) <- c("hourmin","sensorglucose5perc","sensorglucoseqone","sensorglucosemedian","sensorglucoseqthree","sensorglucose95perc")
  quartiles$hourmin <- unique(aggregateAGPdata$hourmin)
  quartiles <- quartiles[order(quartiles$hourmin),]
  
  for (i in 1:nrow(quartiles)) {
    quartiles$sensorglucose5perc[i] <- quantile(as.numeric(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])]),0.05)
    quartiles$sensorglucoseqone[i] <- as.numeric(summary(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])])[2])
    quartiles$sensorglucosemedian[i] <- as.numeric(summary(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])])[3])
    quartiles$sensorglucoseqthree[i] <- as.numeric(summary(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])])[5])
    quartiles$sensorglucose95perc[i] <- quantile(as.numeric(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])]),0.95)
  }
  
  quartiles$smooth5perc <- as.numeric(smooth(quartiles$sensorglucose5perc,kind = "3R",twiceit = TRUE))
  quartiles$smoothqone <- as.numeric(smooth(quartiles$sensorglucoseqone,kind = "3R",twiceit = TRUE))
  quartiles$smoothmed <- as.numeric(smooth(quartiles$sensorglucosemedian,kind = "3R",twiceit = TRUE))
  quartiles$smoothqthree <- as.numeric(smooth(quartiles$sensorglucoseqthree,kind = "3R",twiceit = TRUE))
  quartiles$smooth95perc <- as.numeric(smooth(quartiles$sensorglucose95perc,kind = "3R",twiceit = TRUE))

# Plots
  aggAGPtukey <- ggplot(quartiles, aes(x = quartiles$hourmin))+ 
    geom_ribbon(aes(ymin = quartiles$smoothqone,ymax = quartiles$smoothqthree,fill = "Interquartile Range"),alpha = 0.5)+
    geom_line(aes(y = quartiles$smoothmed, color = "Median"))+
    geom_line(aes(y = quartiles$smooth95perc,linetype="5th & 95th Percentile"))+
    geom_line(aes(y = quartiles$smooth5perc),linetype="dashed")+
    ggtitle("Aggregate Daily Overlay (Tukey Smoothing)")+
    ylab("Sensor BG (mg/dL)")+
    xlab("Time (hour)")+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_x_datetime(labels = function(x) format(x, format = "%H:%M"))+
    scale_fill_manual("",values = "blue")+
    scale_color_manual("",values = "red")+
    scale_linetype_manual("",values = "dashed")
  
  AGPloess <- ggplot(aggregateAGPdata, aes(x = aggregateAGPdata$time, y = aggregateAGPdata$sensorglucose))+
    geom_smooth(aes(y = aggregateAGPdata$sensorglucose,color = aggregateAGPdata$subjectid),se = FALSE)+
    geom_point(aes(y = aggregateAGPdata$sensorglucose, color = aggregateAGPdata$subjectid),shape = ".",alpha = 0.3)+
    ggtitle("Daily Overlay Per Subject (LOESS Smoothing)")+
    ylab("Sensor BG (mg/dL)")+
    xlab("Time (hour)")+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(colour = "Subject ID")+
    scale_x_datetime(labels = function(x) format(x, format = "%H:%M"))
  
  aggAGPloess <- ggplot(aggregateAGPdata, aes(x = aggregateAGPdata$time, y = aggregateAGPdata$sensorglucose))+
    geom_smooth(aes(y = aggregateAGPdata$sensorglucose), se = FALSE)+
    geom_point(aes(y = aggregateAGPdata$sensorglucose),shape = ".")+
    ggtitle("Aggregate Daily Overlay (LOESS Smoothing)")+
    ylab("Sensor BG (mg/dL)")+
    xlab("Time (hour)")+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_x_datetime(labels = function(x) format(x, format = "%H:%M"))

  pdf(base::paste(outputdirectory,"/","AGP_Tukey.pdf",sep = ""),
      width = 11,height = 8.5)
  plot(aggAGPtukey)
  dev.off()
  
  pdf(base::paste(outputdirectory,"/","AGP_Loess_Subject.pdf",sep = ""),
      width = 11,height = 8.5)
  plot(AGPloess)
  dev.off()
  
  pdf(base::paste(outputdirectory,"/","Aggregate_AGP_Loess.pdf",sep = ""),
      width = 11,height = 8.5)
  plot(aggAGPloess)
  dev.off()
}