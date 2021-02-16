#' Calculate CGM Variables
#'
#' This function takes cleaned CGM data and returns clinically relevant measures
#' (e.g. percent time spent over 140, MAGE, MODD, etc.). 
#' 
#' All files must be saved as a csv, and must have three columns, the first of 
#' which contains the subject ID in the first cell and date of CGM placement in 
#' the second (see example files). The names of the columns must be "subjectid" 
#' "timestamp" and "sensorglucose" (without quotes) respectively. Files can be 
#' cleaned and formatted using this package's cleandata() function.
#' 
#' @param inputdirectory The directory containing cleaned CSV files for
#' analysis.
#' @param outputdirectory The directory where you would like the results
#' spreadsheet to be written.
#' @param outputname The name of the file containing final CGM variables 
#' (without the file extension).
#' @param customintervals A list of custom blood glucose intervals. Minutes and 
#' percent time below the lower bound, in the specified range, and above the 
#' upper bound are calculated for each interval in the list. Number of 
#' excursions below the lower bound and above the upper bound are also 
#' calculated for each interval. 
#' @param aboveexcursionlength The number of minutes blood sugar must be above 
#' threshold to count an excursion.
#' @param belowexcursionlength The number of minutes blood sugar must be below 
#' threshold to count an excursion.
#' @param magedef How large an excursion needs to be in order to count in the 
#' MAGE calculation (e.g. greater than 1 standard deviation).
#' @param congan CONGA interval in hours.
#' @param daystart The numeric hour at which daytime should start (e.g. to start 
#' counting day time at 6:00am, set daystart = 6).
#' @param dayend The numeric hour at which daytime should end (this parameter 
#' uses military time, so to stop counting day time at 10:00pm, set dayend = 22).
#' @param id_filename If true, the file name will be used for subject ID 
#' rather than the ID contained in the data.
#' @param format Whether observations are in rows or columns.
#' @param printname Whether or not to print each file name (for troubleshooting).
#' @usage cgmvariables(inputdirectory,
#' outputdirectory = tempdir(),
#' outputname = "REDCap Upload",
#' customintervals = list(c(180,250),c(250,400)),
#' aboveexcursionlength = 35,
#' belowexcursionlength = 10,
#' magedef = "1sd",
#' congan = 1,
#' daystart = 6,
#' dayend = 22,
#' id_filename = F,
#' format = "rows",
#' printname = F)
#' @examples cgmvariables(system.file("extdata","Cleaned",package = "cgmanalysis"))
#' @return A data frame containing calculated CGM variables, with each column
#' representing one CGM file.
#' @export

cgmvariables <- function(inputdirectory,
                         outputdirectory = tempdir(),
                         outputname = "REDCap Upload",
                         customintervals = list(c(180,250),c(250,400)),
                         aboveexcursionlength = 35,
                         belowexcursionlength = 10,
                         magedef = "1sd",
                         congan = 1,
                         daystart = 6,
                         dayend = 22,
                         id_filename = F,
                         format = "rows",
                         printname = F) {

# Read in data, create results dataframe. The dataframe has one column for each 
# file in the input directory, and is desgined to be uploaded to REDCap. 
  files <- base::list.files(path = inputdirectory,full.names = TRUE)
  cgmupload <- 
    base::as.data.frame(base::matrix(nrow = 0,ncol = base::length(files)))
  base::colnames(cgmupload) <- base::rep("Record",base::length(files))
# Define the order in which lubridate parses dates.  
  dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                      "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                      "Ydm HM","Ydm HMS","ydm HM","ydm HMS")
  allhours <- 0:23
# Iterate through the input directory and calculate CGM variables for each file.
# The cgmvariables() function only works on CSV files that have been cleaned by 
# cleandata(), or that have been manually edited and fit the format of
# cleandata() output. 
  for (f in 1:base::length(files)) {    
# Basic variables
    table <- utils::read.csv(files[f],stringsAsFactors = FALSE,na.strings = c("NA",""))
# Remove duplicates
    if(id_filename == F) {
      table$subjectid <- table$subjectid[1]
    } else {
      table$subjectid[1] <- sub("*.csv","",basename(files[f]))
    }
    table <- unique(table)
# Print name
    if(printname == T) {
      print(basename(files[f]))
    }
# Column names to lower case
    colnames(table) = tolower(colnames(table))
    cgmupload["subject_id",f] <- table$subjectid[1]
# Format columns.    
    table$timestamp <- 
      base::as.POSIXct(lubridate::parse_date_time(table$timestamp,
                                                  dateparseorder,tz = "UTC"))
    table$sensorglucose[table$sensorglucose=="Low"] <- 40
    table$sensorglucose[table$sensorglucose=="High"] <- 400
    table$sensorglucose <- suppressWarnings(base::as.numeric(table$sensorglucose))
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp)))
    interval <- base::abs(interval)
    cgmupload["date_cgm_placement", f] <- 
      base::as.character(min(table$timestamp,na.rm = T))
    
    totaltime <- 
      base::as.numeric(base::difftime(base::max(table$timestamp, na.rm = T),
                                      base::min(table$timestamp,na.rm = T),
                                      units = "secs"))
    cgmupload["percent_cgm_wear",f] <- 
      base::floor(((base::length(which(!is.na(table$sensorglucose)))/(totaltime/interval))*100))
    cgmupload["num_days_good_data",f] <- 
      base::round(base::length(which(!is.na(table$sensorglucose)))/(86400/interval))
    
    table <- table[!is.na(table$timestamp) & !is.na(table$sensorglucose),]
    
    cgmupload["total_sensor_readings",f] <- 
      base::as.numeric(base::length(base::which(!is.na(table$sensorglucose))))
    cgmupload["average_sensor",f] <- 
      base::mean(table$sensorglucose[base::which(!is.na(table$sensorglucose))],na.rm = T)
    cgmupload["estimated_a1c",f] <- 
      base::round((46.7 + (base::mean(table$sensorglucose[
        base::which(!is.na(table$sensorglucose))]))) / 28.7,digits = 1)
    cgmupload["gmi",f] <- base::round(3.31 + (0.02392 * base::mean(table$sensorglucose[
      base::which(!is.na(table$sensorglucose))])), digits = 1)
    cgmupload["q1_sensor",f] <- 
      base::as.numeric(base::summary(table$sensorglucose[
        base::which(!is.na(table$sensorglucose))])[2])
    cgmupload["median_sensor",f] <- 
      base::as.numeric(base::summary(table$sensorglucose[
        base::which(!is.na(table$sensorglucose))])[3])
    cgmupload["q3_sensor",f] <- 
      base::as.numeric(base::summary(table$sensorglucose[
        base::which(!is.na(table$sensorglucose))])[5])
    cgmupload["standard_deviation",f] <- 
      stats::sd(table$sensorglucose[base::which(!is.na(table$sensorglucose))])
    cgmupload["cv",f] <- 
      (stats::sd(table$sensorglucose[base::which(!is.na(table$sensorglucose))]))/
      base::mean(table$sensorglucose[base::which(!is.na(table$sensorglucose))])
    cgmupload["min_sensor",f] <- 
      base::min(table$sensorglucose[base::which(!is.na(table$sensorglucose))])
    cgmupload["max_sensor",f] <- 
      base::max(table$sensorglucose[base::which(!is.na(table$sensorglucose))])
    
# Excursions over 120 calculations.
    BGover120 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover120[BGover120 < 120] <- 0
    BGover120[BGover120 >= 120] <- 1
    BG120.rle <- base::rle(BGover120)
    excursions120 <- 
      base::as.numeric(BG120.rle$lengths[base::which(BG120.rle$values == 1)])
    
    cgmupload["excursions_over_120",f] <- 
      base::length(base::which(excursions120 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_120",f] <- base::sum(BGover120) * (interval/60)
    cgmupload["percent_time_over_120",f] <- 
      ((base::sum(BGover120) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
# Over 140.
    BGover140 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover140[BGover140 < 140] <- 0
    BGover140[BGover140 >= 140] <- 1
    BG140.rle <- base::rle(BGover140)
    excursions140 <- 
      base::as.numeric(BG140.rle$lengths[base::which(BG140.rle$values == 1)])
    
    cgmupload["excursions_over_140",f] <- 
      base::length(base::which(excursions140 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_140",f] <- base::sum(BGover140) * (interval/60)
    cgmupload["percent_time_over_140",f] <- 
      ((base::sum(BGover140) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
# Over 180.
    BGover180 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover180[BGover180 < 180] <- 0
    BGover180[BGover180 >= 180] <- 1
    BG180.rle <- base::rle(BGover180)
    excursions180 <- 
      base::as.numeric(BG180.rle$lengths[base::which(BG180.rle$values == 1)])
    
    cgmupload["excursions_over_180",f] <- 
      base::length(base::which(excursions180 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_180",f] <- base::sum(BGover180) * (interval/60)
    cgmupload["percent_time_over_180",f] <- 
      ((base::sum(BGover180) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
# Over 200.
    BGover200 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover200[BGover200 < 200] <- 0
    BGover200[BGover200 >= 200] <- 1
    BG200.rle <- base::rle(BGover200)
    excursions200 <- 
      base::as.numeric(BG200.rle$lengths[base::which(BG200.rle$values == 1)])
    
    cgmupload["excursions_over_200",f] <- 
      base::length(base::which(excursions200 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_200",f] <- base::sum(BGover200) * (interval/60)
    cgmupload["percent_time_over_200",f] <- 
      ((base::sum(BGover200) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100

    cgmupload["avg_excur_over_140_per_day",f] <- 
      as.numeric(cgmupload["excursions_over_140",f])/
      as.numeric(cgmupload["num_days_good_data",f])
    cgmupload["avg_excur_over_200_per_day",f] <- 
      as.numeric(cgmupload["excursions_over_200",f])/
      as.numeric(cgmupload["num_days_good_data",f])
        
# Over 250.
    BGover250 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover250[BGover250 < 250] <- 0
    BGover250[BGover250 >= 250] <- 1
    BG250.rle <- base::rle(BGover250)
    excursions250 <- 
      base::as.numeric(BG250.rle$lengths[base::which(BG250.rle$values == 1)])
    
    cgmupload["excursions_over_250",f] <- 
      base::length(base::which(excursions250 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_250",f] <- base::sum(BGover250) * (interval/60)
    cgmupload["percent_time_over_250",f] <- 
      ((base::sum(BGover250) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
# Under 54.
    BGunder54 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGunder54[BGunder54 <= 54] <- 1
    BGunder54[BGunder54 > 54] <- 0
    BG54.rle <- base::rle(BGunder54)
    excursions54 <- 
      base::as.numeric(BG54.rle$lengths[base::which(BG54.rle$values == 1)])
    
    cgmupload["excursions_under_54",f] <- 
      base::length(base::which(excursions54 > 
                                 ((belowexcursionlength * 60)/interval)))
    cgmupload["min_spent_under_54",f] <- base::sum(BGunder54) * (interval/60)
    cgmupload["percent_time_under_54",f] <- 
      ((base::sum(BGunder54) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
# Under 60.
    BGunder60 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGunder60[BGunder60 <= 60] <- 1
    BGunder60[BGunder60 > 60] <- 0
    BG60.rle <- base::rle(BGunder60)
    excursions60 <- 
      base::as.numeric(BG60.rle$lengths[base::which(BG60.rle$values == 1)])
    
    cgmupload["excursions_under_60",f] <- 
      base::length(base::which(excursions60 > 
                                 ((belowexcursionlength * 60)/interval)))
    cgmupload["min_spent_under_60",f] <- base::sum(BGunder60) * (interval/60)
    cgmupload["percent_time_under_60",f] <- 
      ((base::sum(BGunder60) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
# Under 70.
    BGunder70 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))],
                 length = 1)
    BGunder70[BGunder70 <= 70] <- 1
    BGunder70[BGunder70 > 70] <- 0
    BG70.rle <- base::rle(BGunder70)
    excursions70 <- 
      base::as.numeric(BG70.rle$lengths[base::which(BG70.rle$values == 1)])
    
    cgmupload["excursions_under_70",f] <- 
      base::length(base::which(excursions70 > 
                                 ((belowexcursionlength * 60)/interval)))
    cgmupload["min_spent_under_70",f] <- base::sum(BGunder70) * (interval/60)
    cgmupload["percent_time_under_70",f] <- 
      ((base::sum(BGunder70) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
# Time in range.
    BGinrange <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))],
                       length = 1)
    BGinrange <- ifelse(BGinrange %in% 70:180, 1,0)
    cgmupload["min_spent_70_180",f] <- base::sum(BGinrange) * (interval/60)
    cgmupload["percent_time_70_180",f] <- 
      ((base::sum(BGinrange) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
# Custom intervals
    if(!is.null(customintervals[[1]])) {
      lows <- unlist(lapply(customintervals, '[[', 1))
      highs <- unlist(lapply(customintervals, '[[', 2))
      for (r in 1:length(customintervals)) {
        # Range
        BGinrange <- base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))],
                                  length = 1)
        BGinrange <- ifelse(BGinrange %in% lows[r]:highs[r], 1,0)
        minname <- paste0("min_spent_",lows[r],"_",highs[r])
        cgmupload[minname,f] <- base::sum(BGinrange) * (interval/60)
        percname <- paste0("percent_time_",lows[r],"_",highs[r])
        cgmupload[percname,f] <- 
          ((base::sum(BGinrange) * (interval/60))/
             (base::length(table$sensorglucose) * (interval/60))) * 100
        # Below
        BGunder <- 
          base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))],
                           length = 1)
        BGunder[BGunder <= lows[r]] <- 1
        BGunder[BGunder > lows[r]] <- 0
        BGunder.rle <- base::rle(BGunder)
        excursionsunder <- 
          base::as.numeric(BGunder.rle$lengths[base::which(BGunder.rle$values == 1)])
        
        cgmupload[paste0("excursions_under_",lows[r]),f] <- 
          base::length(base::which(excursionsunder > 
                                     ((belowexcursionlength * 60)/interval)))
        cgmupload[paste0("min_spent_under_",lows[r]),f] <- base::sum(BGunder) * (interval/60)
        cgmupload[paste0("percent_time_under_",lows[r]),f] <- 
          ((base::sum(BGunder) * (interval/60))/
             (base::length(table$sensorglucose) * (interval/60))) * 100
        # Above
        BGover <- 
          base::as.numeric(table$sensorglucose[base::which(!is.na(
            table$sensorglucose))],length = 1)
        BGover[BGover < highs[r]] <- 0
        BGover[BGover >= highs[r]] <- 1
        BGover.rle <- base::rle(BGover)
        excursionsover <- 
          base::as.numeric(BGover.rle$lengths[base::which(BGover.rle$values == 1)])
        
        cgmupload[paste0("excursions_over_",highs[r]),f] <- 
          base::length(base::which(excursionsover > 
                                     ((aboveexcursionlength * 60)/interval)))
        cgmupload[paste0("min_spent_over_",highs[r]),f] <- base::sum(BGover) * (interval/60)
        cgmupload[paste0("percent_time_over_",highs[r]),f] <- 
          ((base::sum(BGover) * (interval/60))/
             (base::length(table$sensorglucose) * (interval/60))) * 100
      }
    }
# Find daytime AUC.
    if ("wake" %in% colnames(table)) {
      daytime_indexes <- 
        base::which(table$wake == 1)
    } else {
      daytime_indexes <- 
        base::which(base::as.numeric(base::format(table$timestamp,"%H")) %in% 
                      daystart:dayend)
    }
    daytime_sensor <- table$sensorglucose[daytime_indexes]
    xaxis <- 
      base::seq(from = 0, length.out = base::length(daytime_sensor),by = 
                  (interval / 60))
    
# Remove NAs if they are present.
    xaxis[base::which(is.na(daytime_sensor))] <- NA
    xaxis <- xaxis[!is.na(xaxis)]
    daytime_sensor <- daytime_sensor[!is.na(daytime_sensor)]
    aucs <- pracma::cumtrapz(xaxis,daytime_sensor)
    cgmupload["daytime_auc",f] <- aucs[base::length(daytime_sensor)]
    
# TIR variables for daytime
    BGinrange <- ifelse(daytime_sensor %in% 70:180, 1,0)
    cgmupload["min_spent_70_180_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_70_180_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor < 54, 1,0)
    cgmupload["min_spent_under_54_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_under_54_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor < 60, 1,0)
    cgmupload["min_spent_under_60_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_under_60_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor < 70, 1,0)
    cgmupload["min_spent_under_70_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_under_70_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor > 180, 1,0)
    cgmupload["min_spent_over_180_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_over_180_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor > 200, 1,0)
    cgmupload["min_spent_over_200_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_over_200_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor > 250, 1,0)
    cgmupload["min_spent_over_250_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_over_250_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
# Other daytime sensor glucose variables.
    cgmupload["daytime_avg_sensor_glucose",f] <- 
      base::mean(stats::na.omit(daytime_sensor))
    cgmupload["daytime_min_sensor_glucose",f] <- base::min(daytime_sensor)
    cgmupload["daytime_max_sensor_glucose",f] <- base::max(daytime_sensor)
    cgmupload["daytime_sd",f] <- stats::sd(daytime_sensor)

# Nighttime AUC.
    if ("wake" %in% colnames(table)) {
      nighttime_indexes <- base::which(table$wake == 0)
    } else {
      nighttime_indexes <- 
        base::which(base::as.numeric(base::format(table$timestamp,"%H")) %in% 
                      allhours[base::which(!(0:23 %in% daystart:dayend))])
    }
    if (length(nighttime_indexes) > 0) {
      nighttime_sensor <- table$sensorglucose[nighttime_indexes]
      xaxis <- 
        base::seq(from = 0, length.out = base::length(nighttime_indexes),by = 
                    (interval / 60))
      
      # Day/night ratio.
      cgmupload["day_night_sensor_ratio",f] <- 
        base::round(base::length(daytime_sensor)/base::length(nighttime_sensor),1)
      
      # Remove NAs if they are present.
      xaxis[base::which(is.na(nighttime_sensor))] <- NA
      xaxis <- xaxis[!is.na(xaxis)]
      nighttime_sensor <- nighttime_sensor[!is.na(nighttime_sensor)]
      aucs <- pracma::cumtrapz(xaxis,nighttime_sensor)
      cgmupload["nighttime_auc",f] <- aucs[base::length(nighttime_sensor)]
      
      # TIR variables for nighttime
      BGinrange <- ifelse(nighttime_sensor %in% 70:180, 1,0)
      cgmupload["min_spent_70_180_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
      cgmupload["percent_time_70_180_night",f] <- 
        (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
      
      BGinrange <- ifelse(nighttime_sensor < 54, 1,0)
      cgmupload["min_spent_under_54_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
      cgmupload["percent_time_under_54_night",f] <- 
        (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
      
      BGinrange <- ifelse(nighttime_sensor < 60, 1,0)
      cgmupload["min_spent_under_60_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
      cgmupload["percent_time_under_60_night",f] <- 
        (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
      
      BGinrange <- ifelse(nighttime_sensor < 70, 1,0)
      cgmupload["min_spent_under_70_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
      cgmupload["percent_time_under_70_night",f] <- 
        (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
      
      BGinrange <- ifelse(nighttime_sensor > 180, 1,0)
      cgmupload["min_spent_over_180_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
      cgmupload["percent_time_over_180_night",f] <- 
        (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
      
      BGinrange <- ifelse(nighttime_sensor > 200, 1,0)
      cgmupload["min_spent_over_200_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
      cgmupload["percent_time_over_200_night",f] <- 
        (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
      
      BGinrange <- ifelse(nighttime_sensor > 250, 1,0)
      cgmupload["min_spent_over_250_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
      cgmupload["percent_time_over_250_night",f] <- 
        (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100    
      
      # Other nighttime sensor glucose variables.
      cgmupload["nighttime_avg_sens_glucose",f] <- 
        base::mean(stats::na.omit(nighttime_sensor))
      cgmupload["nighttime_min_sens_glucose",f] <- base::min(nighttime_sensor)
      cgmupload["nighttime_max_sens_glucose",f] <- base::max(nighttime_sensor)
      cgmupload["nighttime_sd",f] <- stats::sd(nighttime_sensor)
    }
    
# Total AUC.
    sensorBG <- base::as.numeric(table$sensorglucose,length = 1)
    xaxis <- 
      base::seq(from = 0, length.out = base::length(sensorBG),by = 
                  (interval / 60))
    
# Remove NAs if they are present.
    xaxis[base::which(is.na(sensorBG))] <- NA
    xaxis <- xaxis[!is.na(xaxis)]
    sensorBG <- sensorBG[!is.na(sensorBG)]
    aucs <- pracma::cumtrapz(xaxis,sensorBG)
    cgmupload["total_auc",f] <- aucs[base::length(sensorBG)]
    
    cgmupload["average_auc_per_day",f] <-
      base::as.numeric(cgmupload["total_auc",f]) /
      base::as.numeric(cgmupload["num_days_good_data",f])
    
# AUC over 180.
    sensorover180 <- table$sensorglucose
    sensorover180 <- sensorover180[sensorover180 >= 180]
    sensorover180 <- sensorover180[!is.na(sensorover180)]
    xaxis <- 
      base::seq(from = 0, length.out = base::length(sensorover180),by = 
                  (interval / 60))
    
# Calculate cumulative AUC, and subtract recatangle where length = 180 &
# width = minutes.
    if (base::length(sensorover180) > 1) {
      aucs <- pracma::cumtrapz(xaxis,sensorover180)
      aucs <- 
        (aucs[base::length(sensorover180)]) - (xaxis[base::length(xaxis)] * 180)
      cgmupload["auc_over_180",f] <- aucs
    } else {
      cgmupload["auc_over_180",f] <- 0
    }
    cgmupload["average_auc_180",f] <-
      base::as.numeric(cgmupload["auc_over_180",f]) /
      base::as.numeric(cgmupload["num_days_good_data",f])
    
# Calculate MAGE.
# Smooth data using an exponentially weighted moving average, calculate SD of 
# unsmoothed data.  
    table$smoothed <- 
      base::as.numeric(zoo::rollapply(zoo::zoo(table$sensorglucose), 9, 
                                      function(x) c(1,2,4,8,16,8,4,2,1) %*% 
                                        (x / 46),fill = NA))
    table$smoothed[1:4] <- base::mean(stats::na.omit(table$sensorglucose[1:4]))
    table$smoothed[(base::length(table$smoothed)-3):
                     base::length(table$smoothed)] <- 
      base::mean(table$sensorglucose[(base::length(table$sensorglucose)-3):
                                       base::length(table$sensorglucose)])
    
    sd <- stats::sd(table$sensorglucose)
# Identify turning points, peaks, and nadirs.
    tpoints <- pastecs::turnpoints(table$smoothed)
    peaks <- tpoints$pos[tpoints$peaks]
    pits <- tpoints$pos[tpoints$pits]
# Calculate the difference between each nadir and its following peak. If the     
# data starts on a peak, remove it. Otherwise remove the final pit to create an 
# even number of pits and peaks.
    if (tpoints[["firstispeak"]] == TRUE && base::length(peaks) != 
        base::length(pits)) {
      peaks <- peaks[2:base::length(peaks)]
    } else if (tpoints[["firstispeak"]] == FALSE && 
               base::length(peaks) != base::length(pits)) {
      pits <- pits[1:(base::length(pits)-1)]
    }
    differences <- table$sensorglucose[peaks] - table$sensorglucose[pits]
    
# Calculate the average of the differences greater than the entire dataset 
# SD, 2SD, etc.
    if (magedef == "1sd") {
      cgmupload["r_mage",f] <- 
        base::mean(stats::na.omit(differences[base::which(differences > sd)]))
    } else if (magedef == "1.5sd") {
      cgmupload["r_mage",f] <- 
        base::mean(stats::na.omit(differences[base::which(differences > 
                                                            (sd * 1.5))]))
    } else if ( magedef == "2sd") {
      cgmupload["r_mage",f] <- 
        base::mean(stats::na.omit(differences[base::which(differences > 
                                                            (sd * 2))]))
    } else {
      cgmupload["r_mage",f] <- 
        base::mean(stats::na.omit(differences[base::which(differences > 
                                                            magedef)]))
    }
    
#J-index
    cgmupload["j_index",f] <- 
      0.001 * (base::mean(table$sensorglucose, na.rm = T) + 
                 stats::sd(table$sensorglucose, na.rm = T))^2
# CONGA    
    n <- (congan * 3600)
    conga.times <- table$timestamp + n
    conga.times <- conga.times[!is.na(conga.times)]
    conga.times <- conga.times[base::order(conga.times)]
    conga.times <- conga.times[base::which(conga.times %in% table$timestamp)]
    begin.times <- conga.times - n
    suppressWarnings(congas <- table$sensorglucose[base::which(table$timestamp %in% conga.times)] - 
      table$sensorglucose[base::which(table$timestamp %in% begin.times)])
    cgmupload[base::paste0("conga_",congan),f] <- stats::sd(congas,na.rm = T)
# MODD.
    table$time <- lubridate::round_date(table$timestamp,"5 minutes")
    table$time <- base::strftime(table$time, format = "%H:%M",tz = "UTC")
    moddtable <- 
      base::data.frame(base::matrix(ncol = 2,nrow = 
                                      base::length(unique(table$time))))
    base::colnames(moddtable) <- c("time","mean_differences")
    moddtable$time <- base::unique(table$time)
# For each time, calculate differences (absolute values) and average them.   
    for (r in 1:nrow(moddtable)) {
      moddtable$mean_differences[r] <- 
        base::mean(base::abs(base::diff(table$sensorglucose[
          base::which(table$time == moddtable$time[r])])))
    }
# Average the averages.
    cgmupload["modd",f] <- 
      base::mean(stats::na.omit(moddtable$mean_differences))
    
# LBGI and HBGI (based on dc1386 appendix)
    a <- 1.084
    b <- 5.381
    y <- 1.509
    table$gluctransform <- y * ((base::log(table$sensorglucose)^a)-b)
    table$rBG <- 10 * (table$gluctransform^2)
    rl <- table$rBG[base::which(table$gluctransform < 0)]
    rh <- table$rBG[base::which(table$gluctransform > 0)]
    cgmupload["lbgi",f] <- base::mean(stats::na.omit(rl))
    cgmupload["hbgi",f] <- base::mean(stats::na.omit(rh))
  }
  
# Write file.
  cgmupload <- 
    base::cbind("Variable / Field Name" = rownames(cgmupload),cgmupload)
  if (format == "rows") {
    cgmupload <- base::as.data.frame(base::t(cgmupload))
    cgmupload <- cgmupload[-1,]
  }
  filename <- base::paste(outputdirectory,"/",outputname,".csv",sep = "")
  utils::write.csv(cgmupload, file = filename,row.names = FALSE,na = "")
}
