#' Calculate CGM Variables
#'
#' This function takes cleaned CGM data and returns clinically relevant measures
#' (e.g. percent time spent over 140). Some variables, like number of glucometer
#' tests and date of placement will need to be filled in manually in REDCap.
#' For post-meal glucoses, meals must be determined and entered manually based
#' on participant food log.
#' @param inputdirectory The directory containing cleaned CSV files for
#' analysis.
#' @param outputdirectory The directory where you would like the results
#' spreadsheet to be written.
#' @param outputname The name of the file containing final CGM variables.
#' @param cgmtype The brand of CGM you are analyzing, either "iPro" or "Dexcom."
#' @import pracma
#' @usage
#' cgmvariables(inputdirectory = "Cleaned CSVs",
#'              outputdirectory = tempdir(),
#'              outputname = "REDCap Upload",
#'              cgmtype = "iPro")
#' @examples
#' cgmvariables(system.file("extdata","Cleaned_iPro_CSVs",
#'                          package = "cgmanalysis"))
#' cgmvariables(system.file("extdata","Cleaned_Dexcom_CSVs",
#'                          package = "cgmanalysis"),cgmtype = "Dexcom")
#' @return a data frame containing calculated CGM variables, with each column
#' representing one CGM file.
#' @export

cgmvariables <- function(inputdirectory = "Cleaned CSVs",
                         outputdirectory = tempdir(),
                         outputname = "REDCap Upload",
                         cgmtype = "iPro") {

  files <- list.files(path = inputdirectory,full.names = TRUE)
  cgmupload <- data.frame(matrix(nrow = length(rownames),ncol = length(files)))
  colnames(cgmupload) <- rep("Record",length(files))

  for (f in 1:length(files)) {

# Read in data, format time columns.
    if (cgmtype == "iPro"){
      table <- utils::read.csv(files[f], stringsAsFactors = FALSE)
      table$Time <- gsub(table$Time, pattern = ".00",replacement = "",
                         fixed = TRUE)
      table$Time <- strptime(table$Time,format = "%H:%M:%S")
      table$Time <- strftime(table$Time)
      table$Time <- sub(".* ", "",table$Time)
      table$Timestamp <- gsub(pattern = ".00",replacement = "",table$Timestamp,
                              fixed = TRUE)
      cgmupload["subject_id",f] <- paste(table$Patient.ID[5],"--1",sep = "")
    } else if (cgmtype == "Dexcom") {
      table <- utils::read.csv(files[f], stringsAsFactors = FALSE)
      table <- table[, 1:13]
      colnames(table) <- c("PatientInfoField","PatientInfoValue",
                           "GlucoseInternalTime","DateTime",
                           "Sensor.Glucose..mg.dL.","MeterInternalTime",
                           "MeterDisplayTime","MeterValue",
                           "EventLoggedInternalTime","EventLoggedDisplayTime",
                           "EventTime","EventType","EventDescription")
      table$Time <- NA
      table$Time <- substring(table$DateTime,12)
      table$Timestamp <- NA
      table$Timestamp <- as.POSIXct(table$Time, format = "%H:%M:%S")
      id <- strsplit(files[f],split = c("/"))[[1]][2]
      id <- strsplit(id,split = c("_"))[[1]][1]
      id <- gsub(".*/","",id)
      cgmupload["subject_id",f] <- id
    }

# Basic variables.
    table$Sensor.Glucose..mg.dL. <- as.numeric(table$Sensor.Glucose..mg.dL.)
    cgmupload["date_cgm_placement", f] <- ""
    cgmupload["cgm_data_success_failure", f] <- ""
    cgmupload["insulin_post_study", f] <- ""
    cgmupload["average_num_gmeter_tests", f] <- ""
    cgmupload["total_number_of_glucometer", f] <- ""
    cgmupload["num_days_cgm_wear",f] <-
      length(which(!is.na(table$Sensor.Glucose..mg.dL.)))/288
    cgmupload["num_days_good_data",f] <- cgmupload["num_days_cgm_wear",f]
    cgmupload["total_sensor_readings",f] <-
      as.numeric(length(which(!is.na(table$Sensor.Glucose..mg.dL.))))
    cgmupload["average_sensor",f] <-
      mean(table$Sensor.Glucose..mg.dL.[which
                                        (!is.na(table$Sensor.Glucose..mg.dL.))])
    cgmupload["q1_sensor",f] <-
      as.numeric(summary(table$Sensor.Glucose..mg.dL.[which(
        !is.na(table$Sensor.Glucose..mg.dL.))])[2])

    cgmupload["median_sensor",f] <-
      as.numeric(summary(table$Sensor.Glucose..mg.dL.[which(
        !is.na(table$Sensor.Glucose..mg.dL.))])[3])

    cgmupload["q3_sensor",f] <-
      as.numeric(summary(table$Sensor.Glucose..mg.dL.[which(
        !is.na(table$Sensor.Glucose..mg.dL.))])[5])

    cgmupload["standard_deviation",f] <-
      stats::sd(table$Sensor.Glucose..mg.dL.[which
                                      (!is.na(table$Sensor.Glucose..mg.dL.))])
    cgmupload["min_sensor",f] <-
      min(table$Sensor.Glucose..mg.dL.[which
                                       (!is.na(table$Sensor.Glucose..mg.dL.))])
    cgmupload["max_sensor",f] <-
      max(table$Sensor.Glucose..mg.dL.[which
                                       (!is.na(table$Sensor.Glucose..mg.dL.))])

# Excursions over 120 calculations.
    BGover120 <-
      as.numeric(table$Sensor.Glucose..mg.dL.
                 [which(!is.na(table$Sensor.Glucose..mg.dL.))],length = 1)
    BGover120[BGover120 < 120] <- NA
    peaks <- 0
    notna <- 0
    for (i in 1:length(BGover120)) {
      if (is.na(BGover120[i]) && notna == 0) {
        next()
      } else if (!is.na(BGover120[i]) && notna == 0 && !is.na(BGover120[i+1])) {
        notna <- 1
        peaks <- peaks + 1
      } else if (!is.na(BGover120[i]) && notna == 1) {
        next()
      } else if (is.na(BGover120[i]) && notna == 1) {
        notna <- 0
      }
    }
    cgmupload["excursions_over_120",f] <- as.numeric(peaks)
    cgmupload["min_spent_over_120",f] <-
      as.numeric((length(BGover120[which(!is.na(BGover120))]) - peaks) * 5)
    cgmupload["percent_time_over_120",f] <-
      (as.numeric(cgmupload["min_spent_over_120",f]) /
         (as.numeric(cgmupload["total_sensor_readings",f])*5)) * 100

# Over 140.
    BGover140 <- as.numeric(table$Sensor.Glucose..mg.dL.[
      which(!is.na(table$Sensor.Glucose..mg.dL.))],length = 1)
    BGover140[BGover140 < 140] <- NA
    peaks <- 0
    notna <- 0
    for (i in 1:length(BGover140)) {
      if (is.na(BGover140[i]) && notna == 0) {
        next()
      } else if (!is.na(BGover140[i]) && notna == 0 && !is.na(BGover140[i+1])) {
        notna <- 1
        peaks <- peaks + 1
      } else if (!is.na(BGover140[i]) && notna == 1) {
        next()
      } else if (is.na(BGover140[i]) && notna == 1) {
        notna <- 0
      }
    }
    cgmupload["excursions_over_140",f] <- as.numeric(peaks)
    cgmupload["min_spent_over_140",f] <-
      as.numeric((length(BGover140[which(!is.na(BGover140))]) - peaks) * 5)
    cgmupload["percent_time_over_140",f] <-
      (as.numeric(cgmupload["min_spent_over_140",f]) /
         (as.numeric(cgmupload["total_sensor_readings",f])*5)) * 100

    cgmupload["avg_excur_over_140_per_day",f] <- as.numeric(cgmupload[
      "excursions_over_140",f])/as.numeric(cgmupload["num_days_cgm_wear",f])

# Over 200.
    BGover200 <- as.numeric(table$Sensor.Glucose..mg.dL.[
      which(!is.na(table$Sensor.Glucose..mg.dL.))],length = 1)
    BGover200[BGover200 < 200] <- NA
    peaks <- 0
    notna <- 0
    for (i in 1:length(BGover200)) {
      if (is.na(BGover200[i]) && notna == 0) {
        next()
      } else if (!is.na(BGover200[i]) && notna == 0 && !is.na(BGover200[i+1])) {
        notna <- 1
        peaks <- peaks + 1
      } else if (!is.na(BGover200[i]) && notna == 1) {
        next()
      } else if (is.na(BGover200[i]) && notna == 1) {
        notna <- 0
      }
    }
    cgmupload["excursions_over_200",f] <- as.numeric(peaks)
    cgmupload["min_spent_over_200",f] <-
      as.numeric((length(BGover200[which(!is.na(BGover200))]) - peaks) * 5)
    cgmupload["percent_time_over_200",f] <-
      (as.numeric(cgmupload["min_spent_over_200",f]) /
         (as.numeric(cgmupload["total_sensor_readings",f])*5)) * 100

    cgmupload["avg_excur_over_200_per_day",f] <-
      as.numeric(cgmupload["excursions_over_200",f]) /
      as.numeric(cgmupload["num_days_cgm_wear",f])

# Under 60.
    BGunder60 <- as.numeric(table$Sensor.Glucose..mg.dL.[
      which(!is.na(table$Sensor.Glucose..mg.dL.))],length = 1)
    BGunder60[BGunder60 > 60] <- NA
    peaks <- 0
    notna <- 0
    for (i in 1:length(BGunder60)) {
      if (is.na(BGunder60[i]) && notna == 0) {
        next()
      } else if (!is.na(BGunder60[i]) && notna == 0 && !is.na(BGunder60[i+1])) {
        notna <- 1
        peaks <- peaks + 1
      } else if (!is.na(BGunder60[i]) && notna == 1) {
        next()
      } else if (is.na(BGunder60[i]) && notna == 1) {
        notna <- 0
      }
    }
    cgmupload["excursions_under_60",f] <- as.numeric(peaks)
    cgmupload["min_spent_under_60",f] <-
      as.numeric((length(BGunder60[which(!is.na(BGunder60))]) - peaks) * 5)
    cgmupload["percent_time_under_60",f] <-
      (as.numeric(cgmupload["min_spent_under_60",f]) /
         (as.numeric(cgmupload["total_sensor_readings",f])*5)) * 100

    cgmupload["avg_excur_under_60_per_day",f] <-
      as.numeric(cgmupload["excursions_under_60",f]) /
      as.numeric(cgmupload["num_days_cgm_wear",f])

# Under 70.
    BGunder70 <- as.numeric(table$Sensor.Glucose..mg.dL.[
      which(!is.na(table$Sensor.Glucose..mg.dL.))],length = 1)
    BGunder70[BGunder70 > 70] <- NA
    peaks <- 0
    notna <- 0
    for (i in 1:length(BGunder70)) {
      if (is.na(BGunder70[i]) && notna == 0) {
        next()
      } else if (!is.na(BGunder70[i]) && notna == 0 && !is.na(BGunder70[i+1])) {
        notna <- 1
        peaks <- peaks + 1
      } else if (!is.na(BGunder70[i]) && notna == 1) {
        next()
      } else if (is.na(BGunder70[i]) && notna == 1) {
        notna <- 0
      }
    }
    cgmupload["excursions_under_70",f] <- as.numeric(peaks)
    cgmupload["min_spent_under_70",f] <-
      as.numeric((length(BGunder70[which(!is.na(BGunder70))]) - peaks) * 5)
    cgmupload["percent_time_under_70",f] <-
      (as.numeric(cgmupload["min_spent_under_70",f]) /
         (as.numeric(cgmupload["total_sensor_readings",f])*5)) * 100

    cgmupload["avg_excur_under_70_per_day",f] <-
      as.numeric(cgmupload["excursions_under_70",f]) /
      as.numeric(cgmupload["num_days_cgm_wear",f])

# Find daytime AUC.
    times <- table$Time[which(!is.na(table$Time))]
    hours <- as.numeric(substr(times,1,2))
    daytime_indexes <- which(hours %in% 6:22)
    daytime_sensor <- table$Sensor.Glucose..mg.dL.[daytime_indexes]
    daytime_times <- table$Timestamp[daytime_indexes]
    xaxis <- seq(from = 0, length.out = length(daytime_times),by = 5)

# Remove NAs if they are present.
    xaxis[which(is.na(daytime_sensor))] <- NA
    xaxis <- xaxis[!is.na(xaxis)]
    daytime_sensor <- daytime_sensor[!is.na(daytime_sensor)]
    aucs <- pracma::cumtrapz(xaxis,daytime_sensor)
    cgmupload["daytime_auc",f] <- aucs[length(daytime_sensor)]

# Other daytime sensor glucose variables.
    cgmupload["daytime_avg_sensor_glucose",f] <- mean(daytime_sensor)
    cgmupload["daytime_min_sensor_glucose",f] <- min(daytime_sensor)
    cgmupload["daytime_max_sensor_glucose",f] <- max(daytime_sensor)
    cgmupload["daytime_sd",f] <- stats::sd(daytime_sensor)

# Nighttime AUC.
    nighttime_indexes <- which(hours %in% c(23,24,0:5))
    nighttime_sensor <- table$Sensor.Glucose..mg.dL.[nighttime_indexes]
    nighttime_times <- table$Timestamp[nighttime_indexes]
    xaxis <- seq(from = 0, length.out = length(nighttime_times),by = 5)

# Remove NAs if they are present.
    xaxis[which(is.na(nighttime_sensor))] <- NA
    xaxis <- xaxis[!is.na(xaxis)]
    nighttime_sensor <- nighttime_sensor[!is.na(nighttime_sensor)]
    aucs <- pracma::cumtrapz(xaxis,nighttime_sensor)
    cgmupload["nighttime_auc",f] <- aucs[length(nighttime_sensor)]

# Other nighttime sensor glucose variables.
    cgmupload["nighttime_avg_sens_glucose",f] <- mean(nighttime_sensor)
    cgmupload["nighttime_min_sens_glucose",f] <- min(nighttime_sensor)
    cgmupload["nighttime_max_sens_glucose",f] <- max(nighttime_sensor)
    cgmupload["nighttime_sd",f] <- stats::sd(nighttime_sensor)

# Total AUC.
    sensorBG <- as.numeric(table$Sensor.Glucose..mg.dL.,length = 1)
    xaxis <- seq(from = 0, length.out = length(sensorBG),by = 5)

# Remove NAs if they are present.
    xaxis[which(is.na(sensorBG))] <- NA
    xaxis <- xaxis[!is.na(xaxis)]
    sensorBG <- sensorBG[!is.na(sensorBG)]
    aucs <- pracma::cumtrapz(xaxis,sensorBG)
    cgmupload["total_auc",f] <- aucs[length(sensorBG)]

    cgmupload["average_auc_per_day",f] <-
      as.numeric(cgmupload["total_auc",f]) /
      as.numeric(cgmupload["num_days_cgm_wear",f])

# AUC over 180.
    sensorover180 <- table$Sensor.Glucose..mg.dL.
    sensorover180 <- sensorover180[sensorover180 >= 180]
    sensorover180 <- sensorover180[!is.na(sensorover180)]
    xaxis <- seq(from = 0, length.out = length(sensorover180),by = 5)

# Calculate cumulative AUC, and subtract recatangle where length = 180 &
# width = minutes.
    if (length(sensorover180) > 1) {
      aucs <- pracma::cumtrapz(xaxis,sensorover180)
      aucs <- (aucs[length(sensorover180)]) - (xaxis[length(xaxis)] * 180)
      cgmupload["auc_over_180",f] <- aucs
    } else {
      cgmupload["auc_over_180",f] <- 0
    }

    cgmupload["average_auc_180",f] <-
      as.numeric(cgmupload["auc_over_180",f]) /
      as.numeric(cgmupload["num_days_cgm_wear",f])
  }

# Write file.
  cgmupload <- cgmupload[-1,]
  cgmupload[is.na(cgmupload)] <- ""
  cgmupload[cgmupload == "NaN"] <- ""
  cgmupload <- cbind("Variable / Field Name" = rownames(cgmupload),cgmupload)
  filename <- paste(outputdirectory,"/",outputname,".csv",sep = "")
  utils::write.csv(cgmupload, file = filename,row.names = FALSE)
}
