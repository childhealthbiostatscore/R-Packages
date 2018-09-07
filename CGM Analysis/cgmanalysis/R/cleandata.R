#' Clean CGM Data
#' 
#' This function returns cleaned CGM files for analysis. Files must not be 
#' edited, and should be saved in the original format. If any files need to be 
#' edited manually, please save them in the format specified by the 
#' cgmvariables() function. If this function is unable to read your unedited 
#' CGM data, it may help to save your data in the format above. 
#' 
#' Because Diasend data is exported in an Excel document containing multiple 
#' tabs, the CGM data must be in the first tab in order to be read effectively.
#' 
#' @param inputdirectory The directory containing CSV files for cleaning prior 
#' to analysis.
#' @param outputdirectory The directory where cleaned CSV files will be written.
#' @param removegaps Determines whether the data are cleaned or not. If set to 
#' TRUE, any gaps in the data will be removed along with the 24 hours of data 
#' containing the gap(s). The tail end of the data will also be trimmed to 
#' ensure the timeseries is in discrete 24 hour chunks.
#' @param gapfill If set to TRUE (and if removegaps = TRUE), gaps smaller than 
#' or equal to maximumgap will be interpolated rather than removed.
#' @param maximumgap Allows the user to determine the longest data gap (in 
#' minutes) that will be interpolated. 
#' @usage cleandata(inputdirectory,
#' outputdirectory = tempdir(),
#' removegaps = TRUE,
#' gapfill = TRUE,
#' maximumgap = 20)
#' @examples \dontrun{cleandata(system.file("extdata", "De-identified",
#' package = "cgmanalysis"))}
#' @return
#' @export

cleandata <- function(inputdirectory,
                      outputdirectory = tempdir(),
                      removegaps = TRUE,
                      gapfill = TRUE,
                      maximumgap = 20) {

# Set system locale to read all characters. Read in file list. Creat output 
# directory.
  base::Sys.setlocale("LC_ALL", "C")
  files <- base::list.files(path = inputdirectory,full.names = TRUE)
  dir.create(outputdirectory,showWarnings = FALSE)
  dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                      "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                      "Ydm HM","Ydm HMS","ydm HM","ydm HMS")
  
# Read in data, check CGM type.  
  for (f in 1:base::length(files)) {
    ext <- tools::file_ext(files[f])
    enc <- base::as.character(readr::guess_encoding(files[f])[1,1])
    if (ext == "txt") {
      table <- utils::read.table(files[f],
                                 sep = "\t",
                                 encoding = "UTF-16LE",
                                 skipNul = TRUE,
                                 header = TRUE,
                                 stringsAsFactors = FALSE,
                                 na.strings = "",
                                 fileEncoding = enc)
    } else if (ext == "csv") {
        table <- utils::read.csv(files[f],
                                 stringsAsFactors = FALSE,
                                 header = TRUE,
                                 na.strings = "",
                                 skipNul = TRUE,
                                 fileEncoding = enc)
    } else if (ext == "xls" | ext == "xlsx" | ext == "xlsm") {
          table <- gdata::read.xls(files[f],
                                   stringsAsFactors = FALSE,
                                   na.strings = "")
        }
    
    if (base::ncol(table) == 3 && base::colnames(table)[3] == "X") {
      cgmtype <- "diasend"
    } else if (base::ncol(table) == 18) {
      cgmtype <- "libre"
    } else if (base::ncol(table) == 13 | base::ncol(table) == 14) {
      cgmtype <- "dexcom"
    } else if (base::ncol(table) == 47) {
      cgmtype <- "carelink"
    } else if (base::ncol(table) == 3 && base::colnames(table)[2] == "timestamp" 
               && base::colnames(table)[3] == 'sensorglucose') {
      cgmtype <- "manual"
    } else if (base::ncol(table) == 17 | base::ncol(table) == 22 | base::ncol(table) == 34) {
      cgmtype <- "ipro"
    } else {
      stop(base::paste("File '",files[f],"' is formatted incorrectly, and the 
                       data cannot be read.",sep = ""))
      }
      
# Format columns.
    if (cgmtype == "diasend") {
      id <- base::colnames(table)[1]
      table <- table[-c(base::which(!is.na(table[,3]))),]
      table <- table[,-c(3)]
      base::colnames(table) <- c("timestamp","sensorglucose")
      table <- table[-c(1),]
    } else if (cgmtype == "carelink") {
      id <- table$Patient.ID[1]
      table <- table[-c(1:6),]
      base::colnames(table) <- table[base::which(table[,3] == "Sensor")+1,]
      table <- table[-c(1:(base::which(table[,3] == "Sensor")+1)),]
      table <- table[-c(base::which(!is.na(table$`Event Marker`))),]
      table$timestamp <- base::paste(table$Date,table$Time)
      table$timestamp <- base::gsub('.{3}$',"",table$timestamp)
      table <- table[,c('timestamp','Sensor Glucose (mg/dL)')]
      base::colnames(table) <- c('timestamp','sensorglucose')
    } else if (cgmtype == "dexcom") {
      id <- table$Patient.Info[3]
      table <- table[,c('Timestamp..YYYY.MM.DDThh.mm.ss.',
                        'Glucose.Value..mg.dL.')]
      base::colnames(table) <- c('timestamp','sensorglucose')
      table$timestamp <- base::sub("T"," ",table$timestamp)
    } else if (cgmtype == "libre") {
      id <- table[1,1]
      base::colnames(table) <- table[2,]
      table <- table[-c(1:2),]
      table <- table[,c("Meter Timestamp","Historic Glucose(mg/dL)")]
      base::colnames(table) <- c('timestamp','sensorglucose')
    } else if (cgmtype == "manual") {
      table$sensorglucose <- 
        base::suppressWarnings(base::as.numeric(table$sensorglucose))
      table <- 
        table[-c(max(base::which(!is.na(table$sensorglucose)))+1:nrow(table)),]
      id <- table[,1][1]
      table <- table[,-c(1)]
    } else if (cgmtype == "ipro") {
      base::colnames(table) <- table[11,]
      id <- table[2,2]
      table <- table[-c(1:11),]
      table$Timestamp <- base::sub("[.]00","",table$Timestamp)
      table <- table[,c("Timestamp","Sensor Glucose (mg/dL)")]
      base::colnames(table) <- c('timestamp','sensorglucose')
    }

# Make sensor glucose numeric, sort table by timestamp, remove duplicate rows. 
# If necessary, remove rows with no data.
    if (NA %in% table$timestamp) {
      table <- table[-c(base::which(is.na(table$timestamp))),]
    }
    
    table$timestamp <- 
      base::as.POSIXct(lubridate::parse_date_time(table$timestamp,
                                                  dateparseorder),tz = "UTC")
    table$sensorglucose <- 
      base::suppressWarnings(base::as.numeric(table$sensorglucose))
    table <- table[base::order(table$timestamp),]

# ***Make record start time ignore "PULSE INIT" etc. for iPro***
    recordstart <- 
      base::strftime(table$timestamp[min(which(!is.na(table$sensorglucose)))],
                     format = "%m/%d/%Y %T")
    removaltime <- 
      base::strftime(table$timestamp[length(table$timestamp)],
                     format = "%m/%d/%Y %T")
    
# Set interval based on mode of timestamp diff.
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp)))
    
# Clean data (optional).
    if (removegaps == TRUE) {
# Remove first rows without sensor glucose data.      
      if (is.na(table$sensorglucose[1])) {
        table <- 
          table[-c(1:base::min(base::which(!is.na(table$sensorglucose))) - 1),]
      }
# Remove first 4 hours of data based on timestamp. Add 14,400 seconds (4 hours) 
# to first timestamp. 
      hour4 <- base::as.numeric(table$timestamp[1]) + 14400
# Determine which row contains the timestamp closest to hour4, remove all rows 
# up to and including that row.    
      table <- 
        table[-c(1:(which(abs(as.numeric(table$timestamp) - hour4) == 
                            min(abs(as.numeric(table$timestamp) - hour4)))[1])),]
      
# Fill in small sensor glucose data gaps. 
      if (gapfill == TRUE) {
        table$sensorglucose <- zoo::na.approx(table$sensorglucose,na.rm = FALSE,
                                              maxgap = (maximumgap*60)/interval)
      }
# If remaining gaps are larger than the maximum, remove the 24 chunk containing 
# the gap.  
      repeat(
        if (NA %in% table$sensorglucose) {
# Determine the start time for the sensor data gap.          
          startNA <- 
            base::as.numeric(table$timestamp[base::min(base::which(is.na(
              table$sensorglucose)))])
# Add 24 hours minus one recording interval.          
          hour24 <- startNA + (86400 - interval)
          table <- table[-c(base::suppressWarnings(base::which(base::abs(
            base::as.numeric(table$timestamp) - startNA) == base::min(base::abs(
              base::as.numeric(table$timestamp) - startNA))):(base::which(
                base::abs(base::as.numeric(
                  table$timestamp) - hour24) == base::min(base::abs(
                    base::as.numeric(table$timestamp) - hour24)))))),]
        } else if (!(NA %in% table$sensorglucose)) {
          break()
        }
      )
      if (base::length(table$timestamp) == 0) {
        stop(base::paste("File '",files[f],"' does not have enough data and 
                         cannot be processed with the current settings.",
                         sep = ""))
      }
# Trim end of data so it is in 24 hour chunks.
      seconds <- 
        ((base::as.numeric(base::floor(table$timestamp[base::length(
          table$timestamp)] - table$timestamp[1]))) * 86400) - interval
      table <- 
        table[-c(base::which(table$timestamp > 
                               (table$timestamp[1] + seconds))),]
      if ((1 - base::as.numeric(table$timestamp[base::length(
        table$timestamp)] - table$timestamp[1])%%1) > 0.1) {
        seconds <- ((base::as.numeric(base::floor(table$timestamp[base::length(
          table$timestamp)] - table$timestamp[1]))) * 86400) - interval
        table <- 
          table[-c(base::which(table$timestamp > 
                                 (table$timestamp[1] + seconds))),]
      }
    }
    table$subjectid <- ""
    table$subjectid[1] <- id
    table$subjectid[2] <- recordstart
    table$subjectid[3] <- removaltime
    table <-table[,c("subjectid","timestamp","sensorglucose")]
    filename <- 
      base::paste(outputdirectory,"/",tools::file_path_sans_ext(
        basename(files[f])),".csv",sep = "")
    utils::write.csv(table,file = filename,row.names = FALSE)
  }
}