#' Clean Dexcom CGM Data
#'
#' This function cleans exported Dexcom CGM data and gets it ready for analysis.
#' It removes the first 4 hours of sensor data, then searches for missing data
#' in the rest of the file. The default settings assume intervals of 5 minutes
#' and that gaps of 20 minutes or less can be filled in with linear
#' interpolation. The exported .csv file is written to a new directory called
#' "Cleaned CSVs."
#' @param inputdirectory The directory containing raw Dexcom files to be
#' formatted.
#' @param outputdirectory The directory where you would like the cleaned Dexcom
#' files to be written.
#' @param interval The length of time between CGM sensor measurements. Defaults
#' to 5 minutes.
#' @param maxgap The maximum gap (in minutes) in sensor data that will be
#' interpolated. Defaults to 20 minutes.
#' @param gapfill Defaults to TRUE. If set to FALSE, gaps in sensor data will be
#' kept.
#' @import zoo
#' @usage
#' cleandexcomdata(inputdirectory,
#'                 outputdirectory = tempdir(),
#'                 interval = 5,
#'                 maxgap = 20,
#'                 gapfill = TRUE)
#' @return one formatted CSV file per file contained in inputdirectory.
#' @examples
#' cleandexcomdata(system.file("extdata", "Dexcom_Tests",
#'                 package = "cgmanalysis"))
#' @export

cleandexcomdata <- function(inputdirectory,
                            outputdirectory = tempdir(),
                            interval = 5,
                            maxgap = 20,
                            gapfill = TRUE){

# Create outputdirectory if it does not already exist.
  dir.create(outputdirectory, showWarnings = FALSE)

# List files in input directory.
  files <- list.files(path = inputdirectory,full.names = TRUE)

# Iterate through each file in directory and clean it.
# Check whether file is a G5 Dexcom or not.
  for (f in 1:length(files)) {
    if (tools::file_ext(files[f]) == "txt"){
      con <- file(files[f],"r")
      first_line <- readLines(con,n=1,skipNul = TRUE)
      close(con)
      test <- "Device Info"
      Sys.setlocale('LC_ALL','C')
      if (grepl(test,first_line) == TRUE){
        g5 <- TRUE
      } else {
        g5 <- FALSE
      }
    }

# If it's a G5 Dexcom, read in using UTF-16.
    if (g5 == FALSE){
      dexcomdata <- utils::read.table(files[f],header = FALSE, sep = "\t",
                               stringsAsFactors = FALSE,na.strings =
                                 c("Low","High"),fill = TRUE)
    } else if (g5 == TRUE) {
      dexcomdata <- utils::read.table(files[f],header = FALSE, sep = "\t",
                               stringsAsFactors = FALSE,na.strings =
                                 c("Low","High"),fill = TRUE,
                               fileEncoding="UTF-16LE")
    }
    colnames(dexcomdata) <- dexcomdata[1,]
    dexcomdata <- dexcomdata[-c(1),]

# Use the original filename to name output.
    id <- sub(".*\\/", "", files[f])
    id <- sub(".txt", "",id)
    if (g5 == FALSE){
      dexcomdata$PatientInfoValue <- id
    } else if (g5 == TRUE){
      colnames(dexcomdata)[8] <- "GlucoseValue"
      colnames(dexcomdata)[2] <- "DateTime"
      dexcomdata <- dexcomdata[c("Index","Event Type","Event Subtype",
                                 "DateTime","GlucoseValue","Patient Info",
                                 "Device Info","Source Device ID",
                                 "Insulin Value (u)","Carb Value (grams)",
                                 "Duration (hh:mm:ss)",
                                 "Glucose Rate of Change (mg/dL/min)",
                                 "Transmitter Time (Long Integer)")]
    }

# Remove first four hours of data,
    dexcomdata <- dexcomdata[-c(1:(240/interval)),]
    dexcomdata$GlucoseValue <- as.numeric(dexcomdata$GlucoseValue)

# Fill in gaps according to maximum gap setting. Make sure glucose values are
# the correct format.
    if (gapfill == TRUE) {
      dexcomdata$GlucoseValue <- zoo::na.approx(dexcomdata$GlucoseValue,
                                           maxgap = (maxgap/interval))

# Iterate through each row in the dataframe, and check for NAs. If the glucose
# value is missing, mark it and the following 24 hours of data for deletion.
      for (r in 1:length(dexcomdata$GlucoseValue)) {
        if (is.na(dexcomdata$GlucoseValue[r]) && (r+((1440/interval)-1)) <
            length(dexcomdata$GlucoseValue)){
          dexcomdata$GlucoseValue[r:(r+((1440/interval)-1))] <- "Delete"
        } else if (is.na(dexcomdata$GlucoseValue[r]) && (r+((1440/interval)-1))
                   > length(dexcomdata$GlucoseValue)) {
          dexcomdata$GlucoseValue[r:length(dexcomdata$GlucoseValue)] <- "Delete"
        }
      }

# Delete rows and trim to even number of 24 hour chunks.
      dexcomdata <- dexcomdata[dexcomdata$GlucoseValue != "Delete", ]
      days <- floor((length(dexcomdata$GlucoseValue)/(1440/interval)))
      dexcomdata <- dexcomdata[1:(days * (1440/interval)),]
    }

# Write to CSV file.
    formattedfilename <- paste(outputdirectory,"/",gsub(".txt", "",id),
                               "_Formatted_R.csv",sep = "")
    utils::write.csv(dexcomdata, file = formattedfilename, row.names = FALSE)
  }
}
