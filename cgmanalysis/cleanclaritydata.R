#' Clean Clarity CGM Data
#'
#' This function cleans Dexcom CGM data exported from Clarity and gets it ready
#' for analysis. It removes the first 4 hours of sensor data, then searches for
#' missing data in the rest of the file. The default settings assume intervals
#' of 5 minutes and that gaps of 20 minutes or less can be filled in with linear
#' interpolation. The exported .csv file is written to a new directory called
#' "Cleaned CSVs."
#' @param 
#' @import 
#' @return 
#' @examples 
#' @export

cleanclaritydata <- function(inputdirectory,
                             outputdirectory = "Cleaned Clarity CSVs",
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

# Check delimiter.
    con = file(files[f],"r")
    firstline <- readLines(con,n=1,skipNul = TRUE)
    close(con)
    if (grepl(',', firstline) == TRUE){
      delimiter <- 'csv'
    } else {
      delimiter <- 'tab'
    }
    
# Read in the data.
    if (delimiter == 'tab'){
      dexcomdata <- utils::read.table(files[f],header = TRUE, sep = c('\t',','),
                                      stringsAsFactors = FALSE,
                                      na.strings = c("Low","High",""),
                                      fill = TRUE)
    } else if (delimiter == 'csv'){
      dexcomdata <- utils::read.csv(files[f],header = TRUE,
                                    stringsAsFactors = FALSE,
                                    na.strings = c("Low","High",""),
                                    fill = TRUE)
      }

# Use patient info to name output.
    id <- paste(dexcomdata$Patient.Info[1],dexcomdata$Patient.Info[2],sep = '')
    
# Remove all unnecessary columns, rename relevant columns.    
    cols <- c('Timestamp..YYYY.MM.DDThh.mm.ss.','Glucose.Value..mg.dL.')
    dexcomdata <- dexcomdata[,cols]
    colnames(dexcomdata) <- c("DateTime","GlucoseValue")
    
# Remove rows without glucose data.
    dexcomdata <- dexcomdata[-c(1:min(which(complete.cases(dexcomdata)))),]

# Remove first four hours of data,make glucose column numeric. Format Datetime.
    dexcomdata <- dexcomdata[-c(1:(240/interval)),]
    dexcomdata$GlucoseValue <- as.numeric(dexcomdata$GlucoseValue)
    dexcomdata$DateTime <- gsub('T',' ',dexcomdata$DateTime)
    
    dexcomdatacols<- c("Index","Event Type","Event Subtype","Patient Info",
                       "Device Info","Source Device ID","Insulin Value (u)",
                       "Carb Value (grams)","Duration (hh:mm:ss)",
                       "Glucose Rate of Change (mg/dL/min)",
                       "Transmitter Time (Long Integer)")
    
    for (i in dexcomdatacols) {
      dexcomdata[,i] <- NA
    }
    
    dexcomdata <- dexcomdata[c("Index","Event Type","Event Subtype",
                               "DateTime","GlucoseValue","Patient Info",
                               "Device Info","Source Device ID",
                               "Insulin Value (u)","Carb Value (grams)",
                               "Duration (hh:mm:ss)",
                               "Glucose Rate of Change (mg/dL/min)",
                               "Transmitter Time (Long Integer)")]

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
    formattedfilename <- paste(outputdirectory,"/",id," Formatted R.csv",
                               sep = "")
    utils::write.csv(dexcomdata, file = formattedfilename, row.names = FALSE)
  }
}
