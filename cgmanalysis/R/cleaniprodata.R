#' Clean Medtronic iPro Data
#'
#' This function cleans exported iPro data and gets it ready for analysis. It
#' removes the first four hours of CGM data, fills in data for gaps smaller than
#' maxgap, and removes the 24 hours chunk containign gaps larger than maxgap.
#' The exported .csv file is written to a new directory called "Cleaned CSVs."
#' @param inputdirectory The directory containing raw iPro files to be
#' formatted.
#' @param outputdirectory The directory where you would like the cleaned iPro
#' files to be written.
#' @param askforinfo Defaults to FALSE. If set to TRUE, the user will need to
#' input subject information to the console.
#' @param maxgap The maximum gap (in minutes) in sensor data that will be
#' interpolated. Defaults to 20 minutes.
#' @param gapfill Defaults to TRUE. If set to FALSE, gaps in sensor data will be
#' kept.
#' @import zoo
#' @usage
#' cleaniprodata(inputdirectory,
#'               outputdirectory = tempdir(),
#'               askforinfo = FALSE,
#'               maxgap = 20,
#'               gapfill = TRUE)
#' @return one formatted CSV file per file contained in inputdirectory.
#' @examples
#' cleaniprodata(system.file("extdata", "iPro_CSVs", package = "cgmanalysis"))
#' @export

cleaniprodata <- function(inputdirectory,
                          outputdirectory = tempdir(),
                          askforinfo = FALSE,
                          maxgap = 20,
                          gapfill = TRUE){

# Create outputdirectory if it does not already exist.
  dir.create(outputdirectory, showWarnings = FALSE)

# List files in input directory.
  files <- list.files(path = inputdirectory,full.names = TRUE)

# Iterate through directory.
  for (f in 1:length(files)){

# Read csv file.
    cgmdata <- utils::read.csv(files[f], stringsAsFactors = FALSE,
                               na.strings = "")

# Read in subject name and ID, remove parentheses, split into first name, last
# name, and subject ID elements (based on spaces).
    name <- cgmdata[2,2]
    name <- gsub("\\(|\\)","",name)
    name <- strsplit(name," ")

# Make row 11 the column names, and delete the first 11 rows.
    colnames(cgmdata) <- cgmdata[11,]
    cgmdata <- cgmdata[-c(1:11),]

# Add empty columns and reorder columns.
    cgmdata$`Monitor ID` <- NA
    cgmdata$`Gap Minute` <- NA
    cgmdata$`Total Gap Time` <- NA
    cgmdata$`First Name` <- NA
    cgmdata$`Last Name` <- NA
    cgmdata$`Patient ID` <- NA
    cgmdata$`Monitor ID` <- NA
    cgmdata$`Meal` <- NA
    cgmdata$`Gap Minute` <- NA
    cgmdata$`Total Gap Time` <- NA
    cgmdata$`Delete` <- NA
    cgmdata <- cgmdata[c('First Name','Last Name','Patient ID','Monitor ID',
                         'Index','Timestamp','Date','Time','ISIG Value',
                         'Used in Calibration','BG Reading (mg/dL)',
                         'Sensor Event','Meal','Gap Minute','Total Gap Time',
                         'Sensor Glucose (mg/dL)','Delete')]

# If necessary, ask for user input on the subject's name and ID, add columns for
# these values. Otherwise, use the name in the CSV file.
    if(askforinfo == TRUE){
      cgmdata$`First Name` <- readline(prompt = "Subject First Name: ")
      cgmdata$`Last Name` <- readline(prompt = "Subject Last Name: ")
      cgmdata$`Patient ID` <- readline(prompt = "Patient ID: ")
    } else {
      cgmdata$`First Name` <- name[[1]][1]
      cgmdata$`Last Name` <- name[[1]][2]
      cgmdata$`Patient ID` <- name[[1]][3]
    }

# Remove rows containing data from glucometer tests.
    for (i in 1:nrow(cgmdata)) {
      if (!is.na(cgmdata$`Used in Calibration`[i]))
      {cgmdata$Delete[i] <- "y"}
    }

    cgmdata <- subset(cgmdata,is.na(cgmdata$Delete))

# Remove first rows where sensor glucose is blank, and then the first four
# hours of continuous data (each row represents 5 minutes).
    for (i in 1:nrow(cgmdata)) {
      if (is.na(cgmdata[i,'Sensor Glucose (mg/dL)'])){
        cgmdata[i,'Sensor Glucose (mg/dL)'] <- "Delete"
      } else {
        break()
      }
    }

    cgmdata <- cgmdata[cgmdata$`Sensor Glucose (mg/dL)` != "Delete",]
    cgmdata <- cgmdata[-c(1:48),]

# Make sensor glucose column numeric.
    cgmdata$`Sensor Glucose (mg/dL)` <-
      as.numeric(cgmdata$`Sensor Glucose (mg/dL)`)

# Fill small gaps in data.
    if (gapfill == TRUE) {
      cgmdata$`Sensor Glucose (mg/dL)` <-
        zoo::na.approx(cgmdata$`Sensor Glucose (mg/dL)`,
                  maxgap = as.numeric(maxgap/5),na.rm = FALSE)

# If gaps are larger than the maximum, remove the 24 chunk containing the gap.
      repeat{
        for (i in 1:nrow(cgmdata)) {
          if (is.na(cgmdata$`Sensor Glucose (mg/dL)`[i])) {
            cgmdata <- cgmdata[-c(i:(i + 288)), ]}
        }
        if (!(NA %in% cgmdata$`Sensor Glucose (mg/dL)`)) {
          break
        }
      }

# Trim end of CGM data so they are in 24 hours chunks.
      days <- floor((length(!is.na(cgmdata$`Sensor Glucose (mg/dL)`)))/288)
      cgmdata <- cgmdata[1:(days * 288),]
    }

# Make file name for writing .csv file.
    formattedfilename <- gsub(".*\\/", "",files[f])
    formattedfilename <- paste(outputdirectory,"/",
                               gsub(".csv", "",formattedfilename),
                               "_Formatted_R.csv", sep = "")

# Write new .csv file based on Patient ID input.
    cgmdata$Delete <- NULL
    utils::write.csv(cgmdata, file = formattedfilename, row.names = FALSE)
  }
}
