#' Easy GV Table Maker
#'
#' This function takes a directory of CGM data and makes one table with every
#' sensor value. For computing MAGE, quartile range, etc. These can easily be
#' pasted into the EasyGV spreadsheet (by Nathan R Hill. © University of Oxford
#' 2010+)
#' @param inputdirectory The directory containing cleaned CSV files.
#' @param outputdirectory The directory where you would like the Easy GV-ready
#' table to be written.
#' @usage
#' easygvtable(inputdirectory,
#'             outputdirectory = tempdir())
#' @return a dataframe where each column contains every glucose value for a
#' given patient.
#' @examples
#' easygvtable(system.file("extdata","Cleaned_iPro_CSVs",
#'                         package = "cgmanalysis"))
#' @export

easygvtable <- function(inputdirectory,
                        outputdirectory = tempdir()){

  files <- list.files(path = inputdirectory,full.names = TRUE)
  glucosetable <- data.frame(matrix(nrow = 1728,ncol = length(files)))

  for (f in 1:length(files)){
    cgmdata <- utils::read.csv(files[f], stringsAsFactors = FALSE, na.strings = "")
    id <- gsub(" .*$","",files[f])
    id <- gsub(".*/","",id)
    colnames(glucosetable)[f] <- id
    glucosetable[,f] <-
      suppressWarnings(as.numeric(cgmdata$Sensor.Glucose..mg.dL.[1:1728],
                                  showWarnings = FALSE))
  }
  formattedfilename <- paste(outputdirectory,"/","Glucose Table.csv",sep = "")
  utils::write.csv(glucosetable,file = formattedfilename,row.names = FALSE)
}
