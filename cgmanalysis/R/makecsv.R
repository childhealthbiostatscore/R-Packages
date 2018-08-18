#' Convert Excel Workbooks to CSV.
#'
#' This function iterates through a specified directory containing CGM data
#' stored in multiple excel files. It converts each file to a CSV file and saves
#' it in a new directory called "CSVs." Requires gdata package. Windows users
#' must have Perl installed.
#' @param inputdirectory The directory containing Excel files to be converted.
#' @param outputdirectory The directory where you would like the CSV files to be
#' written.
#' @import gdata
#' @usage
#' makecsv(inputdirectory,
#'         outputdirectory = tempdir())
#' @return a directory of CSV files with data matching the original Excel
#' workbooks.
#' @examples
#' \dontrun{makecsv(system.file("extdata/", "iPro_Tests",
#'                  package = "cgmanalysis"))}
#' @export

makecsv <- function(inputdirectory,
                    outputdirectory = tempdir()){

# Detect OS.
  if (.Platform$OS.type == "windows") {
    windowsyn <- "y"
  } else {
    windowsyn  <- "n"
  }

# Create default output directory if it does not already exist in working
# directory.
  dir.create(outputdirectory, showWarnings = FALSE)

# List all files in the specified input directory.
  excelfiles <- list.files(path = inputdirectory)

# Iterate through each file in the input directory.
  for (i in 1:length(excelfiles)){
    inputfilename <- paste(inputdirectory,"/",excelfiles[i], sep = "")

# Format the output file name for each input file.
    outputfilename <- paste(excelfiles[i])
    outputfilename <- sub(".x.*","",outputfilename)
    outputfilename <- paste(outputdirectory,"/",outputfilename,".csv",
                            sep = "")
    if (windowsyn == "y") {
      outputfilename <- paste("C:",outputfilename,sep = "")
    }

# Read in the data from the Excel file.
    data <- gdata::read.xls(inputfilename)

# Write the data into a .csv file in the specified output directory (with the
# correct output file name).
    utils::write.csv(data, file = outputfilename,row.names = FALSE)
  }
}
