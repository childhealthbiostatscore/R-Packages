#'
#' @param inputdirectory The directory containing Excel files to be converted.
#' @param outputdirectory The directory where you would like the CSV files to be
#' written.
#' @import gdata, tools
#' @usage
#' makecsv(inputdirectory,
#'         outputdirectory = tempdir())
#' @return 
#' @examples
#' @export

makecsv <- function(inputdirectory,
                    outputdirectory = tempdir()){

# Set locale to read all characters.    
  Sys.setlocale('LC_ALL','C')
  
# Create default output directory if it does not already exist in working
# directory.
  base::dir.create(outputdirectory, showWarnings = FALSE)

# List all files in the specified input directory.
  files <- base::list.files(path = inputdirectory,full.names = TRUE)

# Iterate through input directory, check file extension, read in file.
  for (i in 1:length(files)) {
    ext <- tools::file_ext(files[i])
    filename <- file_path_sans_ext(basename(files[i]))
    if (ext == "xls" | ext == "xlsx" | ext == "xlsm") {
      file <- gdata::read.xls(files[i],header = TRUE,stringsAsFactors = FALSE)
    } else if (ext == "csv") {
      file <- utils::read.csv(files[i],header = TRUE,stringsAsFactors = FALSE)
    } else if (ext == "txt") {
      file <- utils::read.delim(files[i],header = TRUE,stringsAsFactors = FALSE,skipNul = TRUE)
    }
# Write file to output directory as CSV.  
    write.table(file, file = paste(outputdirectory,"/",filename,".csv",sep = ""),row.names = FALSE,col.names = TRUE,sep = ",")
  }
}