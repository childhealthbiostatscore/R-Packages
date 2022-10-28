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
#' @param id_filename If true, the file name will be used for subject ID
#' rather than the ID contained in the data.
#' @param verbose If true, each file name will print as the program runs for
#' troubleshooting purposes.
#' @param unit Default unit is mg/dL. Any other value will multiply the
#' "sensorglucose" column by 18 (i.e. the package assumes that unit != "mg/dL"
#' implies that the units are mmol/L).
#' @usage cleandata(inputdirectory,
#' outputdirectory = tempdir(),
#' removegaps = TRUE,
#' gapfill = TRUE,
#' maximumgap = 20,
#' id_filename = F,
#' verbose = F,
#' unit = "mg/dL")
#' @examples \dontrun{
#' cleandata(system.file("extdata", "De-identified",
#'   package = "cgmanalysis"
#' ))
#' }
#' @export

cleandata <- function(inputdirectory,
                      outputdirectory = tempdir(),
                      removegaps = TRUE,
                      gapfill = TRUE,
                      maximumgap = 20,
                      id_filename = F,
                      verbose = F,
                      unit = "mg/dL") {
  # Set system locale to read all characters. Read in file list. Creat output
  # directory.
  files <- base::list.files(path = inputdirectory, full.names = TRUE, recursive = T)
  base::dir.create(outputdirectory, showWarnings = FALSE)

  # Read in data, check CGM type.
  for (f in 1:base::length(files)) {
    if (verbose == T) {
      print(basename(files[f]))
    }
    ext <- tools::file_ext(files[f])
    enc <- base::as.character(readr::guess_encoding(files[f])[1, 1])
    if (ext == "txt") {
      table <- utils::read.table(files[f],
        sep = "\t",
        skipNul = TRUE,
        header = TRUE,
        stringsAsFactors = FALSE,
        na.strings = "",
        fileEncoding = enc,
        comment.char = ""
      )
    } else if (ext == "csv") {
      table <- utils::read.csv(files[f],
        stringsAsFactors = FALSE,
        header = TRUE,
        na.strings = ""
      )
      if (base::ncol(table) <= 2) {
        table <- utils::read.csv(files[f],
          sep = ";",
          stringsAsFactors = FALSE,
          header = TRUE,
          na.strings = ""
        )
      }
    } else if (ext == "xls" | ext == "xlsx" | ext == "xlsm") {
      table <- suppressMessages(readxl::read_excel(files[f], col_types = "text"))
      table <- as.data.frame(table)
    } else if (ext == "xml") {
      doc <- XML::xmlParse(files[f])
      l <- XML::xmlToList(doc)
      id <- l$.attrs[["Id"]]
      l <- l[["GlucoseReadings"]]
      times <- lapply(l, function(x) {
        x[["DisplayTime"]]
      })
      times <- do.call(rbind, times)
      sensor <- lapply(l, function(x) {
        x[["Value"]]
      })
      sensor <- do.call(rbind, sensor)
      table <- cbind(times, sensor)
      table <- as.data.frame(table)
      colnames(table) <- c("timestamp", "sensorglucose")
      table$subjectid <- NA
      table$subjectid[1] <- id
      table <- table[, c("subjectid", "timestamp", "sensorglucose")]
    } else if (ext == "ASC") {
      table <- utils::read.delim(files[f])
    }

    if (base::ncol(table) == 3 & base::colnames(table)[3] == "X" | base::ncol(table) == 2) {
      cgmtype <- "diasend"
    } else if (base::ncol(table) == 18 | base::ncol(table) == 19) {
      if (table[2, 1] == "Device") {
        cgmtype <- "libre pro"
      } else {
        cgmtype <- "libre"
      }
    } else if (base::ncol(table) == 4) {
      cgmtype <- "libre pro"
    } else if (base::ncol(table) == 13 | base::ncol(table) == 14) {
      cgmtype <- "dexcom"
    } else if (base::ncol(table) >= 47) {
      cgmtype <- "carelink"
    } else if (base::ncol(table) == 3 && base::colnames(table)[2] == "timestamp" &&
      base::colnames(table)[3] == "sensorglucose") {
      cgmtype <- "manual"
    } else if (base::ncol(table) == 17 | base::ncol(table) == 22 | base::ncol(table) == 34) {
      cgmtype <- "ipro"
    } else if (base::ncol(table) == 6 & ext == "ASC") {
      cgmtype <- "asc"
    } else if (base::ncol(table) == 6) {
      cgmtype <- "tslimg4"
    } else if (base::ncol(table) %in% 39:41) {
      cgmtype <- "tandem"
    } else {
      stop(base::paste("File '", files[f], "' is formatted incorrectly and the data cannot be read.", sep = ""))
    }
    ext <- paste0(".", ext)
    # Format columns.
    if (cgmtype == "diasend") {
      if (id_filename == F) {
        id <- base::colnames(table)[2]
      } else {
        id <- sub(ext, "", basename(files[f]))
      }
      table <- table[-c(1:base::which(table[, 1] == "Time")), ]
      base::colnames(table) <- c("timestamp", "sensorglucose")
    } else if (cgmtype == "carelink") {
      if (id_filename == F) {
        id <- table[1, grep("Patient", colnames(table))]
      } else {
        id <- sub(ext, "", basename(files[f]))
      }
      base::colnames(table) <- table[base::which(table[, 3] == "Sensor")[1] + 1, ]
      table <- table[-c(1:(base::which(table[, 3] == "Sensor")[1] + 1)), ]
      table$timestamp <- base::paste(table$Date, table$Time)
      table$timestamp <- base::gsub(".{3}$", "", table$timestamp)
      table <- table[, base::grep("timestamp|Sensor Glucose", colnames(table))]
      table <- table[, sort(colnames(table), decreasing = T)]
      base::colnames(table) <- c("timestamp", "sensorglucose")
    } else if (cgmtype == "dexcom") {
      if (id_filename == F) {
        id <- table[3, grep("patient", tolower(colnames(table)))]
      } else {
        id <- sub(ext, "", basename(files[f]))
      }
      table$sensorglucose <- table[, grep("glucose", tolower(colnames(table)))[1]]
      table$timestamp <- table[, grep("timestamp", tolower(colnames(table)))]
      table <- table[, c("timestamp", "sensorglucose")]
      table$timestamp <- base::sub("T", " ", table$timestamp)
    } else if (cgmtype == "libre") {
      if (id_filename == F) {
        id <- table[1, 1]
      } else {
        id <- sub(ext, "", basename(files[f]))
      }
      base::colnames(table) <- table[2, ]
      table <- table[-c(1:2), ]
      table <- table[, c(
        grep("Timestamp", colnames(table)),
        grep("Glucose", colnames(table))[1]
      )]
      base::colnames(table) <- c("timestamp", "sensorglucose")
    } else if (cgmtype == "libre pro") {
      if (id_filename == F) {
        id <- table[1, 1]
      } else {
        id <- sub(ext, "", basename(files[f]))
      }
      base::colnames(table) <- table[2, ]
      table <- table[-c(1:2), ]
      cols <- grep("time|historic", tolower(colnames(table)))
      table <- table[, cols]
      base::colnames(table) <- c("timestamp", "sensorglucose")
    } else if (cgmtype == "manual") {
      if (id_filename == F) {
        id <- table[, 1][1]
      } else {
        id <- sub(ext, "", basename(files[f]))
      }
      table$sensorglucose <-
        base::suppressWarnings(base::as.numeric(as.character(table$sensorglucose)))
      table <-
        table[-c(max(base::which(!is.na(table$sensorglucose))) + 1:nrow(table)), ]
      table <- table[, -c(1)]
    } else if (cgmtype == "ipro") {
      base::colnames(table) <- table[11, ]
      if (id_filename == F) {
        id <- table[2, 2]
      } else {
        id <- sub(ext, "", basename(files[f]))
      }
      table <- table[-c(1:11), ]
      if (grepl("- | /", table$Timestamp[1]) == F) {
        table$Timestamp <- base::as.POSIXct(as.numeric(table$Timestamp) * (60 * 60 * 24),
          origin = "1899-12-30",
          tz = "UTC"
        )
      }
      table$Timestamp <- base::sub("[.]00", "", table$Timestamp)
      table <- table[, c("Timestamp", "Sensor Glucose (mg/dL)")]
      base::colnames(table) <- c("timestamp", "sensorglucose")
    } else if (cgmtype == "asc") {
      id <- sub("\\..*", "", basename(files[f]))
      table$timestamp <- paste(table$Date, table$Time)
      table$sensorglucose <- table$Value
      table <- table[, c("timestamp", "sensorglucose")]
    } else if (cgmtype == "tslimg4") {
      row <- base::which(table[, 1] == "DeviceType")
      base::colnames(table) <- table[row, ]
      if (id_filename == F) {
        id <- table[row - 7, 2]
      } else {
        id <- sub(ext, "", basename(files[f]))
      }
      table <- table[-c(1:row), ]
      table$timestamp <- table$EventDateTime
      table$sensorglucose <- as.numeric(table$`Readings (CGM / BGM)`)
      table <- table[, c("timestamp", "sensorglucose")]
    } else if (cgmtype == "tandem") {
      if (id_filename == F) {
        id <- table[2, 2]
      } else {
        id <- sub(ext, "", basename(files[f]))
      }
      colnames(table) <- table[6, ]
      table <- table[-c(1:6), ]
      table <- table[1:(max(which(table[, 3] == "EGV"))), ]
      table <- table[, 4:5]
      colnames(table) <- c("timestamp", "sensorglucose")
      table$timestamp <- sub("T", " ", table$timestamp)
    }

    # Make sensor glucose numeric, sort table by timestamp, remove duplicate rows.
    # If necessary, remove rows with no data.
    table$timestamp <- parsedate::parse_date(table$timestamp, approx = F)

    table$sensorglucose <-
      base::suppressWarnings(base::as.numeric(base::sub(",", ".", table$sensorglucose)))
    table <- table[base::order(table$timestamp), ]
    table <- table[!(format(table$timestamp, "%H:%M:%S") == "00:00:00" & is.na(table$sensorglucose)), ]
    if (NA %in% table$timestamp) {
      table <- table[-c(base::which(is.na(table$timestamp))), ]
    }

    if (unit != "mg/dL") {
      table$sensorglucose <- table$sensorglucose * 18
    }

    recordstart <-
      base::strftime(table$timestamp[min(which(!is.na(table$sensorglucose)))],
        format = "%m/%d/%Y %T"
      )
    removaltime <-
      base::strftime(table$timestamp[length(table$timestamp)],
        format = "%m/%d/%Y %T"
      )

    # Set interval based on mode of timestamp diff.
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp)))

    # Clean data (optional).
    if (removegaps == TRUE) {
      # Remove first rows without sensor glucose data.
      if (is.na(table$sensorglucose[1])) {
        table <-
          table[-c(1:base::min(base::which(!is.na(table$sensorglucose))) - 1), ]
      }
      # Remove first 4 hours of data based on timestamp. Add 14,400 seconds (4 hours)
      # to first timestamp.
      hour4 <- base::as.numeric(table$timestamp[1]) + 14400
      # Determine which row contains the timestamp closest to hour4, remove all rows
      # up to and including that row.
      table <-
        table[-c(1:(which(abs(as.numeric(table$timestamp) - hour4) ==
          min(abs(as.numeric(table$timestamp) - hour4), na.rm = T))[1])), ]

      # Fill in small sensor glucose data gaps.
      if (gapfill == TRUE) {
        table$sensorglucose <- zoo::na.approx(table$sensorglucose,
          na.rm = FALSE,
          maxgap = (maximumgap * 60) / interval
        )
      }
      # If remaining gaps are larger than the maximum, remove the 24 chunk containing
      # the gap.
      repeat(
        if (NA %in% table$sensorglucose) {
          # Determine the start time for the sensor data gap.
          startNA <-
            base::as.numeric(table$timestamp[base::min(base::which(is.na(
              table$sensorglucose
            )))])
          # Add 24 hours minus one recording interval.
          hour24 <- startNA + (86400 - interval)
          table <- table[-c(base::suppressWarnings(base::which(base::abs(
            base::as.numeric(table$timestamp) - startNA
          ) == base::min(base::abs(
            base::as.numeric(table$timestamp) - startNA
          ))):(base::which(
            base::abs(base::as.numeric(
              table$timestamp
            ) - hour24) == base::min(base::abs(
              base::as.numeric(table$timestamp) - hour24
            ))
          )))), ]
        } else if (!(NA %in% table$sensorglucose)) {
          break()
        })
      if (base::length(table$timestamp) == 0) {
        stop(base::paste("File '", files[f], "' does not have enough data and
                         cannot be processed with the current settings.",
          sep = ""
        ))
      }
      # Trim end of data so it is in 24 hour chunks.
      seconds <-
        ((base::as.numeric(base::floor(table$timestamp[base::length(
          table$timestamp
        )] - table$timestamp[1]))) * 86400) - interval
      table <-
        table[-c(base::which(table$timestamp >
          (table$timestamp[1] + seconds))), ]
      if ((1 - base::as.numeric(table$timestamp[base::length(
        table$timestamp
      )] - table$timestamp[1]) %% 1) > 0.1) {
        seconds <- ((base::as.numeric(base::floor(table$timestamp[base::length(
          table$timestamp
        )] - table$timestamp[1]))) * 86400) - interval
        table <-
          table[-c(base::which(table$timestamp >
            (table$timestamp[1] + seconds))), ]
      }
    }
    table$subjectid <- ""
    table$subjectid[1] <- id
    table$subjectid[2] <- recordstart
    table$subjectid[3] <- removaltime
    table$subjectid <- as.character(table$subjectid)
    table <- table[, c("subjectid", "timestamp", "sensorglucose")]
    filename <-
      base::paste(outputdirectory, "/", tools::file_path_sans_ext(
        basename(files[f])
      ), ".csv", sep = "")
    utils::write.csv(as.data.frame(table), file = filename, row.names = FALSE)
  }
}
