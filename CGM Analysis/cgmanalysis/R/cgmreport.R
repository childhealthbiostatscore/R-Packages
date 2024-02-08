#' Generate AGP
#'
#' This function takes a directory of cleaned CGM data and generates two
#' aggregate AGPs for all of the files combined. One aggregate AGP is produced
#' using Tukey smoothing, and the other uses ggplot's built in LOESS smoothing
#' function. The function also produces an AGP plot with each unique subject
#' plotted individually on the same graph (using LOESS smoothing).
#'
#' @param inputdirectory The directory containing all cleaned, formatted CGM
#' data to be analyzed.
#' @param outputdirectory The directory where plot PDF files should be written.
#' @param tz The time zone in which the data were recorded.
#' @param yaxis The range of the yaxis in mg/dL.
#' @usage cgmreport(inputdirectory, outputdirectory = tempdir(), tz = "UTC",
#' yaxis = c(0,400))
#' @examples cgmreport(system.file("extdata", "Cleaned", package = "cgmanalysis"))
#' @return Aggregate and per subject AGP reports based on all of the cleaned CGM
#' data in the input directory.
#' @export

cgmreport <- function(inputdirectory,
                      outputdirectory = tempdir(),
                      tz = "UTC",
                      yaxis = c(0, 400)) {
  # Get file list.
  files <- base::list.files(path = inputdirectory, full.names = TRUE)

  # Create data frame to store all glucose values from all files (for aggregate
  # AGP).
  aggregateAGPdata <- base::data.frame(matrix(ncol = 3, nrow = 0))
  colnames(aggregateAGPdata) <- c("subjectid", "time", "sensorglucose")

  # Iterate through directory, combine all data.
  for (f in 1:length(files)) {
    cgmdata <-
      utils::read.csv(files[f], stringsAsFactors = FALSE, header = TRUE, skipNul = TRUE)
    id <- cgmdata$subjectid[1]
    cgmdata$subjectid <- id
    aggregateAGPdata <- rbind(cgmdata, aggregateAGPdata)
  }
  aggregateAGPdata$sensorglucose <- as.numeric(aggregateAGPdata$sensorglucose)

  # Remove missing data.
  aggregateAGPdata <- aggregateAGPdata[stats::complete.cases(aggregateAGPdata), ]

  # Remove dates, so aggregate AGP data can be sorted by time of day.
  dateparseorder <- c(
    "mdy HM", "mdy HMS", "mdY HM", "mdY HMS", "dmy HM", "dmy HMS",
    "dmY HM", "dmY HMS", "Ymd HM", "Ymd HMS", "ymd HM", "ymd HMS",
    "Ydm HM", "Ydm HMS", "ydm HM", "ydm HMS"
  )
  aggregateAGPdata$timestamp <-
    as.POSIXct(lubridate::parse_date_time(aggregateAGPdata$timestamp,
      dateparseorder,
      tz = tz
    ))
  aggregateAGPdata$hour <- lubridate::round_date(aggregateAGPdata$timestamp, "hour")
  aggregateAGPdata$time <-
    as.POSIXct(strftime(aggregateAGPdata$timestamp, format = "%H:%M"),
      format = "%H:%M"
    )
  aggregateAGPdata$hourmin <-
    lubridate::round_date(aggregateAGPdata$timestamp, "10 minutes")
  aggregateAGPdata$hourmin <-
    as.POSIXct(strftime(aggregateAGPdata$hourmin, format = "%H:%M"),
      format = "%H:%M"
    )

  # Find 25%ile, median, and 75%ile blood glucose for each hour, store in new table.
  quartiles <-
    data.frame(matrix(nrow = length(unique(aggregateAGPdata$hourmin)), ncol = 6))
  colnames(quartiles) <-
    c(
      "hourmin", "sensorglucose5perc", "sensorglucoseqone", "sensorglucosemedian",
      "sensorglucoseqthree", "sensorglucose95perc"
    )
  quartiles$hourmin <- unique(aggregateAGPdata$hourmin)
  quartiles <- quartiles[order(quartiles$hourmin), ]

  for (i in 1:nrow(quartiles)) {
    quartiles$sensorglucose5perc[i] <-
      stats::quantile(as.numeric(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])]), 0.05)
    quartiles$sensorglucoseqone[i] <-
      as.numeric(summary(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])])[2])
    quartiles$sensorglucosemedian[i] <-
      as.numeric(summary(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])])[3])
    quartiles$sensorglucoseqthree[i] <-
      as.numeric(summary(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])])[5])
    quartiles$sensorglucose95perc[i] <-
      stats::quantile(as.numeric(aggregateAGPdata$sensorglucose[which(aggregateAGPdata$hourmin == quartiles$hourmin[i])]), 0.95)
  }

  quartiles$smooth5perc <-
    as.numeric(stats::smooth(quartiles$sensorglucose5perc, kind = "3R", twiceit = TRUE))
  quartiles$smoothqone <-
    as.numeric(stats::smooth(quartiles$sensorglucoseqone, kind = "3R", twiceit = TRUE))
  quartiles$smoothmed <-
    as.numeric(stats::smooth(quartiles$sensorglucosemedian, kind = "3R", twiceit = TRUE))
  quartiles$smoothqthree <-
    as.numeric(stats::smooth(quartiles$sensorglucoseqthree, kind = "3R", twiceit = TRUE))
  quartiles$smooth95perc <-
    as.numeric(stats::smooth(quartiles$sensorglucose95perc, kind = "3R", twiceit = TRUE))

  # Plots
  aggAGPtukey <- ggplot2::ggplot(quartiles, ggplot2::aes(x = quartiles$hourmin)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = quartiles$smoothqone, ymax = quartiles$smoothqthree, fill = "Interquartile Range"), alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = quartiles$smoothmed, color = "Median")) +
    ggplot2::geom_line(ggplot2::aes(y = quartiles$smooth95perc, linetype = "5th & 95th Percentile")) +
    ggplot2::geom_line(ggplot2::aes(y = quartiles$smooth5perc), linetype = "dashed") +
    ggplot2::ggtitle("Aggregate Daily Overlay (Tukey Smoothing)") +
    ggplot2::ylab("Sensor BG (mg/dL)") +
    ggplot2::xlab("Time (hour)") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_datetime(labels = function(x) format(x, format = "%H:%M")) +
    ggplot2::scale_fill_manual("", values = "blue") +
    ggplot2::scale_color_manual("", values = "red") +
    ggplot2::scale_linetype_manual("", values = "dashed") +
    ggplot2::ylim(yaxis[1], yaxis[2])

  AGPloess <-
    ggplot2::ggplot(aggregateAGPdata, ggplot2::aes(x = aggregateAGPdata$time, y = aggregateAGPdata$sensorglucose)) +
    ggplot2::geom_smooth(ggplot2::aes(y = aggregateAGPdata$sensorglucose, color = aggregateAGPdata$subjectid), se = FALSE) +
    ggplot2::geom_point(ggplot2::aes(y = aggregateAGPdata$sensorglucose, color = aggregateAGPdata$subjectid), shape = ".") +
    ggplot2::ggtitle("Daily Overlay Per Subject (LOESS Smoothing)") +
    ggplot2::ylab("Sensor BG (mg/dL)") +
    ggplot2::xlab("Time (hour)") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(colour = "Subject ID") +
    ggplot2::scale_x_datetime(labels = function(x) format(x, format = "%H:%M")) +
    ggplot2::ylim(yaxis[1], yaxis[2])

  aggAGPloess <-
    ggplot2::ggplot(aggregateAGPdata, ggplot2::aes(x = aggregateAGPdata$time, y = aggregateAGPdata$sensorglucose)) +
    ggplot2::geom_smooth(ggplot2::aes(y = aggregateAGPdata$sensorglucose), se = FALSE) +
    ggplot2::geom_point(ggplot2::aes(y = aggregateAGPdata$sensorglucose), shape = ".") +
    ggplot2::ggtitle("Aggregate Daily Overlay (LOESS Smoothing)") +
    ggplot2::ylab("Sensor BG (mg/dL)") +
    ggplot2::xlab("Time (hour)") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_datetime(labels = function(x) format(x, format = "%H:%M")) +
    ggplot2::ylim(yaxis[1], yaxis[2])

  grDevices::pdf(base::paste(outputdirectory, "/", "AGP_Tukey.pdf", sep = ""),
    width = 11, height = 8.5
  )
  graphics::plot(aggAGPtukey)
  grDevices::dev.off()

  grDevices::pdf(base::paste(outputdirectory, "/", "AGP_Loess_Subject.pdf", sep = ""),
    width = 11, height = 8.5
  )
  graphics::plot(AGPloess)
  grDevices::dev.off()

  grDevices::pdf(base::paste(outputdirectory, "/", "Aggregate_AGP_Loess.pdf", sep = ""),
    width = 11, height = 8.5
  )
  graphics::plot(aggAGPloess)
  grDevices::dev.off()
}
