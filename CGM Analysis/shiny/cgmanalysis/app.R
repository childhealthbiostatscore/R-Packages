#-------------------------------------------------------------------------------
#
# This Shiny app takes an input of CGM data files and returns an ambulatory 
# glucose profile along with various CGM metrics. 
#
# v 1.0 
# Tim Vigers 
# 8/29/19
#
#-------------------------------------------------------------------------------

library(shiny)
library(ggplot2)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Input: Gap fill option
      numericInput("gapfill", "Maximum Gap Length to Fill (minutes):", 20),
      # Input: Remove gaps options
      radioButtons("removegaps", "Remove Gaps?",
                   choices = c("Yes" = T,
                               "No" = F),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),

      # Horizontal line 
      tags$hr(),
      # Download buttons
      downloadButton("downloadData", "Download Summary Measures"),
      downloadButton("downloadTukey", "Download Tukey Plot"),
      downloadButton("downloadLoess", "Download Loess Plot")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: Tabset w/ plots and summary
      tabsetPanel(type = "tabs",
                  tabPanel("Summary Measures", tableOutput("summary")),
                  tabPanel("AGP Tukey", plotOutput("tukey")),
                  tabPanel("AGP Loess",  plotOutput("loess"))
      )
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  data <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                            "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                            "Ydm HM","Ydm HMS","ydm HM","ydm HMS")
        filename <- F
        ext <- tools::file_ext(input$file1$datapath)
        enc <- base::as.character(readr::guess_encoding(input$file1$datapath)[1,1])
        if (ext == "txt") {
          table <- utils::read.table(input$file1$datapath,
                                     sep = "\t",
                                     skipNul = TRUE,
                                     header = TRUE,
                                     stringsAsFactors = FALSE,
                                     na.strings = "",
                                     fileEncoding = enc,
                                     comment.char = "")
        } else if (ext == "csv") {
          table <- utils::read.csv(input$file1$datapath,
                                   stringsAsFactors = FALSE,
                                   header = TRUE,
                                   na.strings = "",
                                   skipNul = TRUE,
                                   fileEncoding = enc)
        } else if (ext == "xls" | ext == "xlsx" | ext == "xlsm") {
          table <- readxl::read_excel(input$file1$datapath)
        }
        
        if (base::ncol(table) == 3 && base::colnames(table)[3] == "X" | base::ncol(table) == 2) {
          cgmtype <- "diasend"
        } else if (base::ncol(table) == 18) {
          cgmtype <- "libre"
        } else if (base::ncol(table) == 4) {
          cgmtype <- "libre pro"
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
          stop(base::paste("File '",input$file1$datapath,"' is formatted incorrectly, and the 
                       data cannot be read.",sep = ""))
        }
        
        # Format columns.
        if (cgmtype == "diasend") {
          if (filename == F) {
            id <- base::colnames(table)[2]
          } else {id <- sub("\\..*","",basename(input$file1$datapath))}
          table <- table[-c(1:base::which(table[,1] == "Time")),]
          base::colnames(table) <- c("timestamp","sensorglucose")
        } else if (cgmtype == "carelink") {
          if (filename == F) {
            id <- table$Patient.ID[1]
          } else {id <- sub("\\..*","",basename(input$file1$datapath))}
          table <- table[-c(1:6),]
          base::colnames(table) <- table[base::which(table[,3] == "Sensor")+1,]
          table <- table[-c(1:(base::which(table[,3] == "Sensor")+1)),]
          table <- table[-c(base::which(!is.na(table$`Event Marker`))),]
          table$timestamp <- base::paste(table$Date,table$Time)
          table$timestamp <- base::gsub('.{3}$',"",table$timestamp)
          table <- table[,c('timestamp','Sensor Glucose (mg/dL)')]
          base::colnames(table) <- c('timestamp','sensorglucose')
        } else if (cgmtype == "dexcom") {
          if ('Glucose.Value..mg.dL.' %in% colnames(table)) {
            if (filename == F) {
              id <- table$Patient.Info[3]
            } else {id <- sub("\\..*","",basename(input$file1$datapath))}
            if ('Timestamp..YYYY.MM.DDThh.mm.ss.' %in% colnames(table)) {
              table <- table[,c('Timestamp..YYYY.MM.DDThh.mm.ss.','Glucose.Value..mg.dL.')]
            } else {
              table <- table[,c('Timestamp..YYYY.MM.DD.hh.mm.ss.','Glucose.Value..mg.dL.')]
            }
            base::colnames(table) <- c('timestamp','sensorglucose')
            table$timestamp <- base::sub("T"," ",table$timestamp)
          } else {
            if (filename == F) {
              id <- table$PatientInfoValue[1]
            } else {id <- sub("\\..*","",basename(input$file1$datapath))}
            table <- table[,c("GlucoseDisplayTime","GlucoseValue")]
            base::colnames(table) <- c('timestamp','sensorglucose')
          }
        } else if (cgmtype == "libre") {
          if (filename == F) {
            id <- table[1,1]
          } else {id <- sub("\\..*","",basename(input$file1$datapath))}
          base::colnames(table) <- table[2,]
          table <- table[-c(1:2),]
          table <- table[,c("Meter Timestamp","Historic Glucose(mg/dL)")]
          base::colnames(table) <- c('timestamp','sensorglucose')
        } else if (cgmtype == "libre pro") {
          if (filename == F) {
            id <- table[1,1]
          } else {id <- sub("\\..*","",basename(input$file1$datapath))}
          base::colnames(table) <- table[2,]
          table <- table[-c(1:2),]
          table <- table[,c("Time","Historic Glucose (mg/dL)")]
          base::colnames(table) <- c('timestamp','sensorglucose')
        } else if (cgmtype == "manual") {
          if (filename == F) {
            id <- table[,1][1]
          } else {id <- sub("\\..*","",basename(input$file1$datapath))}
          table$sensorglucose <- 
            base::suppressWarnings(base::as.numeric(table$sensorglucose))
          table <- 
            table[-c(max(base::which(!is.na(table$sensorglucose)))+1:nrow(table)),]
          table <- table[,-c(1)]
        } else if (cgmtype == "ipro") {
          base::colnames(table) <- table[11,]
          if (filename == F) {
            id <- table[2,2]
          } else {id <- sub("\\..*","",basename(input$file1$datapath))}
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
        
        recordstart <- 
          base::strftime(table$timestamp[min(which(!is.na(table$sensorglucose)))],
                         format = "%m/%d/%Y %T")
        removaltime <- 
          base::strftime(table$timestamp[length(table$timestamp)],
                         format = "%m/%d/%Y %T")
        
        # Set interval based on mode of timestamp diff.
        interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp)))
        
        # Clean data (optional).
        if (input$removegaps == TRUE) {
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
          if (input$gapfill == TRUE) {
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
            stop(base::paste("File '",input$file1$datapath,"' does not have enough data and 
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
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(table)
    
  })
  # Summary measures
  summ <- reactive({
    f <- 1
    aboveexcursionlength = 35
    belowexcursionlength = 10
    magedef = "1sd"
    congan = 1
    daystart = 6
    dayend = 22
    format = "rows"
    # Make table
    cgmupload <- data.frame(matrix(nrow = 0,ncol = 1))
    # Define the order in which lubridate parses dates.  
    dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                        "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                        "Ydm HM","Ydm HMS","ydm HM","ydm HMS")
    allhours <- 0:23
    table <- data()
    # Remove duplicates
    table$subjectid <- table$subjectid[1]
    table <- unique(table)
    
    cgmupload["subject_id",f] <- as.character(table$subjectid[1])
    colnames(cgmupload) <- "Record"
    # Format columns.    
    table$timestamp <- 
      base::as.POSIXct(lubridate::parse_date_time(table$timestamp,
                                                  dateparseorder,tz = "UTC"))
    table$sensorglucose <- base::as.numeric(table$sensorglucose)
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp)))
    interval <- base::abs(interval)
    cgmupload["date_cgm_placement", f] <- 
      base::as.character(min(table$timestamp,na.rm = T))
    
    totaltime <- 
      base::as.numeric(base::difftime(base::max(table$timestamp, na.rm = T),
                                      base::min(table$timestamp,na.rm = T),
                                      units = "secs"))
    cgmupload["percent_cgm_wear",f] <- 
      base::round(((base::length(which(!is.na(table$sensorglucose)))/(totaltime/interval))*100),2)
    
    table <- table[,-c(1)]
    table <- table[stats::complete.cases(table),]
    
    cgmupload["num_days_good_data",f] <- 
      base::round(base::length(table$sensorglucose)/(86400/interval))
    cgmupload["total_sensor_readings",f] <- 
      base::as.numeric(base::length(base::which(!is.na(table$sensorglucose))))
    cgmupload["average_sensor",f] <- 
      base::mean(table$sensorglucose[base::which(!is.na(table$sensorglucose))],na.rm = T)
    cgmupload["estimated_a1c",f] <- 
      base::round((46.7 + (base::mean(table$sensorglucose[
        base::which(!is.na(table$sensorglucose))]))) / 28.7,digits = 1)
    cgmupload["gmi",f] <- base::round(3.31 + (0.02392 * base::mean(table$sensorglucose[
      base::which(!is.na(table$sensorglucose))])), digits = 1)
    cgmupload["q1_sensor",f] <- 
      base::as.numeric(base::summary(table$sensorglucose[
        base::which(!is.na(table$sensorglucose))])[2])
    cgmupload["median_sensor",f] <- 
      base::as.numeric(base::summary(table$sensorglucose[
        base::which(!is.na(table$sensorglucose))])[3])
    cgmupload["q3_sensor",f] <- 
      base::as.numeric(base::summary(table$sensorglucose[
        base::which(!is.na(table$sensorglucose))])[5])
    cgmupload["standard_deviation",f] <- 
      stats::sd(table$sensorglucose[base::which(!is.na(table$sensorglucose))])
    cgmupload["cv",f] <- 
      (stats::sd(table$sensorglucose[base::which(!is.na(table$sensorglucose))]))/
      base::mean(table$sensorglucose[base::which(!is.na(table$sensorglucose))])
    cgmupload["min_sensor",f] <- 
      base::min(table$sensorglucose[base::which(!is.na(table$sensorglucose))])
    cgmupload["max_sensor",f] <- 
      base::max(table$sensorglucose[base::which(!is.na(table$sensorglucose))])
    
    # Excursions over 120 calculations.
    BGover120 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover120[BGover120 < 120] <- 0
    BGover120[BGover120 >= 120] <- 1
    BG120.rle <- base::rle(BGover120)
    excursions120 <- 
      base::as.numeric(BG120.rle$lengths[base::which(BG120.rle$values == 1)])
    
    cgmupload["excursions_over_120",f] <- 
      base::length(base::which(excursions120 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_120",f] <- base::sum(BGover120) * (interval/60)
    cgmupload["percent_time_over_120",f] <- 
      ((base::sum(BGover120) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
    # Over 140.
    BGover140 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover140[BGover140 < 140] <- 0
    BGover140[BGover140 >= 140] <- 1
    BG140.rle <- base::rle(BGover140)
    excursions140 <- 
      base::as.numeric(BG140.rle$lengths[base::which(BG140.rle$values == 1)])
    
    cgmupload["excursions_over_140",f] <- 
      base::length(base::which(excursions140 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_140",f] <- base::sum(BGover140) * (interval/60)
    cgmupload["percent_time_over_140",f] <- 
      ((base::sum(BGover140) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
    # Over 180.
    BGover180 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover180[BGover180 < 180] <- 0
    BGover180[BGover180 >= 180] <- 1
    BG180.rle <- base::rle(BGover180)
    excursions180 <- 
      base::as.numeric(BG180.rle$lengths[base::which(BG180.rle$values == 1)])
    
    cgmupload["excursions_over_180",f] <- 
      base::length(base::which(excursions180 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_180",f] <- base::sum(BGover180) * (interval/60)
    cgmupload["percent_time_over_180",f] <- 
      ((base::sum(BGover180) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
    # Over 200.
    BGover200 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover200[BGover200 < 200] <- 0
    BGover200[BGover200 >= 200] <- 1
    BG200.rle <- base::rle(BGover200)
    excursions200 <- 
      base::as.numeric(BG200.rle$lengths[base::which(BG200.rle$values == 1)])
    
    cgmupload["excursions_over_200",f] <- 
      base::length(base::which(excursions200 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_200",f] <- base::sum(BGover200) * (interval/60)
    cgmupload["percent_time_over_200",f] <- 
      ((base::sum(BGover200) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
    cgmupload["avg_excur_over_140_per_day",f] <- 
      as.numeric(cgmupload["excursions_over_140",f])/
      as.numeric(cgmupload["num_days_good_data",f])
    cgmupload["avg_excur_over_200_per_day",f] <- 
      as.numeric(cgmupload["excursions_over_200",f])/
      as.numeric(cgmupload["num_days_good_data",f])
    
    # Over 250.
    BGover250 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGover250[BGover250 < 250] <- 0
    BGover250[BGover250 >= 250] <- 1
    BG250.rle <- base::rle(BGover250)
    excursions250 <- 
      base::as.numeric(BG250.rle$lengths[base::which(BG250.rle$values == 1)])
    
    cgmupload["excursions_over_250",f] <- 
      base::length(base::which(excursions250 > 
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload["min_spent_over_250",f] <- base::sum(BGover250) * (interval/60)
    cgmupload["percent_time_over_250",f] <- 
      ((base::sum(BGover250) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
    # Under 54.
    BGunder54 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGunder54[BGunder54 <= 54] <- 1
    BGunder54[BGunder54 > 54] <- 0
    BG54.rle <- base::rle(BGunder54)
    excursions54 <- 
      base::as.numeric(BG54.rle$lengths[base::which(BG54.rle$values == 1)])
    
    cgmupload["excursions_under_54",f] <- 
      base::length(base::which(excursions54 > 
                                 ((belowexcursionlength * 60)/interval)))
    cgmupload["min_spent_under_54",f] <- base::sum(BGunder54) * (interval/60)
    cgmupload["percent_time_under_54",f] <- 
      ((base::sum(BGunder54) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
    # Under 60.
    BGunder60 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(
        table$sensorglucose))],length = 1)
    BGunder60[BGunder60 <= 60] <- 1
    BGunder60[BGunder60 > 60] <- 0
    BG60.rle <- base::rle(BGunder60)
    excursions60 <- 
      base::as.numeric(BG60.rle$lengths[base::which(BG60.rle$values == 1)])
    
    cgmupload["excursions_under_60",f] <- 
      base::length(base::which(excursions60 > 
                                 ((belowexcursionlength * 60)/interval)))
    cgmupload["min_spent_under_60",f] <- base::sum(BGunder60) * (interval/60)
    cgmupload["percent_time_under_60",f] <- 
      ((base::sum(BGunder60) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
    # Under 70.
    BGunder70 <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))],
                       length = 1)
    BGunder70[BGunder70 <= 70] <- 1
    BGunder70[BGunder70 > 70] <- 0
    BG70.rle <- base::rle(BGunder70)
    excursions70 <- 
      base::as.numeric(BG70.rle$lengths[base::which(BG70.rle$values == 1)])
    
    cgmupload["excursions_under_70",f] <- 
      base::length(base::which(excursions70 > 
                                 ((belowexcursionlength * 60)/interval)))
    cgmupload["min_spent_under_70",f] <- base::sum(BGunder70) * (interval/60)
    cgmupload["percent_time_under_70",f] <- 
      ((base::sum(BGunder70) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
    # Time in range.
    BGinrange <- 
      base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))],
                       length = 1)
    BGinrange <- ifelse(BGinrange %in% 70:180, 1,0)
    cgmupload["min_spent_70_180",f] <- base::sum(BGinrange) * (interval/60)
    cgmupload["percent_time_70_180",f] <- 
      ((base::sum(BGinrange) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100
    
    # Find daytime AUC.
    daytime_indexes <- 
      base::which(base::as.numeric(base::format(table$timestamp,"%H")) %in% 
                    daystart:dayend)
    daytime_sensor <- table$sensorglucose[daytime_indexes]
    xaxis <- 
      base::seq(from = 0, length.out = base::length(daytime_sensor),by = 
                  (interval / 60))
    
    # Remove NAs if they are present.
    xaxis[base::which(is.na(daytime_sensor))] <- NA
    xaxis <- xaxis[!is.na(xaxis)]
    daytime_sensor <- daytime_sensor[!is.na(daytime_sensor)]
    aucs <- pracma::cumtrapz(xaxis,daytime_sensor)
    cgmupload["daytime_auc",f] <- aucs[base::length(daytime_sensor)]
    
    # TIR variables for daytime
    BGinrange <- ifelse(daytime_sensor %in% 70:180, 1,0)
    cgmupload["min_spent_70_180_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_70_180_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor < 54, 1,0)
    cgmupload["min_spent_under_54_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_under_54_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor < 60, 1,0)
    cgmupload["min_spent_under_60_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_under_60_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor < 70, 1,0)
    cgmupload["min_spent_under_70_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_under_70_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor > 180, 1,0)
    cgmupload["min_spent_over_180_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_over_180_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor > 200, 1,0)
    cgmupload["min_spent_over_200_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_over_200_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(daytime_sensor > 250, 1,0)
    cgmupload["min_spent_over_250_day",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_over_250_day",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(daytime_sensor) * (interval/60)) * 100
    
    # Other daytime sensor glucose variables.
    cgmupload["daytime_avg_sensor_glucose",f] <- 
      base::mean(stats::na.omit(daytime_sensor))
    cgmupload["daytime_min_sensor_glucose",f] <- base::min(daytime_sensor)
    cgmupload["daytime_max_sensor_glucose",f] <- base::max(daytime_sensor)
    cgmupload["daytime_sd",f] <- stats::sd(daytime_sensor)
    
    
    
    # Nighttime AUC.
    nighttime_indexes <- 
      base::which(base::as.numeric(base::format(table$timestamp,"%H")) %in% 
                    allhours[base::which(!(0:23 %in% daystart:dayend))])
    nighttime_sensor <- table$sensorglucose[nighttime_indexes]
    xaxis <- 
      base::seq(from = 0, length.out = base::length(nighttime_indexes),by = 
                  (interval / 60))
    
    # Day/night ratio.
    cgmupload["day_night_sensor_ratio",f] <- 
      base::round(base::length(daytime_sensor)/base::length(nighttime_sensor),1)
    
    # Remove NAs if they are present.
    xaxis[base::which(is.na(nighttime_sensor))] <- NA
    xaxis <- xaxis[!is.na(xaxis)]
    nighttime_sensor <- nighttime_sensor[!is.na(nighttime_sensor)]
    aucs <- pracma::cumtrapz(xaxis,nighttime_sensor)
    cgmupload["nighttime_auc",f] <- aucs[base::length(nighttime_sensor)]
    
    # TIR variables for nighttime
    BGinrange <- ifelse(nighttime_sensor %in% 70:180, 1,0)
    cgmupload["min_spent_70_180_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_70_180_night",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(nighttime_sensor < 54, 1,0)
    cgmupload["min_spent_under_54_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_under_54_night",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(nighttime_sensor < 60, 1,0)
    cgmupload["min_spent_under_60_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_under_60_night",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(nighttime_sensor < 70, 1,0)
    cgmupload["min_spent_under_70_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_under_70_night",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(nighttime_sensor > 180, 1,0)
    cgmupload["min_spent_over_180_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_over_180_night",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(nighttime_sensor > 200, 1,0)
    cgmupload["min_spent_over_200_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_over_200_night",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100
    
    BGinrange <- ifelse(nighttime_sensor > 250, 1,0)
    cgmupload["min_spent_over_250_night",f] <- base::sum(BGinrange,na.rm = T) * (interval/60)
    cgmupload["percent_time_over_250_night",f] <- 
      (base::sum(BGinrange,na.rm = T) * (interval/60))/(base::length(nighttime_sensor) * (interval/60)) * 100    
    
    # Other nighttime sensor glucose variables.
    cgmupload["nighttime_avg_sens_glucose",f] <- 
      base::mean(stats::na.omit(nighttime_sensor))
    cgmupload["nighttime_min_sens_glucose",f] <- base::min(nighttime_sensor)
    cgmupload["nighttime_max_sens_glucose",f] <- base::max(nighttime_sensor)
    cgmupload["nighttime_sd",f] <- stats::sd(nighttime_sensor)
    
    # Total AUC.
    sensorBG <- base::as.numeric(table$sensorglucose,length = 1)
    xaxis <- 
      base::seq(from = 0, length.out = base::length(sensorBG),by = 
                  (interval / 60))
    
    # Remove NAs if they are present.
    xaxis[base::which(is.na(sensorBG))] <- NA
    xaxis <- xaxis[!is.na(xaxis)]
    sensorBG <- sensorBG[!is.na(sensorBG)]
    aucs <- pracma::cumtrapz(xaxis,sensorBG)
    cgmupload["total_auc",f] <- aucs[base::length(sensorBG)]
    
    cgmupload["average_auc_per_day",f] <-
      base::as.numeric(cgmupload["total_auc",f]) /
      base::as.numeric(cgmupload["num_days_good_data",f])
    
    # AUC over 180.
    sensorover180 <- table$sensorglucose
    sensorover180 <- sensorover180[sensorover180 >= 180]
    sensorover180 <- sensorover180[!is.na(sensorover180)]
    xaxis <- 
      base::seq(from = 0, length.out = base::length(sensorover180),by = 
                  (interval / 60))
    
    # Calculate cumulative AUC, and subtract recatangle where length = 180 &
    # width = minutes.
    if (base::length(sensorover180) > 1) {
      aucs <- pracma::cumtrapz(xaxis,sensorover180)
      aucs <- 
        (aucs[base::length(sensorover180)]) - (xaxis[base::length(xaxis)] * 180)
      cgmupload["auc_over_180",f] <- aucs
    } else {
      cgmupload["auc_over_180",f] <- 0
    }
    cgmupload["average_auc_180",f] <-
      base::as.numeric(cgmupload["auc_over_180",f]) /
      base::as.numeric(cgmupload["num_days_good_data",f])
    
    # Calculate MAGE.
    # Smooth data using an exponentially weighted moving average, calculate SD of 
    # unsmoothed data.  
    table$smoothed <- 
      base::as.numeric(zoo::rollapply(zoo::zoo(table$sensorglucose), 9, 
                                      function(x) c(1,2,4,8,16,8,4,2,1) %*% 
                                        (x / 46),fill = NA))
    table$smoothed[1:4] <- base::mean(stats::na.omit(table$sensorglucose[1:4]))
    table$smoothed[(base::length(table$smoothed)-3):
                     base::length(table$smoothed)] <- 
      base::mean(table$sensorglucose[(base::length(table$sensorglucose)-3):
                                       base::length(table$sensorglucose)])
    
    sd <- stats::sd(table$sensorglucose)
    # Identify turning points, peaks, and nadirs.
    tpoints <- pastecs::turnpoints(table$smoothed)
    peaks <- base::which(tpoints[["peaks"]] == TRUE)
    pits <- base::which(tpoints[["pits"]] == TRUE)
    # Calculate the difference between each nadir and its following peak. If the     
    # data starts on a peak, remove it. Otherwise remove the final pit to create an 
    # even number of pits and peaks.
    if (tpoints[["firstispeak"]] == TRUE && base::length(peaks) != 
        base::length(pits)) {
      peaks <- peaks[2:base::length(peaks)]
    } else if (tpoints[["firstispeak"]] == FALSE && 
               base::length(peaks) != base::length(pits)) {
      pits <- pits[1:(base::length(pits)-1)]
    }
    differences <- table$sensorglucose[peaks] - table$sensorglucose[pits]
    
    # Calculate the average of the differences greater than the entire dataset 
    # SD, 2SD, etc.
    if (magedef == "1sd") {
      cgmupload["r_mage",f] <- 
        base::mean(stats::na.omit(differences[base::which(differences > sd)]))
    } else if (magedef == "1.5sd") {
      cgmupload["r_mage",f] <- 
        base::mean(stats::na.omit(differences[base::which(differences > 
                                                            (sd * 1.5))]))
    } else if ( magedef == "2sd") {
      cgmupload["r_mage",f] <- 
        base::mean(stats::na.omit(differences[base::which(differences > 
                                                            (sd * 2))]))
    } else {
      cgmupload["r_mage",f] <- 
        base::mean(stats::na.omit(differences[base::which(differences > 
                                                            magedef)]))
    }
    
    #J-index
    cgmupload["j_index",f] <- 
      0.001 * (base::mean(table$sensorglucose, na.rm = T) + 
                 stats::sd(table$sensorglucose, na.rm = T))^2
    # CONGA    
    n <- (congan * 3600)
    conga.times <- table$timestamp + n
    conga.times <- conga.times[!is.na(conga.times)]
    conga.times <- conga.times[base::order(conga.times)]
    conga.times <- conga.times[base::which(conga.times %in% table$timestamp)]
    begin.times <- conga.times - n
    congas <- table$sensorglucose[base::which(table$timestamp %in% conga.times)] - 
      table$sensorglucose[base::which(table$timestamp %in% begin.times)]
    cgmupload[base::paste0("conga_",congan),f] <- stats::sd(congas,na.rm = T)
    # MODD.
    table$time <- lubridate::round_date(table$timestamp,"5 minutes")
    table$time <- base::strftime(table$time, format = "%H:%M",tz = "UTC")
    moddtable <- 
      base::data.frame(base::matrix(ncol = 2,nrow = 
                                      base::length(unique(table$time))))
    base::colnames(moddtable) <- c("time","mean_differences")
    moddtable$time <- base::unique(table$time)
    # For each time, calculate differences (absolute values) and average them.   
    for (r in 1:nrow(moddtable)) {
      moddtable$mean_differences[r] <- 
        base::mean(base::abs(base::diff(table$sensorglucose[
          base::which(table$time == moddtable$time[r])])))
    }
    # Average the averages.
    cgmupload["modd",f] <- 
      base::mean(stats::na.omit(moddtable$mean_differences))
    
    # LBGI and HBGI (based on dc1386 appendix)
    a <- 1.084
    b <- 5.381
    y <- 1.509
    table$gluctransform <- y * ((base::log(table$sensorglucose)^a)-b)
    table$rBG <- 10 * (table$gluctransform^2)
    rl <- table$rBG[base::which(table$gluctransform < 0)]
    rh <- table$rBG[base::which(table$gluctransform > 0)]
    cgmupload["lbgi",f] <- base::mean(stats::na.omit(rl))
    cgmupload["hbgi",f] <- base::mean(stats::na.omit(rh))
    return(as.data.frame(cgmupload))
  })
  
  output$summary <- renderTable({summ()},striped = T,rownames = T)
  
  # Plots 
  tukey <- reactive({
    table <- data()
    dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                        "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                        "Ydm HM","Ydm HMS","ydm HM","ydm HMS")
    table$timestamp <- 
      lubridate::parse_date_time(table$timestamp,dateparseorder,tz = "UTC")
    table <- table[!is.na(table$sensorglucose),]
    table$hour <- lubridate::round_date(table$timestamp,"hour")
    table$time <- 
      as.POSIXct(strftime(table$timestamp,format = "%H:%M"),
                 format = "%H:%M")
    table$hourmin <- 
      lubridate::round_date(table$timestamp,"10 minutes")
    table$hourmin <- 
      as.POSIXct(strftime(table$hourmin,format = "%H:%M"),
                 format = "%H:%M")
    # Quartiles
    quartiles <- 
      data.frame(matrix(nrow = length(unique(table$hourmin)),ncol = 6))
    colnames(quartiles) <- 
      c("hourmin","sensorglucose5perc","sensorglucoseqone","sensorglucosemedian",
        "sensorglucoseqthree","sensorglucose95perc")
    quartiles$hourmin <- unique(table$hourmin)
    quartiles <- quartiles[order(quartiles$hourmin),]
    
    for (i in 1:nrow(quartiles)) {
      quartiles$sensorglucose5perc[i] <- 
        stats::quantile(as.numeric(table$sensorglucose[which(table$hourmin == quartiles$hourmin[i])]),0.05)
      quartiles$sensorglucoseqone[i] <- 
        as.numeric(summary(table$sensorglucose[which(table$hourmin == quartiles$hourmin[i])])[2])
      quartiles$sensorglucosemedian[i] <- 
        as.numeric(summary(table$sensorglucose[which(table$hourmin == quartiles$hourmin[i])])[3])
      quartiles$sensorglucoseqthree[i] <- 
        as.numeric(summary(table$sensorglucose[which(table$hourmin == quartiles$hourmin[i])])[5])
      quartiles$sensorglucose95perc[i] <- 
        stats::quantile(as.numeric(table$sensorglucose[which(table$hourmin == quartiles$hourmin[i])]),0.95)
    }
    
    quartiles$smooth5perc <- 
      as.numeric(stats::smooth(quartiles$sensorglucose5perc,kind = "3R",twiceit = TRUE))
    quartiles$smoothqone <- 
      as.numeric(stats::smooth(quartiles$sensorglucoseqone,kind = "3R",twiceit = TRUE))
    quartiles$smoothmed <- 
      as.numeric(stats::smooth(quartiles$sensorglucosemedian,kind = "3R",twiceit = TRUE))
    quartiles$smoothqthree <- 
      as.numeric(stats::smooth(quartiles$sensorglucoseqthree,kind = "3R",twiceit = TRUE))
    quartiles$smooth95perc <- 
      as.numeric(stats::smooth(quartiles$sensorglucose95perc,kind = "3R",twiceit = TRUE))
    # Plot
    AGPtukey <- ggplot2::ggplot(quartiles, ggplot2::aes(x = quartiles$hourmin))+ 
      ggplot2::geom_ribbon(ggplot2::aes(ymin = quartiles$smoothqone,ymax = quartiles$smoothqthree,fill = "Interquartile Range"),alpha = 0.5)+
      ggplot2::geom_line(ggplot2::aes(y = quartiles$smoothmed, color = "Median"))+
      ggplot2::geom_line(ggplot2::aes(y = quartiles$smooth95perc,linetype="5th & 95th Percentile"))+
      ggplot2::geom_line(ggplot2::aes(y = quartiles$smooth5perc),linetype="dashed")+
      ggplot2::ggtitle("Aggregate Daily Overlay (Tukey Smoothing)")+
      ggplot2::ylab("Sensor BG (mg/dL)")+
      ggplot2::xlab("Time (hour)")+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
      ggplot2::scale_x_datetime(labels = function(x) format(x, format = "%H:%M"))+
      ggplot2::scale_fill_manual("",values = "blue")+
      ggplot2::scale_color_manual("",values = "red")+
      ggplot2::scale_linetype_manual("",values = "dashed")+
      ggplot2::ylim(0,400)
    return(AGPtukey)
  })
  output$tukey <- renderPlot({tukey()})
  # Render Loess plot
  loess <- reactive({
    table <- data()
    dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                        "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                        "Ydm HM","Ydm HMS","ydm HM","ydm HMS")
    table$timestamp <- 
      lubridate::parse_date_time(table$timestamp,dateparseorder,tz = "UTC")
    table$time <- 
      as.POSIXct(strftime(table$timestamp,format = "%H:%M"),
                 format = "%H:%M")
    table <- table[!is.na(table$sensorglucose),]
    AGPloess <- 
      ggplot2::ggplot(table, ggplot2::aes(x = table$time, y = table$sensorglucose))+
      ggplot2::geom_smooth(ggplot2::aes(y = table$sensorglucose),se = FALSE)+
      ggplot2::geom_point(ggplot2::aes(y = table$sensorglucose),shape = ".")+
      ggplot2::ggtitle("Daily Overlay Per Subject (LOESS Smoothing)")+
      ggplot2::ylab("Sensor BG (mg/dL)")+
      ggplot2::xlab("Time (hour)")+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
      ggplot2::labs(colour = "Subject ID")+
      ggplot2::scale_x_datetime(labels = function(x) format(x, format = "%H:%M"))+
      ggplot2::ylim(0,400)
    return(AGPloess)
  })
  output$loess <- renderPlot({loess()})
  # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = "cgm_summary.csv",
    content = function(file) {
      write.csv(summ(),file,row.names = T)
    }
  )
  # Download Tukey plot
  output$downloadTukey <- downloadHandler(
    filename = "tukey.png",
    content = function(file) {
      png(file,width=8,height=6,units="in",res=1200)
      print(tukey())
      dev.off()
      
    })
  # Download Loess plot
  output$downloadLoess <- downloadHandler(
    filename = "loess.png",
    content = function(file) {
      png(file,width=6,height=6,units="in",res=1200)
      print(loess())
      dev.off()
      
    })
}

# Create Shiny app ----
shinyApp(ui, server)