library(shiny)
library(lubridate)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Initials
  initials <- reactive({
    validate(
      need(input$initials != "", "Don't forget to enter initials!")
    )
    toupper(input$initials)
  })
  
  outputFileNames <- reactive({
    tmsp <- gsub(":", "_", Sys.time())
    tmsp <- gsub(" ", "_", tmsp)
    directory <- paste("Psychophys_Tutorial_A", initials(), tmsp, sep="_")
    archive <- paste("SLEEP_TutoralA_", gsub(".csv", "", sleepIntervals()$name), initials(), "_", tmsp, "_Archive", ".csv", sep="")
    output <- paste("SLEEP_TutoralA_", gsub(".csv", "", sleepIntervals()$name), "_", initials(), "_", tmsp, ".csv", sep="")
    
    list(outputDirectory=directory, outputArchive=archive, outputCorrected=output)
  })
  
  output$validateOutputDirectory <- renderUI({
    HTML(paste("<font color=\"green\">", outputFileNames()["outputDirectory"], "</font><br/>", helpText("You can find this directory on your Desktop."), sep = ''))
  })
  
  # Sleep intervals
  sleepIntervals <- reactive({
    input$sleepIntervals
  })
  
  output$validateSleepIntervals <- renderUI({
    if(length(sleepIntervals()$datapath)== 0){
      HTML("<font color=\"red\">Don't forget to upload sleep intervals data!</font>")
    } else{
      HTML(paste("<font color=\"green\"> Successfully uploaded: <strong>", sleepIntervals()$name, "</strong></font>"))
    }
  })
  
  macro0 <- eventReactive(input$runA, {
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Running Macro 0...", value = 10)
    
    # Read in data
    sleepIntervalsOrig <- read.csv(sleepIntervals()$datapath, sep=",", header=TRUE, stringsAsFactors = FALSE)
    sleepIntervalsData <- sleepIntervalsOrig
    sleepIntervalsData$StartTimeL <- parse_date_time(as.character(sleepIntervalsData[,"start_time"]), "HMS")
    sleepIntervalsData$StartDateL <- parse_date_time(as.character(sleepIntervalsData[,"start_date"]), "ymd")
    
    sleepIntervalsData$StartDate_X <- NA
    sleepIntervalsData$StartDay_X <- NA

    for(i in 1:nrow(sleepIntervalsData)){
      if(is.na(hour(sleepIntervalsData$StartTimeL[i]))){
        sleepIntervalsData$StartDay_X[i] <- NA
      } else{
        if(hour(sleepIntervalsData$StartTimeL[i]) >= 0 & hour(sleepIntervalsData$StartTimeL[i]) <=6){
          newdate <- sleepIntervalsData$StartDateL[i] - 1
          sleepIntervalsData$StartDate_X[i] <- format(newdate, "%Y-%m-%d")
          sleepIntervalsData$StartDay_X[i] <- weekdays(newdate)
        } else{
          newdate <- sleepIntervalsData$StartDateL[i]
          sleepIntervalsData$StartDate_X[i] <- format(newdate, "%Y-%m-%d")
          sleepIntervalsData$StartDay_X[i] <- weekdays(newdate)
        }
      }
    }

    sleepIntervalsData$Summer_X <- 0
    for(k in 1:nrow(sleepIntervalsData)){
      d <- sleepIntervalsData$StartDate_X[k]
      if(is.na(d) == FALSE){
        m <- month(d)
        if(m >= 7 & m <= 8){
          sleepIntervalsData$Summer_X[which(sleepIntervalsData$subject_id == sleepIntervalsData$subject_id[k])] <- 1
          # sleepIntervalsData$Summer_X[k] <- 1
        }
      }
    }

    drop <- c("StartTimeL","StartDateL")
    sleepIntervalsData <- sleepIntervalsData[ , !(names(sleepIntervalsData) %in% drop)]
    # names(sleepIntervalsData)[1:29] <- c("AnalysisName","SubjectID","HealthyHeart","DataStartDate","DataStartTime","IntervalType","Interval_Type","Interval#","StartDate","StartDay","StartTime","EndDate","EndDay","EndTime","Duration","InvTimeSWmin","InvalidSW","OnsetLatency","SnoozeTimemin","Efficiency","WASO","WakeTime","Wake","@#WakeBouts","AvgWakeBmin","SleepTime","Sleep","@#SleepBouts","AvgSleepBmin")

    # cat("Creating apoloypse date and time variables...\n")
    midnight1852 <- as.POSIXct('1852-01-01 00:00:00')
    today <- as.POSIXct('2012-05-20 00:00:00 EST')

    # DataStartDate
    datastartdate <- as.character(sleepIntervalsData[,"data_start_date"])
    datastartdate <- parse_date_time(datastartdate, "ymd")
    sleepIntervalsData[,"DataStartDate_A"] <- format(datastartdate, "%y-%m-%d")
    sleepIntervalsData[,"DataStartDate_B"] <- format(datastartdate, "%Y %b %d")
    sleepIntervalsData[,"DataStartDate_Ba"] <- format(datastartdate, "%Y")
    sleepIntervalsData[,"DataStartDate_Bb"] <- format(datastartdate, "%b")
    sleepIntervalsData[,"DataStartDate_Bc"] <- format(datastartdate, "%d")
    sleepIntervalsData[,"DataStartDate_C"] <- as.numeric((datastartdate - midnight1852) * 86400)

    # DataStartTime
    sleepIntervalsData[,"DataStartTime_A"] <- sleepIntervalsData[,"data_start_time"]
    sleepIntervalsData[,"DataStartTime_C"] <-  sapply(strsplit(as.character(sleepIntervalsData[,"DataStartTime_A"]),":"),function(x) {
      as.integer(x[1]) + (as.integer(x[2]) / 60) + (as.integer(x[3]) / 3600)
    }
    )
    sleepIntervalsData[,"DataStartTime_B"] <- format(today + (3600* as.numeric(sleepIntervalsData[,"DataStartTime_C"])), "%H:%M.%S")

    # StartDate
    startdate <- as.character(sleepIntervalsData[,"start_date"])
    startdate <- parse_date_time(startdate, "ymd")
    sleepIntervalsData[,"StartDate_A"] <- format(startdate, "%y-%m-%d")
    sleepIntervalsData[,"StartDate_B"] <- format(startdate, "%Y %b %d")
    sleepIntervalsData[,"StartDate_Ba"] <- format(startdate, "%Y")
    sleepIntervalsData[,"StartDate_Bb"] <- format(startdate, "%b")
    sleepIntervalsData[,"StartDate_Bc"] <- format(startdate, "%d")
    sleepIntervalsData[,"StartDate_C"] <- as.numeric((startdate - midnight1852) * 86400)

    # StartTime
    sleepIntervalsData[,"StartTime_A"] <- sleepIntervalsData[,"start_time"]
    sleepIntervalsData[,"StartTime_C"] <-  sapply(strsplit(as.character(sleepIntervalsData[,"StartTime_A"]),":"), function(x) {
      as.integer(x[1]) + (as.integer(x[2]) / 60) + (as.integer(x[3]) / 3600)
    }
    )
    sleepIntervalsData[,"StartTime_B"] <- format(today + (3600* as.numeric(sleepIntervalsData[,"StartTime_C"])), "%H:%M.%S")

    # EndDate
    enddate <- as.character(sleepIntervalsData[,"end_date"])
    enddate <- parse_date_time(enddate, "ymd")
    sleepIntervalsData[,"EndDate_A"] <- format(enddate, "%y-%m-%d")
    sleepIntervalsData[,"EndDate_B"] <- format(enddate, "%Y %b %d")
    sleepIntervalsData[,"EndDate_Ba"] <- format(enddate, "%Y")
    sleepIntervalsData[,"EndDate_Bb"] <- format(enddate, "%b")
    sleepIntervalsData[,"EndDate_Bc"] <- format(enddate, "%d")
    sleepIntervalsData[,"EndDate_C"] <- as.numeric((enddate - midnight1852) * 86400)

    # EndTime
    sleepIntervalsData[,"EndTime_A"] <- sleepIntervalsData[,"end_time"]
    sleepIntervalsData[,"EndTime_C"] <-  sapply(strsplit(as.character(sleepIntervalsData[,"EndTime_A"]),":"), function(x) {
      as.integer(x[1]) + (as.integer(x[2]) / 60) + (as.integer(x[3]) / 3600)
    }
    )
    sleepIntervalsData[,"EndTime_B"] <- format(today + (3600* as.numeric(sleepIntervalsData[,"EndTime_C"])), "%H:%M.%S")

    # StartDate_X
    startdate_x <- as.character(sleepIntervalsData[,"StartDate_X"])
    startdate_x <- parse_date_time(startdate, "ymd")
    sleepIntervalsData[,"StartDate_X_A"] <- format(startdate_x, "%y-%m-%d")
    sleepIntervalsData[,"StartDate_X_B"] <- format(startdate_x, "%Y %b %d")
    sleepIntervalsData[,"StartDate_X_Ba"] <- format(startdate_x, "%Y")
    sleepIntervalsData[,"StartDate_X_Bb"] <- format(startdate_x, "%b")
    sleepIntervalsData[,"StartDate_X_Bc"] <- format(startdate_x, "%d")
    sleepIntervalsData[,"StartDate_X_C"] <- as.numeric((startdate_x - midnight1852) * 86400)

    if(Sys.info()["sysname"] == "Windows"){
      archivePath <-  paste(strsplit(getwd(), "Desktop")[[1]][1], "Desktop/psychophys/Tutorial_A_Derive_Sleep_Apocalpyse/archive", outputFileNames()["outputDirectory"], sep="/")
      outputPath <-  paste(strsplit(getwd(), "Desktop")[[1]][1], "Desktop", outputFileNames()["outputDirectory"], sep="/")
    } else{
      archivePath <-  paste("~/Desktop/psychophys/Tutorial_A_Derive_Sleep_Apocalpyse/archive", outputFileNames()["outputDirectory"], sep="/")
      outputPath <-  paste("~/Desktop", outputFileNames()["outputDirectory"], sep="/")
    }

    dir.create(path = archivePath)
    SleepIntervalsArchive <- paste(archivePath, outputFileNames()["outputArchive"],sep="/")
    write.table(sleepIntervalsOrig, file=SleepIntervalsArchive, row.names = FALSE, sep=",")
    SleepIntervalsCorrected <- paste(archivePath, outputFileNames()["outputCorrected"],sep="/")
    write.table(sleepIntervalsData, file=SleepIntervalsCorrected, row.names = FALSE, sep=",")

    dir.create(path = outputPath)
    SleepIntervalsArchiveOutput <- paste(outputPath, outputFileNames()["outputArchive"],sep="/")
    write.table(sleepIntervalsOrig, file=SleepIntervalsArchiveOutput, row.names = FALSE, sep=",")
    SleepIntervalsCorrectedOutput <- paste(outputPath, outputFileNames()["outputCorrected"],sep="/")
    write.table(sleepIntervalsData, file=SleepIntervalsCorrectedOutput, row.names = FALSE, sep=",")

    successMsg <- "<em>Successfully Completed Tutorial A: Derive Sleep & Apocalyse Variables!</em>"
    filesMsg <- paste("Created: <strong>", outputFileNames()["outputCorrected"],"</strong> </br>in <strong>", outputFileNames()["outputDirectory"], "</strong> on Desktop.")
    HTML(paste("<font color=\"green\">", successMsg, "</br></br>" , filesMsg, "</font>"), sep="")

    
  })
  
  output$consoleA <- renderUI({
    macro0()
  })
  
  
  
  
})
