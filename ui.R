library(shiny)

shinyUI(
  
  navbarPage("Psychophys",
             tabPanel("Tutorial A: Derive Sleep & Apocalyse Variables",
                        tags$head(
                          tags$style(HTML(".shiny-output-error-validation {color: red;} "))
                          ),
                        sidebarLayout(
                          sidebarPanel(
                            # strong("Tutorial A: Derive Sleep & Apocalyse Variables"),
                            # br(),
                            # br(),
                            helpText("This tutorial is for correcting dates from sleep intervals data where participant slept after midnight."),
                            br(),
                            strong("Output files will be written to:"),
                            htmlOutput("validateOutputDirectory"),
                            br(),
                            strong("Sleep intervals data:"),
                            htmlOutput("validateSleepIntervals"),
                            br(),
                            actionButton("runA", "Run Tutorial A")
                            ),
                          mainPanel(
                            textInput("initials", "Step 1: Enter Initials", placeholder="e.g., MYI"),
                            helpText("Initials will be used for naming files. No spaces, limit to 5 characters and do not include any special characters (e.g., !@#$)."),
                            br(),
                            fileInput("sleepIntervals", "Step 2: Upload sleep intervals data",
                                      accept = c("csv", ".csv")
                                      ),
                            helpText("The sleep intervals data should be in .csv format and contains..."),
                            br(),
                            htmlOutput("consoleA")
                          )
                          ),
                        hr(),
                        h6("Pediatric Public Health Psychology Lab (PPHP), Concordia Unviersity")
                        )
             )
  )