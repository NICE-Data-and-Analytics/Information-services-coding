library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(spsComps)
library(kableExtra) 

body <- dashboardBody(
  fluidRow(
    box(title = "ISRCTN", width = 4, solidHeader = TRUE, 
        fileInput(inputId = "inputfile", "", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")) %>% 
          bsPopover("click browse to upload .csv file", "then click download button"),
        p(),
        p(),
        downloadButton("report"), 
        
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "Trial converter"),
  dashboardSidebar(disable = TRUE),
  body
)


server <- function(input, output) {
  
  output$report <- downloadHandler(
    
    filename = "report.doc",
    content = function(file) {
      tempReport <- file.path(tempdir(), "ISRCTNcode.Rmd")
      file.copy("ISRCTNcode.Rmd", tempReport, overwrite = TRUE)
      params <- list(report.data = input$inputfile$datapath)
      rendered_report <- rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      file.copy(rendered_report, file)
    }
  )
}

shinyApp(ui, server)