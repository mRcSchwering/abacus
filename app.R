#### app
db <- "test.db"
library(abacus)
Create_testDB("test.db")
library(shiny)
library(shinydashboard)
library(shinyBS)

settings <- list(
  upload = list(
    default = list(
      col = list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10),
      type = "giro", date = "%d.%m.%Y", colSep = "\t", decSep = ",", head = TRUE, skip = 0, nMax = -1
    ),
    test = list(
      col = list(name = 7, iban = 6, bic = 3, date = 8, reference = 4, entry = 5, value = 10, currency = 9),
      type = "savings", date = "%Y-%m-%d", colSep = ";", decSep = ".", head = FALSE, skip = 1, nMax = 100
    )
  )
)

any("test.db" %in% names(options))
abacus::InsertBLOB("Settings", settings, "test.db")
abacus::UpdateBLOB("Settings", settings, "test.db")

library(abacus)
roxygen2::roxygenise()
devtools::install()
abacus::abacusApp("test.db")





# minimal app -------------------------------------------------------------
modalTitle <- "Test Modal"
nPages <- 4

ui <- fluidPage(
 
)

server <- function( input, output, session )
{
 
}

shinyApp(ui, server)








ui <- fluidPage(shinyjs::useShinyjs(), 
                
      )
server <- function(input, output, session) 
  callModule(UploadModal, "enter_tas", open_module = reactive(input$show_modal))

shinyApp(ui, server)



##################################


ui <- fluidPage(sidebarLayout(
  
  sidebarPanel(h3("Status"), verbatimTextOutput("progress"), shinyjs::useShinyjs()),
  
  mainPanel(
    fluidPage(
      h3("Enter New Transactions"),
      uiOutput("settings"), br(), 
      uiOutput("setting_referenceAccount"),
      uiOutput("setting_fileCols"),
      fluidRow(
        column(6,
               fileInput("upload", "upload file", multiple = FALSE, accept = c("text", "csv")),
               textOutput("error")
        ),
        column(6,
               DT::dataTableOutput("referenceAccount")
        )
      ),
      fluidRow(
        br(), h3("file content"), DT::dataTableOutput("fileContent")
      ),
      fluidRow(
        div("If this is the correct reference account and the file is displayed correctly
            proceed by clicking 'continue'. Otherwise retry with other upload settings.
            You can save the current settings by overwriting the current settings, or create a new one."),
        shinyjs::disabled(div(actionButton("start_upload", "continue"), 
                              actionButton("save_settings", "save settings"), id = "start_buttons"))
        )
  ),
  shinyBS::bsModal("modal", "New Transactions", trigger = "", size = "large", uiOutput("modal"))
  )
      ))

server <-function(input, output, session) {
  
  fileCols <- list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10)
  refAccount <- "giro"
  
  progress <- reactiveValues(status = "none", error = FALSE, msg = "")
  
  
  # create ui for settings
  output$settings <- renderUI({
    choice <- list("A" = "a", "B" = "b")
    selectizeInput("uploadSettings", "settings", choice, selected = "a",
                   options = list(maxItems = 1, create = TRUE), multiple = TRUE)
  })
  
  # create ui for reference account setting
  output$setting_referenceAccount <- renderUI({
    print(input$uploadSettings)
    choice <- switch(input$uploadSettings,
                     a = list(choiceA1 = 1, choiceA2 = 2),
                     b = list(choiceB1 = 1, choiceB2 = 2),
                     list(noChoice = 0))
    selectizeInput("accountType", "Reference Account", choice, 
                   options = list(maxItems = 1, create = TRUE), multiple = TRUE)
  })
  
  # create ui for file columns
  output$setting_fileCols <- renderUI({
    tagList(
      div(style="display:inline-block", numericInput("nameCol", "name", fileCols$name, min = 1, width = '100px')),
      div(style="display:inline-block", numericInput("ibanCol", "IBAN", fileCols$iban, min = 1, width = '100px')),
      div(style="display:inline-block", numericInput("bicCol", "BIC", fileCols$bic, min = 1, width = '100px')),
      div(style="display:inline-block", numericInput("dateCol", "date", fileCols$date, min = 1, width = '100px')),
      div(style="display:inline-block", numericInput("referenceCol", "reference", fileCols$reference, min = 1, width = '100px')),
      div(style="display:inline-block", numericInput("entryCol", "entry", fileCols$entry, min = 1, width = '100px')),
      div(style="display:inline-block", numericInput("valueCol", "value", fileCols$value, min = 1, width = '100px')),
      div(style="display:inline-block", numericInput("currencyCol", "name", fileCols$currency, min = 1, width = '100px'))       
    )
  })
  
  
  # file upload
  uploaded <- eventReactive(input$upload, {
    fileInfo <- input$upload
    
    obj <- try(Read_csv(refAccount, fileInfo$datapath, fileCols, db))
    if( class(obj) != "Transactions" ){
      progress$status <- "none"
      progress$msg <- "Either your file could not be uploaded or the reference account was not found"
      progress$error <- TRUE
      shinyjs::disable("start_buttons")
      return(NULL)
    }
    
    progress$status <- "uploaded"
    progress$msg <- ""
    progress$error <- FALSE
    shinyjs::enable("start_buttons")
    return(obj)
  })
  output$error <- renderText(progress$msg)
  
  
  # show uploaded file contents with reference account
  output$referenceAccount <- DT::renderDataTable({
    validate(need(uploaded(), ""))
    uploaded()$Reference
  }, options = list(dom = "t"), class = "stripe hover")
  output$fileContent <- DT::renderDataTable({
    validate(need(uploaded(), ""))
    uploaded()$Transactions
  }, options = list(dom = "tp", pageLength = 5, autoWidth = TRUE, scrollX = TRUE), class = "stripe hover" )
  
  
  # start modal
  observeEvent(input$start_upload, {
    if( progress$status == "uploaded" ){
      shinyBS::toggleModal(session, "modal", toggle = "open")
    }
  })
  
  
  # dynamic UI for the modal
  output$modal <- renderUI({
    out <- switch(progress$status,
                  uploaded = tagList(h1("Uploaded"))
    )
    out
  })
  
  output$progress <- renderPrint(progress$status)
  
}

shinyApp(ui, server)






# force reupload every time
tags$script(paste0('$( "#', ns("file"), '" ).on( "click", function() { this.value = null; });'))



## Idee: db als option temporär einbinden
withOptions <- function(optlist, expr)
{
  oldopt <- options(optlist)
  on.exit(options(oldopt))
  expr <- substitute(expr)
  eval.parent(expr)
} 
