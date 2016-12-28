
library(testthat)
test_dir("./inst/tests")
test_dir(system.file("tests", "", package = "abacus"))






library(abacus)

#### app
db <- "test.db"
library(abacus)
Create_testDB("test.db")
library(shiny)


fileUpload_modal <- tagList(
  fileInput("upload", "upload file", multiple = FALSE, accept = NULL),
  actionButton("button_uploaded", "continue")
)
fileCheck_modal <- tagList(
  div(h3("reference account"), DT::dataTableOutput("referenceAccount"), 
      helpText("Is this the correct reference account?")),
  div(h3("file content"), DT::dataTableOutput("fileContent"),
      helpText("Was the file read correctly?")),
  actionButton("button_checked", "continue")
)

ui <- fluidPage(sidebarLayout(

  sidebarPanel(h3("Status"), verbatimTextOutput("status"), shinyjs::useShinyjs()),
  
  mainPanel(
    h3("Enter New Transactions"),
    actionButton("button_started", "Enter New Transactions"),
    shinyBS::bsModal("fileupload", "File Upload", trigger = "button_started", size = "large", fileUpload_modal),
    shinyBS::bsModal("fileCheck", "File Check", trigger = "", size = "large", fileCheck_modal)
  )
))

server <-function(input, output, session) {
  fileCols <- list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10)
  refAccount <- "giro"
  
  status <- reactiveValues(tas_enter = "none")
  observeEvent(input$button_started, {
    status$tas_enter <- "started"
    shinyjs::disable("button_uploaded")
  })
  observeEvent(input$button_uploaded, {
    status$tas_enter <- "checked"
    shinyBS::toggleModal(session, "fileupload", "close")
    shinyBS::toggleModal(session, "fileCheck", "open")
  })
  
  uploaded <- eventReactive(input$upload, {
    fileInfo <- input$upload
    if(grepl("csv", fileInfo$type)) obj <- Read_csv(refAccount, fileInfo$datapath, fileCols, db)
    shinyjs::enable("button_uploaded")
    obj
  })
  
  observe(uploaded())
  
  output$referenceAccount <- DT::renderDataTable({
    validate(need(uploaded(), "No file uploaded yet"))
    uploaded()$Reference
  })
  
  output$fileContent <- DT::renderDataTable({
    validate(need(uploaded(), "No file uploaded yet"))
    uploaded()$Transactions
  }, options = list(autoWidth = TRUE, scrollX = TRUE) )
  
  observeEvent(input$button_checked, {
    print("Continue")
  })
  
  output$status <- renderPrint(status$tas_enter)
  
}

shinyApp(ui, server)






######################################

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


##

roxygen2::roxygenise()
devtools::install()


##










































###### ABT
# 100x11
# 1 cont feat
# 7 discr feat (counts), 4 of which sparse
# 2 binäre
# 5 classes
abt <- matrix(NA, 100, 11)
abt[, 11] <- rep(1:5, each = 20)
## random
rs <- 1:100
abt[rs, 1] <- sample(0:1000, length(rs), replace = TRUE)
abt[rs, 2:4] <- matrix(sample(1:10, length(rs) * 3, replace = TRUE), length(rs), 3)
abt[rs, 5:8] <- matrix(sample(c(rep(0, 9, replace = TRUE), 1:5), length(rs) * 4, replace = TRUE), length(rs), 4)
abt[rs, 9:10] <- matrix(sample(c(rep(0, 9, replace = TRUE), 1), length(rs) * 2, replace = TRUE), length(rs), 2)
## class 1: 1 > 1000
rs <- 1:20
abt[rs, 1] <- sample(1001:10000, length(rs), replace = TRUE)
## class 2: 3 > 5 and 9 = 1 
rs <- 21:40
abt[rs, 3] <- matrix(sample(5:10, length(rs) * 1, replace = TRUE), length(rs), 1)
abt[rs, 9] <- 1
## class 3: 2 as id: 2 = 4
rs <- 41:60
abt[rs, 2] <- 4
## class 4: 9:10 = 1 and 6 > 2
rs <- 61:80
abt[rs, 6] <- matrix(sample(3:5, length(rs) * 1, replace = TRUE), length(rs), 1)
abt[rs, 9:10] <- 1
## class 5: 2 as id: 2 = c(1, 5), 8 > 2
rs <- 81:100
abt[rs, 2] <- matrix(sample(c(1, 5), length(rs) * 1, replace = TRUE), length(rs), 1)
abt[rs, 8] <- matrix(sample(3:5, length(rs) * 1, replace = TRUE), length(rs), 1)



###### Training:


#### sda

library(sda)

ranked <- sda.ranking(abt, as.factor(ta$type))
plot(ranked)
trained <- sda(abt, as.factor(ta$type))
predicted <- predict(trained, abt)
strng <- 0.0 # beste 0.0 mit 0.85
post <- apply(predicted$posterior, 1, max)
sum(predicted$class[post > strng] == as.factor(ta$type)[post > strng])

plot(1:nrow(ranked), log(ranked[, 2]))
