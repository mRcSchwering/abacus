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
TestUI <- function(id) {
  ns <- NS(id)
  tagList(
    bsModal(ns("modal"), "foo", trigger = "", "bar"),
    actionButton(ns("button"), "Show modal")
  )
}
Test <- function(input, output, session) {
  ns <- session$ns
  observeEvent(input$button, {
    print("toggle")
    toggleModal(session, "modal", "open")
  })
}

ui <- basicPage(
  TestUI("test")
)

server = function(input, output, session) {
  callModule(Test, "test")
}

shinyApp(ui, server)






##################################




ui <- fluidPage( 
  #TestUI("asd") 
  uiOutput("uiOut")
)

server <- shinyServer(function(input, output, session) {
  #callModule(Test, "asd")
  output$uiOut <- renderUI(popify(textInput("txt", "txt"),
         "popify title", "content"))
})

shinyApp(ui, server)

TestUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("test"), "test")
  )  
}

Test <- function(input, output, session) {
  ns <- session$ns
  shinyBS::addPopover(session, ns("test"), "Test", "content")
}


######



## Idee: db als option temporär einbinden
withOptions <- function(optlist, expr)
{
  oldopt <- options(optlist)
  on.exit(options(oldopt))
  expr <- substitute(expr)
  eval.parent(expr)
} 
