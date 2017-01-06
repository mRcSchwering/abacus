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




ui <- fluidPage( TestUI("asd") )

server <- shinyServer(function(input, output, session) {})

shinyApp(ui, server)

TestUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(fileInput(ns("test1"), "test"), id = ns("test3")),
    textInput(ns("test2"), "asd"),
    bsPopover(ns("test3"), "Name1","The wait times will be brny equally spaced bins"),
    bsPopover(ns("test2"), "Name2","The wait times will be bris many equally spaced bins")
  )  
}


Test <- function(input, output, session) {}


######



## Idee: db als option temporär einbinden
withOptions <- function(optlist, expr)
{
  oldopt <- options(optlist)
  on.exit(options(oldopt))
  expr <- substitute(expr)
  eval.parent(expr)
} 
