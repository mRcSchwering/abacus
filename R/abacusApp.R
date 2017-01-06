#' abacusApp
#'
#' The App
#'
#' @family application functions
#'
#' @param db              \code{chr} database file (full path with file name)
#' 
#' @return
#' 
#' @examples 
#'
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyBS
#' 
#' @export
#'
abacusApp <- function( db )
{
  if( !file.exists(db) ) stop("Database file does not exist")
  
  # global stuff
  options(database_file = "test.db")
  
  # for some reason modals dont work if I just import shinyBS
  library(shinyBS)
  
  # UI elements
  dashboard_sidebar <- dashboardSidebar(
    shinyjs::useShinyjs(),
    sidebarMenu(
      menuItem("Enter Transactions", tabName = "tab_enter_tas", icon = icon("home")),
      menuItem("Test", tabName = "test", icon = icon("gear"),
               collapsible = TRUE,
               menuSubItem("Test1", tabName = "test1"),
               menuSubItem("Test2", tabName = "test2")
      )
    )
  )
  
  dashboard_body <- dashboardBody(
    tabItems(
      tabItem("tab_enter_tas", 
              UploadModalUI("enter_tas", open_modal = "btn_enter_tas"),
              fluidRow(
                box(title = "Settings",
                    UploadSettingsUI("upload_tas")
                ),
                box(title = "Account Information",
                    p("Reference Account"),
                    p("The Information"),
                    p("Explanation for entering"),
                    actionButton("btn_enter_tas", "Enter"),
                    textOutput("err_enter_tas")
                )
              ),
              fluidRow(
                box(title = "File Content", width = 12,
                    verbatimTextOutput("tas")
                )
              )
      )
    )
  )
  
  ui <- dashboardPage(
    dashboardHeader(title = "abacus"),
    dashboard_sidebar,
    dashboard_body
  )
  
  # Server Logic
  server <- function( input, output, session ) 
  {
    session$onSessionEnded(stopApp)
    db <- getOption("database_file")
    
    # File Upload
    tas <- callModule(UploadSettings, "upload_tas", db = db)
    output$tas <- renderPrint(tas())
    
    
    # multiple pages modal
    callModule(UploadModal, "enter_tas", db = db)
  }
  
  #shinyApp(ui, server)
  runApp(list(ui = ui, server = server), launch.browser = TRUE)
}


