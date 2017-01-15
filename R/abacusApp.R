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
  
  # loading stylesheet
  stylesheet <- paste(
    scan(system.file("extdata", "app_clean.css", package = "abacus"), what = "", sep = "\n"), collapse = " ")
  
  
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
    tags$head(tags$style(HTML(stylesheet))),
    tabItems(
      tabItem("tab_enter_tas", 
              UploadModalUI("enter_tas", open_modal = "btn_enter_tas"),
              fluidRow(
                box(title = "Settings",
                    UploadSettingsUI("upload_tas")
                ),
                column(6,
                    h4("Reference Account"),
                    uiOutput("uploadedReference"),
                    actionButton("btn_enter_tas", "Enter"),
                    verbatimTextOutput("test"),
                    textOutput("err_enter_tas")
                )
              ),
              fluidRow(
                box(title = "File Content", width = 12, 
                    DT::dataTableOutput("uploadedTable")
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
    # module for upload settings
    # displaying uploaded transactions object
    uploadedTas <- callModule(UploadSettings, "upload_tas", db = db)
    output$uploadedTable <- DT::renderDataTable({
      validate(need(!uploadedTas()$err, uploadedTas()$msg))
      Table(uploadedTas()$tas$Transactions, dom = "t", class = "stripe hover", pageLen = 10)
    })
    output$uploadedReference <- renderUI({
      validate(need(!uploadedTas()$err, uploadedTas()$msg))
      HTML(sprintf(
        "<dl><dt>Owner</dt><dd>%s</dd><dt>IBAN</dt><dd>%s</dd><dt>BIC</dt><dd>%s</dd><dt>Type</dt><dd>%s</dd></dl>",
        uploadedTas()$tas$Reference$account_owner, uploadedTas()$tas$Reference$account_iban,
        uploadedTas()$tas$Reference$account_bic, uploadedTas()$tas$Reference$type
      ))
    })
    output$test <- renderPrint(uploadedTas()$tas$Reference)
    
    
    # multiple pages modal
    callModule(UploadModal, "enter_tas", db = db)
  }
  
  #shinyApp(ui, server)
  runApp(list(ui = ui, server = server), launch.browser = TRUE)
}


