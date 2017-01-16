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
  
  
  # global environment
  options(database_file = "test.db")
  
  
  # css stylesheet
  stylesheet <- paste(
    scan(system.file("extdata", "app_clean.css", package = "abacus"), what = "", sep = "\n"), collapse = " ")
  
  
  # for some reason modals dont work if I just import shinyBS
  library(shinyBS)
  
  
  # Dashboard Sidebar UI
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
  
  
  # Dashboard Body UI
  dashboard_body <- dashboardBody(
    tags$head(tags$style(HTML(stylesheet))),
    tabItems(
      tabItem("tab_enter_tas", 
        UploadModalUI("enter_tas", open_modal = "btn_enter_tas"),
        fluidRow(
          box(title = "Settings", UploadSettingsUI("upload_tas")),
          h4("Reference Account"),
          br(),
          uiOutput("uploadedReference"),
          br(),
          actionButton("btn_enter_tas", "Enter Transactions", class = "hlight"),
          helpText("Start writing the uploaded transactions into the database")
        ),
        fluidRow(
          div(class = "box-equal-div", 
            h4("File Content"),
            DT::dataTableOutput("uploadedTable")
          )
        )
      )
    )
  )
  
  
  # Combine UI Elements
  ui <- dashboardPage(
    dashboardHeader(title = "abacus"),
    dashboard_sidebar,
    dashboard_body
  )
  
  
  # Server Logic
  server <- function( input, output, session ) 
  {
    # Init
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
        uploadedTas()$tas$Reference$account_owner, 
        uploadedTas()$tas$Reference$account_iban,
        uploadedTas()$tas$Reference$account_bic, 
        uploadedTas()$tas$Reference$type
      ))
    })
    output$test <- renderPrint(uploadedTas()$tas$Reference)
    observe(
      if( uploadedTas()$err || is.null(uploadedTas()$tas) ){
        shinyjs::disable("btn_enter_tas")
      } else {
        shinyjs::enable("btn_enter_tas")
      }
    )
    
    
    # Enter Transactions
    # multiple pages modal for writing new transactions
    # and also new accounts
    callModule(UploadModal, "enter_tas", db = db)
  }
  
  
  # Run
  runApp(list(ui = ui, server = server), launch.browser = TRUE)
}


