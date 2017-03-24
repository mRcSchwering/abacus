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
abacusApp <- function(db)
{
  if (!file.exists(db)) stop("Database file does not exist")
  
  
  # global environment
  options(database_file = db)

  
  # for some reason modals dont work if I just do shinyBS::
  library(shinyBS)
  
  # read help Texts
  uploadSettings_helps <- .Read_html(
    system.file("www", "helptext_UploadSettings.html", package = "abacus")
  )
  enteringModal_helps <- .Read_html(
    system.file("www", "helptext_EnteringModal.html", package = "abacus")
  )
  
  # Dashboard Sidebar UI
  dashboard_sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Enter Transactions", 
               tabName = "tab_enter_tas", icon = icon("home")),
      menuItem("Test", 
               tabName = "test", icon = icon("gear"),
               collapsible = TRUE,
               menuSubItem("Test1", tabName = "test1"),
               menuSubItem("Test2", tabName = "test2")
      )
    )
  )
  
  
  # Dashboard Body UI
  dashboard_body <- dashboardBody(
    # head
    shinyjs::useShinyjs(),
    tags$head(includeCSS(system.file("www", "app_clean.css", 
                                     package = "abacus"))),
    tags$head(includeScript(system.file("www", "delay.js", 
                                        package = "abacus"))),

    # tabs
    tabItems(
      tabItem("tab_enter_tas", 
        EnteringModalUI(
          "enter_tas", 
          open_modal = "btn_enter_tas",
          helps = enteringModal_helps
        ),
        fluidRow(
          box(title = "Settings", 
              UploadSettingsUI("upload_tas", helps = uploadSettings_helps)),
          h4("Reference Account"),
          br(),
          uiOutput("uploadedReference"),
          br(),
          actionButton("btn_enter_tas", 
                       "Enter Transactions", class = "hlight"),
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
  server <- function(input, output, session) 
  {
    # Init
    session$onSessionEnded(stopApp)
    db <- getOption("database_file")
    
    
    # File Upload
    # module for upload settings
    # displaying uploaded transactions as table
    # show reference account information as description list
    uploadedTas <- callModule(
      UploadSettings, 
      "upload_tas", 
      db = db, 
      helps = uploadSettings_helps
    )
    output$uploadedTable <- DT::renderDataTable({
      validate(need(!uploadedTas()$err, uploadedTas()$msg))
      Table(uploadedTas()$tas$Transactions, dom = "t", 
            class = "stripe hover", pageLen = 10)
    })
    output$uploadedReference <- renderUI({
      validate(need(!uploadedTas()$err, uploadedTas()$msg))
      HTML(sprintf(
        paste0("<dl><dt>Owner</dt><dd>%s</dd><dt>IBAN</dt><dd>%s</dd>",
               "<dt>BIC</dt><dd>%s</dd><dt>Type</dt><dd>%s</dd></dl>"),
        uploadedTas()$tas$Reference$account_owner, 
        uploadedTas()$tas$Reference$account_iban,
        uploadedTas()$tas$Reference$account_bic, 
        uploadedTas()$tas$Reference$type
      ))
    })
    

    # Enter Transactions
    # multiple pages modal for writing new transactions
    # and also new accounts into db
    # can only be opened via button after successful file upload
    observe({
      if (uploadedTas()$err || is.null(uploadedTas()$tas)){
        shinyjs::disable("btn_enter_tas") 
      } else {
          shinyjs::enable("btn_enter_tas")
      }
    })
    callModule(
      EnteringModal, 
      "enter_tas", 
      open_modal = reactive(input$btn_enter_tas),
      tas = reactive(uploadedTas()$tas), 
      db = db
    )
    
    
    
  }
  
  
  # Run
  runApp(list(ui = ui, server = server), launch.browser = TRUE)
}


