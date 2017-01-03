#' UploadSettingsUI
#'
#' UploadModal is a modal with multiple pages for entering transactions into the database.
#' 
#' This is the UI part of the modal.
#' A modal with 4 pages is created.
#' \enumerate{
#'    \item Checking and correcting new accounts found in uploaded transactions
#'    \item Checking and correcting predictions made by machine learning algorithm about new transactions
#'    \item Noting whether duplicated transactions should be entered anyway
#'    \item Confirming sucessful finish
#' }
#'
#' @family application functions
#'
#' @param id              \code{chr} identifier used in shiny session 
#' @param open_modal      \code{reactive} for triggering the modal (e.g. \code{\link{shiny::actionButton}})
#' 
#' @return \code{chr} html code of UI
#' 
#' @examples 
#'
#'
#' @import shinyBS
#' 
#' @export
#'
UploadSettingsUI <- function( id ) 
{
  ns <- NS(id)
  
  # styles
  style <- tags$head(tags$style(
    HTML(paste0("
      .", ns("style_cols"), " { width: 60px; display:inline-block; }
      .", ns("style_skps"), " { width: 70px; display:inline-block; }
      .", ns("style_dt"), " { width: 150px; display:inline-block; margin-left: 20px; }
      .", ns("style_ref"), " { width: 200px; display:inline-block; }
      .", ns("style_seps"), " { width: 70px; display:inline-block; }
      .", ns("style_hd"), " { width: 120px; display:inline-block; margin-left: 20px; }
      .", ns("style_sepsBlock"), " { display:inline-block; margin-left: 20px; }
      .", ns("style_colsBlock"), " { display:inline-block; margin-left: 20px; }
      .", ns("style_mainBlock"), " { display:inline-block; }
      .", ns("style_upload"), " { display:inline-block; margin-left: 50px; }
      #", ns("btnSave"), " { margin-top: -27px; width: 70px; }
      #", ns("saved"), " { width: 200px; }
      .progress { height: 0px; }                              /* globally!!! */
      .", ns("style_settings"), " { font-size: 10px; }
    "))
  ))
  
  # ui elements
  out <- tagList(
    style,
    div(class = ns("style_mainBlock"), uiOutput(ns("saved"))),
    div(class = ns("style_mainBlock"), actionButton(ns("btnSave"), "Save", icon = icon("floppy-o"))),
    div(class = ns("style_upload"), fileInput(ns("upload"), "Upload Table", multiple = FALSE, accept = c("text", "csv"))),
    p("Above you can select settings you have already saved, below you can modify them."),
    br(),
    div(class = ns("style_settings"), uiOutput(ns("settings"))),
    br(),
    verbatimTextOutput(ns("test"))
  )
  
  return(out)
}












#' UploadSettings
#'
#' UploadModal is a modal with multiple pages for entering transactions into the database.
#' 
#' This is the server logic of the modal.
#' While the user goas through 4 pages in the modal following is done.
#' \enumerate{
#'    \item \code{\link{Read}} method extracts new accountss from \emph{transactions} object.
#'    \item \code{\link{Predict}} method enters new accounts into the database and 
#'    then predicts each transaction.
#'    \item \code{\link{Duplicated}} reads the database to see if transactions were already entered.
#'    \item \code{\link{Enter}} finally inserts new transaction into the database.
#' }
#' 
#'
#' @family application functions
#'
#' @param input           \code{list} from shiny session
#' @param output          \code{list} from shiny session
#' @param session         \code{list} from shiny session
#' @param db              \code{chr} of database file (full path and file name)
#' 
#' @return \code{TRUE}
#' 
#' @examples 
#'
#' 
#' @export
#'
UploadSettings <- function( input, output, session, db ) 
{
  ns <- session$ns
  
  # load saved settings (newest first) and reference types
  settings <- SelectBLOB("Settings", db)$upload
  settings <- rev(settings)
  refAccounts <- Select("personalAccounts", db)$type
  
  # fallback in case nothing is saved
  if( is.null(settings) ){
    settings <- list(
      default = list(
        col = list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10),
        type = "giro", date = "%Y-%m-%d", colSep = "\t", decSep = ".", head = TRUE, skip = 0, nMax = -1
      )
    )
  }
  
  # create ui for saved settings
  output$saved <- renderUI({
    choice <- names(settings)
    selectizeInput(ns("uploadSettings"), "Saved Settings", choice, selected = choice[1],
                   options = list(maxItems = 1, create = TRUE), multiple = TRUE)
  })
  
  # create ui for defining upload settings
  output$settings <- renderUI({
    saved <- input$uploadSettings
    saved <- if( !is.null(saved) && saved %in% names(settings) ) settings[[saved]] else NULL
    
    # selection for reference account
    type <- if( !is.null(saved) ) saved$type else ""
    selected <- if( type %in% refAccounts ) type else refAccounts[1]
    ref <- selectInput(ns("accountType"), "reference account", refAccounts, selected = selected)
    ref <- div(class = ns("style_ref"), ref)

    # column mappings
    if( !is.null(saved) ){
      fileCols <- saved$col
    } else {
      fileCols <- list(name = 1, iban = 1, bic = 1, date = 1, reference = 1, entry = 1, value = 1, currency = 1)
    }
    cols <- list(
         numericInput(ns("nameCol"), "name", fileCols$name, min = 1),
         numericInput(ns("ibanCol"), "IBAN", fileCols$iban, min = 1),
         numericInput(ns("bicCol"), "BIC", fileCols$bic, min = 1),
         numericInput(ns("dateCol"), "date", fileCols$date, min = 1),
         numericInput(ns("referenceCol"), "reference", fileCols$reference, min = 1),
         numericInput(ns("entryCol"), "entry", fileCols$entry, min = 1),
         numericInput(ns("valueCol"), "value", fileCols$value, min = 1),
         numericInput(ns("currencyCol"), "name", fileCols$currency, min = 1)       
    )
    cols <- tagList(lapply(cols, function(x) div(class = ns("style_cols"), x)))
  
    # seperators
    selected <- if( !is.null(saved) ) c(saved$colSep, saved$decSep) else c("\t", ".")
    seps <- list(
      selectInput(ns("colSep"), "column separator", list(tab = "\t", ";", ",", ":"), selected = selected[1]),
      selectInput(ns("decSep"), "decimal separator", list(".", ","), selected = selected[2])
    )

    seps <- tagList(lapply(seps, function(x) div(class = ns("style_seps"), x)))
    
    # skip lines
    selected <- if( !is.null(saved) ) c(saved$skip, saved$nMax) else c(0, -1)
    skps <- list(
      numericInput(ns("skip"), "skip lines", min = 0, value = selected[1]),
      numericInput(ns("max"), "max lines", min = -1, value = selected[2])
    )
    skps <- tagList(lapply(skps, function(x) div(class = ns("style_skps"), x)))
    
    # head row
    selected <- if( !is.null(saved) ) saved$head else TRUE
    hd <- checkboxInput(ns("hdBl"), "1st line as column names", value = selected)
    hd <- div(class = ns("style_hd"), hd)

    # date formats
    dateForms <- list(examples = c("%d.%m.%Y", "%Y-%m-%d", "%Y/%m/%d"))
    if( !is.null(saved) ) dateForms[["saved"]] <- saved$date
    dt <- selectizeInput(ns("dateFormat"), "date format", dateForms, selected = dateForms[[length(dateForms)]],
                         options = list(maxItems = 1, create = TRUE), multiple = TRUE)
    dt <- div(class = ns("style_dt"), dt)
    
    # arrange in 2 rows
    tagList(
      div(class = ns("style_row"), ref, dt, div(class = ns("style_sepsBlock"), seps), hd), 
      div(class = ns("style_row"), cols, div(class = ns("style_colsBlock"), skps))
    )
  })
  
  # read table
  tas <- reactive({
    file <- input$upload$datapath
    if( is.null(file) ) return(list(obj = NULL, err = TRUE, msg = "No table uploaded yet"))
    cols <- list(name = input$nameCol, iban = input$ibanCol, bic = input$bicCol, date = input$dateCol, 
         reference = input$referenceCol, entry = input$entryCol, value = input$valueCol, currency = input$currencyCol)
    res <- try(
      Read_csv(input$accountType, file, cols, db, head = input$hdBl, colSep = input$colSep,
               decSep = input$decSep, nSkip = input$skip, nMax = input$max, dateFormat = input$dateFormat)
    )
    
    # catch errors
    if( inherits(res, "try-error") ){
      out <- list(obj = NULL, err = TRUE, msg = res[1])
    } else {
      out <- list(obj = res, err = FALSE, msg = "")
    }
    
    out
  })
  
  output$test <- renderPrint(tas())
  
  return(TRUE)
}
