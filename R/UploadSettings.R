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
  
  out <- tagList(
    uiOutput(ns("saved")),
    br(),
    uiOutput(ns("settings")),
    br(),
    fileInput(ns("upload"), "Upload Table", multiple = FALSE, accept = c("text", "csv")),
    p("Take a look at file contents and reference account information.
              If everything looks like expected continue by clicking ", strong("Enter"), ".")
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
  
  # load saved settings (newest first)
  settings <- SelectBLOB("Settings", "test.db")$upload
  settings <- rev(settings)
  
  # fallback in case nothing is saved
  if( is.null(settings) ){
    settings <- list(
      default = list(
        col = list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10),
        type = "giro"
      )
    )
  }
  
  # personal account types
  refAccounts <- Select("personalAccounts", db)$type
  
  # create ui for settings
  output$saved <- renderUI({
    choice <- names(settings)
    selectizeInput(ns("uploadSettings"), "Saved Settings", choice, selected = choice[1],
                   options = list(maxItems = 1, create = TRUE), multiple = TRUE)
  })
  
  # create ui for defining upload settings
  output$settings <- renderUI({
    selected <- input$uploadSettings
    
    # selection of reference account
    if( !is.null(selected) && selected %in% names(settings) && settings[[selected]]$type %in% refAccounts ){
      refChoices <- c(selected, refAccounts[refAccounts != selected])
    } else {
      refChoices <- refAccounts 
    }  
    sels <- selectizeInput(ns("accountType"), "Reference Account", refChoices, width = '200px', 
            selected = refChoices[1], options = list(maxItems = 1, create = TRUE), multiple = FALSE)
    sels <- div(style="display:inline-block", sels)

    # column mappings
    if( !is.null(selected) && selected %in% names(settings) ){
      fileCols <- settings[[selected]]$col
    } else {
      fileCols <- list(name = 1, iban = 1, bic = 1, date = 1, reference = 1, entry = 1, value = 1, currency = 1)
    }
    cols <- list(
         numericInput(ns("nameCol"), "name", fileCols$name, min = 1, width = '60px'),
         numericInput(ns("ibanCol"), "IBAN", fileCols$iban, min = 1, width = '60px'),
         numericInput(ns("bicCol"), "BIC", fileCols$bic, min = 1, width = '60px'),
         numericInput(ns("dateCol"), "date", fileCols$date, min = 1, width = '60px'),
         numericInput(ns("referenceCol"), "reference", fileCols$reference, min = 1, width = '60px'),
         numericInput(ns("entryCol"), "entry", fileCols$entry, min = 1, width = '60px'),
         numericInput(ns("valueCol"), "value", fileCols$value, min = 1, width = '60px'),
         numericInput(ns("currencyCol"), "name", fileCols$currency, min = 1, width = '60px')       
    )
    cols <- tagList(lapply(cols, function(x) div(style="display:inline-block", x)))
  
    # seperators
    seps <- list(
      selectInput(ns("colSep"), "column separator", list(tab = "\t", ";", ",", ":"), width = "100px"),
      selectInput(ns("decSep"), "decimal separator", list(".", ","), width = "100px")
    )
    seps <- tagList(lapply(seps, function(x) div(style="display:inline-block", x)))
    
    # skip lines
    skps <- list(
      numericInput(ns("skip"), "skip lines", min = 0, value = 0, width = "60px"),
      numericInput(ns("max"), "max lines", min = -1, value = -1, width = "60px")
    )
    skps <- tagList(lapply(skps, function(x) div(style="display:inline-block", x)))
    
    # other
    hd <- checkboxInput(ns("hdBl"), "1st line as column names", value = TRUE)
    hd <- div(style="display:inline-block", hd)
    
    style="margin-bottom: 3em;"
    
    tagList(div(sels, seps, hd), div(cols, skps))
  })
  
  return(TRUE)
}
