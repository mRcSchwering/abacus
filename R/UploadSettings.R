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
  
  # force reupload every time
  re <- tags$script(paste0('$( "#', ns("upload"), '" ).on( "click", function() { this.value = null; });'))
  
  # styles
  style <- tags$head(tags$style(
    HTML(paste0("
      .", ns("style_cols"), " { width: 60px; display:inline-block; }
      .", ns("style_skps"), " { width: 70px; display:inline-block; }
      .", ns("style_dt"), " { width: 150px; display:inline-block; margin-left: 20px; }
      .", ns("style_ref"), " { width: 200px; display:inline-block; }
      .", ns("style_seps"), " { width: 60px; display:inline-block; }
      .", ns("style_hd"), " { width: 120px; display:inline-block; margin-left: 20px; }
      .", ns("style_sepsBlock"), " { display:inline-block; margin-left: 20px; }
      .", ns("style_colsBlock"), " { display:inline-block; margin-left: 20px; }
      .", ns("style_mainBlock"), " { display:inline-block; }
      .", ns("style_upload"), " { display:inline-block; margin-left: 50px; }
      #", ns("btnSave"), " { margin-top: -27px; width: 70px; }
      #", ns("saved"), " { width: 200px; }
      .progress { height: 0px; }                              /* globally!!! */
      .", ns("style_settings"), " { border: 1px solid WhiteSmoke; padding: 5px; }
    "))
  ))
  
  # save confirmation modal
  modal <- shinyBS::bsModal(id = ns("settingsSaved"), title = "Settings Saved", trigger = ns("btnSave"), 
                   "Current settings were saved. They will be visible when the app is restarted.")
  
  # input elements
  el <- tagList(
    div(class = ns("style_mainBlock"), uiOutput(ns("saved"))),
    div(class = ns("style_mainBlock"), actionButton(ns("btnSave"), "Save", icon = icon("floppy-o"))),
    div(class = ns("style_upload"), id = ns("div_upload"),
        fileInput(ns("upload"), "Upload Table", multiple = FALSE, accept = c("text", "csv"))),
    p("Here, you can enter new transaction by uploading a csv/txt table.",
      "Above you can select settings you have already saved, below you can modify them.",
      "Once you upload a table you will see how it is interpreted below.",
      "Any changes in the settings will take place immediately, so you can try around.",
      "If all columns are displayed correctly and the correct reference account is set",
      "procedd by clicking on", strong("Enter Transactions")),
    div(class = ns("style_settings"), uiOutput(ns("settings")))
  )
  
  # tooltips
  tt <- tagList(
    shinyBS::bsPopover(ns("btnSave"), "Save Current Settings", paste(
      "Saves the current settings. The settings name will be overwritten.",
      "You can change the settings name with the selection on the left of this button.",
      "Changes will take effect after the app was restarted."
    ), placement = "right"),
    shinyBS::bsPopover(ns("uploadSettings"), "Saved Settings", paste(
      "Here, you can chose upload settings that were already saved by their name.",
      "You can save the current settings by clicking the button on the right.",
      "This will overwrite the current setting name.",
      "You can enter a new setting name by writing something into the box and clicking <b>Add</b>.",
      "Changes will take effect after the app was restarted."
    ), placement = "right"),
    shinyBS::bsPopover(ns("div_upload"), "Upload Table with Transactions", paste(
      "Upload a table (csv/text) with transactions.",
      "Below you can change the upload settings.",
      "If the table was uploaded correctly, you can proceed by clicking the <b>Enter Transactions</b> button.",
      "If you have to use these upload settings more often, you can save them with the button on the left."
    ), placement = "right")
  )
  
  return(tagList(re, style, modal, el, tt))
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
  allSettings <- SelectBLOB("Settings", db)
  settings <- allSettings$upload
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
    ref <- shinyBS::popify(selectInput(ns("accountType"), "reference account", refAccounts, selected = selected),
      "Reference Account Type", "To which of your accounts do these transactions belong to?", placement = "right")
    ref <- div(class = ns("style_ref"), ref)

    # column mappings
    if( !is.null(saved) ){
      fileCols <- saved$col
    } else {
      fileCols <- list(name = 1, iban = 1, bic = 1, date = 1, reference = 1, entry = 1, value = 1, currency = 1)
    }
    cols <- list(
      shinyBS::popify(numericInput(ns("nameCol"), "name", fileCols$name, min = 1),
        "Name Column", "Which column in the table contains the account owner´s names?", placement = "right"),
      shinyBS::popify(numericInput(ns("ibanCol"), "IBAN", fileCols$iban, min = 1),
        "IBAN Column", "Which columns in the table contains the account IBAN?", placement = "right"),
      shinyBS::popify(numericInput(ns("bicCol"), "BIC", fileCols$bic, min = 1),
        "BIC Column", "Which columns in the table contains the account BIC?", placement = "right"),
      shinyBS::popify(numericInput(ns("dateCol"), "date", fileCols$date, min = 1),
        "Date Column", "Which columns in the table contains the transaction date?", placement = "right"),
      shinyBS::popify(numericInput(ns("referenceCol"), "reference", fileCols$reference, min = 1),
        "Reference Column", "There usually is a reference text and a book entry text. Which column contains the reference text?", placement = "right"),
      shinyBS::popify(numericInput(ns("entryCol"), "entry", fileCols$entry, min = 1),
        "Book Entry Column", "There usually is a reference text and a book entry text. Which column contains the book entry text?", placement = "right"),
      shinyBS::popify(numericInput(ns("valueCol"), "value", fileCols$value, min = 1),
        "Value Column", "Which columns contains the transacted ammount?", placement = "right"),
      shinyBS::popify(numericInput(ns("currencyCol"), "currency", fileCols$currency, min = 1),
        "Currency Column", "Which column contains the currency symbol?", placement = "right")
    )
    cols <- tagList(lapply(cols, function(x) div(class = ns("style_cols"), x)))
  
    # seperators
    selected <- if( !is.null(saved) ) c(saved$colSep, saved$decSep) else c("\t", ".")
    seps <- list(
      shinyBS::popify(selectInput(ns("colSep"), "col sep", list(tab = "\t", ";", ",", ":"), selected = selected[1]),
        "Column Separator", paste("Every table uses a symbol to separate columns. Which one is used here?",
          "If you don´t know, just try them out."), placement = "right"),
      shinyBS::popify(selectInput(ns("decSep"), "dec sep", list(".", ","), selected = selected[2]),
        "Decimal Separator", paste("In Germany decimals are separated by a comma, in other countries by a period.",
          "Which one is needed here. You can just try out which one works.",
          "If decimal numbers are preserved they were recognized correctly."), placement = "right")
    )
    seps <- tagList(lapply(seps, function(x) div(class = ns("style_seps"), x)))
    
    # skip lines
    selected <- if( !is.null(saved) ) c(saved$skip, saved$nMax) else c(0, -1)
    skps <- list(
      shinyBS::popify(numericInput(ns("skip"), "skip lines", min = 0, value = selected[1]),
        "Number of Lines to Skip", "Do the first lines contain some text which does not belong to the actual table?", 
        placement = "right"),
      shinyBS::popify(numericInput(ns("max"), "max lines", min = -1, value = selected[2]),
        "Maximum Number of Lines to Read", 
        paste("Do the last lines contain some text which does not belong to the actual table?",
          "<b>-1</b> to read all lines."), placement = "right")
    )
    skps <- tagList(lapply(skps, function(x) div(class = ns("style_skps"), x)))
    
    # head row
    selected <- if( !is.null(saved) ) saved$head else TRUE
    hd <- shinyBS::popify(checkboxInput(ns("hdBl"), "1st line as column names", value = selected),
      "1st Line are Columns Names",
      "Often the 1st line contains the column names. It does not contain actual values.", placement = "right" )                       
    hd <- div(class = ns("style_hd"), hd)
    
    # date formats
    dateForms <- list(examples = c("%d.%m.%Y", "%Y-%m-%d", "%Y/%m/%d"))
    if( !is.null(saved) ) dateForms[["saved"]] <- saved$date
    dt <- shinyBS::popify(selectizeInput(ns("dateFormat"), "date format", dateForms, 
      selected = dateForms[[length(dateForms)]], options = list(maxItems = 1, create = TRUE), multiple = TRUE),
      "Date Formatting", paste(
        "In the date column, how is the date written? Provide the correct formatting.<br/>",
        "It works like this:<br/><b>%Y</b> is the year in 4 digits, <b>%y</b> in 2 digits,",
        "<b>%m</b> is the month in 2 digits, <b>%d</b> is the day in 2 digits.",
        "Write the date by replacing year, month, day.<br/>Examples:<br/>",
        "<em>%d.%m.%Y</em> for 13.03.1990<br/>",
        "<em>%Y-%m-%d</em> for 1990-03-13<br/>",
        "<em>%Y/%m/%d</em> for 1990/03/13<br/>"
      ), placement = "right")
    dt <- div(class = ns("style_dt"), dt)
    
    # arrange in 2 rows
    tagList(
      div(class = ns("style_row"), ref, dt, div(class = ns("style_sepsBlock"), seps), hd), 
      div(class = ns("style_row"), cols, div(class = ns("style_colsBlock"), skps))
    )
  })
  
  # toggle save button
  observe(
    if( !is.null(input$uploadSettings) && input$uploadSettings != "" ){
      shinyjs::enable("btnSave")
    } else {
      shinyjs::disable("btnSave")
    }
  )
  
  # save current settings
  observeEvent(input$btnSave, {
      curSet <- rev(settings)
      cols <- list(name = input$nameCol, iban = input$ibanCol, bic = input$bicCol, date = input$dateCol, 
        reference = input$referenceCol, entry = input$entryCol, value = input$valueCol, currency = input$currencyCol)
      newSet <- list(
        col = cols, type = input$accountType, date = input$dateFormat, colSep = input$colSep, decSep = input$decSep,
        head = input$hdBl, skip = input$skip, nMax = input$max
      )
      curSet[[input$uploadSettings]] <- newSet
      allSettings[["upload"]] <- curSet
      UpdateBLOB("Settings", allSettings, db)
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
  
  return(tas)
}
