#' UploadSettingsUI
#'
#' UploadSettings module provides UI input elements and server logic for defining, saving and retrieving
#' upload settings, and for uploading a csv table with transactions.
#' 
#' The UI consists of a row for settings selection and table upload, 
#' some paragrpah with information, and a row for adjusting settings.
#' All input elements have help texts (bsPopover).
#' Most elements are rendered in the server part though.
#' 
#' In the first row saved settings can be chosen by name in a selectize input
#' or a new setting name can be entered. 
#' A button next to it saves these settings (a modal will indicate success).
#' There is a upload button or uploading csv or txt tables.
#' 
#' In the second row, a lot of settings for reading and interpreting the table are located.
#' These are numeric, select, selectize, and checkbox inputs.
#' They cover:
#' \itemize{
#'    \item column numbers for all necessary information (like \emph{name}, \emph{IBAN}, \emph{date}...)
#'    \item reference account of the uploaded transactions
#'    \item date format used in the \emph{date} column of the table
#'    \item decimal and column separators
#'    \item whether the first line of the file contains column headings
#'    \item how many lines should be skipped and the maximum number of lines to read in the uploaded file
#' }
#'
#' Some basic css styles are defined within this module.
#' Classes are always prefixed with the \code{id}.
#' Best look into the source code of this function to see all the classes defined in the beginning.
#'
#' @family application functions
#'
#' @param id              \code{chr} identifier used in shiny session
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
    p("Here, you can enter new transactions by uploading a csv/txt table.",
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
      "Please only use letters and numbers for setting names.",
      "Changes will take effect after the app was restarted."
    ), placement = "right"),
    shinyBS::bsPopover(ns("uploadSettings"), "Saved Settings", paste(
      "Here, you can chose upload settings that were already saved by their name.",
      "You can save the current settings by clicking the button on the right.",
      "This will overwrite the current setting name.",
      "You can also enter a new setting name by writing something into the box and clicking <b>Add</b>.",
      "Please only use letters and numbers for setting names [a-zA-Z0-9].",
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
#' UploadSettings module provides UI input elements and server logic for defining, saving and retrieving
#' upload settings, and for uploading a csv table with transactions.
#' 
#' This is the server logic of the module.
#' It handles loading and saving upload settings, reading a uploaded table, and doing a quality control.
#' Since the upload settings change with the selection of saved upload settings,
#' the second row of the UI input elements is completely rendered in the server.
#' Their help texts (bsPopover) are also rendere in the server.
#' 
#' For loading and saving upload settings there is a selectize.
#' Upload settings are saved with a name in the database with entry \emph{Settings} in table 
#' \emph{storage} in the \code{list} element \emph{upload}.
#' The selectize presents saved upload settings and by clicking on a name, these settings are loaded.
#' If the \emph{save} button on the right to it is clicked, the current settings will overwrite
#' the previously saved ones. (This is not in a reactive, so changes will take effect after app restart)
#' If no upload settings were saved, there is a fallback mechanism.
#' New names can be entered and saved. 
#'
#' @family application functions
#'
#' @param input           \code{list} from shiny session
#' @param output          \code{list} from shiny session
#' @param session         \code{list} from shiny session
#' @param db              \code{chr} of database file (full path and file name)
#' 
#' @return \code{list} of 3 elements
#' \itemize{
#'    \item \emph{tas} \code{NULL} or a \code{Transactions} object as created with \code{\link{Read_csv}}
#'    \item \emph{err} \code{bool} whether there was an error during upload
#'    \item \emph{msg} \code{chr} a possible error message
#' }
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
  # keep allSettings (this list might include other settings)
  allSettings <- SelectBLOB("Settings", db)
  settings <- allSettings$upload
  settings <- rev(settings)
  refAccounts <- Select("personalAccounts", db)$type
  
  # fallback in case nothing is saved
  if( is.null(settings) ){
    settings <- list(
      default = list(
        col = list(name = 1, iban = 2, bic = 3, date = 4, reference = 5, entry = 6, value = 7, currency = 8),
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
    type <- if( is.null(saved) ) "" else saved$type
    selected <- if( type %in% refAccounts ) type else refAccounts[1]
    ref <- shinyBS::popify(selectInput(ns("accountType"), "reference account", refAccounts, selected = selected),
      "Reference Account Type", "To which of your accounts do these transactions belong to?", placement = "right")
    ref <- div(class = ns("style_ref"), ref)

    # column mappings
    if( is.null(saved) ){
      fileCols <- list(name = 1, iban = 2, bic = 3, date = 4, reference = 5, entry = 6, value = 7, currency = 8)
    } else {
      fileCols <- saved$col
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
    selected <- if( is.null(saved) ) c("\t", ".") else c(saved$colSep, saved$decSep)
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
    selected <- if( is.null(saved) ) c(0, -1) else c(saved$skip, saved$nMax)
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
    selected <- if( is.null(saved) ) TRUE else saved$head
    hd <- shinyBS::popify(checkboxInput(ns("hdBl"), "1st line as column names", value = selected),
      "1st Line are Columns Names",
      "Often the 1st line contains the column names. It does not contain actual values.", placement = "right" )                       
    hd <- div(class = ns("style_hd"), hd)
    
    # date formats
    dateForms <- list(examples = c("%d.%m.%Y", "%Y-%m-%d", "%Y/%m/%d"))
    if( !is.null(saved) ) dateForms[[2]] <- saved$date
    dt <- shinyBS::popify(selectizeInput(ns("dateFormat"), "date format", dateForms, 
      selected = dateForms[[length(dateForms)]], options = list(maxItems = 1, create = TRUE), multiple = TRUE),
      "Date Formatting", paste(
        "In the date column, how is the date written? Provide the correct formatting.<br/>",
        "It works like this:<br/><b>%Y</b> is the year in 4 digits, <b>%y</b> in 2 digits,",
        "<b>%m</b> is the month in 2 digits, <b>%d</b> is the day in 2 digits.",
        "Write the date by replacing year, month, day.",
        "Then confirm by clicking on <b>Add</b>.<br/>Examples:<br/>",
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
    if( is.null(input$uploadSettings) || input$uploadSettings == "" || 
        !grepl("^[0-9a-zA-Z]*$", input$uploadSettings) ){
      shinyjs::disable("btnSave")
    } else {
      shinyjs::enable("btnSave")
    }
  )
  
  # save current settings
  # by writing upload element back into allSettings
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
      out <- list(tas = NULL, err = TRUE, msg = res[1])
    } else {
      out <- list(tas = res, err = FALSE, msg = "")
    }
    
    out
  })
  
  return(tas)
}
