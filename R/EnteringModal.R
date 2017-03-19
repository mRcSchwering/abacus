#' EnteringModalUI
#'
#' A module for a modal with multiple pages for entering transactions into 
#' the database.
#' 
#' This is the UI part of the module.
#' A modal with 4 pages is created.
#' \enumerate{
#'    \item Checking and correcting new accounts found in uploaded transactions
#'    \item Checking and correcting predictions made by machine learning 
#'          algorithm about new transactions
#'    \item Noting whether duplicated transactions should be entered anyway
#'    \item Confirming sucessful finish
#' }
#'
#' @family application functions
#'
#' @param id              \code{chr} identifier used in shiny session 
#' @param open_modal      \code{reactive} for triggering the modal 
#'                        (e.g. \code{actionButton})
#' 
#' @return \code{chr} html code of UI
#' 
#' @examples 
#'
#' 
#' @export
#'
EnteringModalUI <- function(id, open_modal) 
{
  ns <- NS(id)
  
  # styles
  style <- tags$head(tags$style(
    HTML(
      sprintf(".%s { color: red; }", ns("errmsg")),
      sprintf(".%s { margin-bottom: 0px; }", ns("chown")),
      sprintf(".%s { margin-top: -5px; }", ns("chown")),
      sprintf(".%s { width: 250px; }", ns("chown")),
      sprintf(".%s { height: 30px; }", ns("chown"))
    )
  ))
  
  # building the modal
  modal <- shinyBS::bsModal(
    id = ns("modal"), 
    title = "Test Modal", 
    trigger = open_modal, 
    size = "large", 
    shinyjs::hidden(tagList(
      
      # Page 1
      div(class = "page", id = ns("page1"), 
        h3("New Accounts"), 
        p("These accounts are not in the database yet.
          Please check if you want to enter them like this."),
        p("For owner names please only use", strong("letters"), 
          "and", strong("numbers.")),
        DT::dataTableOutput(ns("newAccounts")),
        p("With a click on ", strong("Next"), 
          " the new accounts are entered into the database -- if there were any.
          If you want to abort, just close this window. 
          Nothing has been entered yet.")
      ),
      
      # Page 2
      div(class = "page", id = ns("page2"), 
        h3("Predictions"), 
        p("For each transaction the following labels were predicted. 
          Please check if they are correct.
          If not correct them."),
        p(strong("Predictions")),
        p("With a click on ", strong("Next"), 
          " the provided labels are double checked. 
          The transactions are not entered yet.
          If you want to abort you can do that by closing this window.
          So far, only new accounts from the previous page were entered.")
      ),
      
      # Page 3
      div(class = "page", id = ns("page3"), 
        h3("Duplicated"),
        p("These transactions were already found in the database.
          They are assumed to be duplicates. 
          If not check them and they will be entered anyway."),
        p(strong("Duplicates")),
        p("With a click on ", strong("Next"),
          " these transactions are entered into the database.
          If you want to abort you can do that by closing this window.
          You can also go to the previous window and correct the ", em("types"),
          ". So far, only the new accounts from", 
          "the previous page were entered.")
      ),
      
      # Page 4
      div(class = "page", id = ns("page4"), 
        h3("Finish"), 
        p("Transactions were successfully entered into the database.")  
      )
    )),
    
    # Bottom Row
    uiOutput(ns("errormsg")),
    actionButton(ns("btnPrev"), "< Previous"),
    actionButton(ns("btnNext"), "Next >")
  )
  
  return(tagList(style, modal))
}












#' EnteringModal
#'
#' A module for a modal with multiple pages for entering transactions 
#' into the database.
#' 
#' This is the server logic of the module.
#' While the user goes through 4 pages in the modal following is done.
#' \enumerate{
#'    \item \code{\link{Read}} method extracts new accountss from 
#'          \emph{transactions} object.
#'    \item \code{\link{Predict}} method enters new accounts into the database 
#'          and then predicts each transaction.
#'    \item \code{\link{Duplicated}} reads the database to see if transactions 
#'          were already entered.
#'    \item \code{\link{Enter}} finally inserts new transaction into the 
#'          database.
#' }
#' 
#'
#' @family application functions
#'
#' @param input           \code{list} from shiny session
#' @param output          \code{list} from shiny session
#' @param session         \code{list} from shiny session
#' @param open_modal      \code{reactive} used as trigger to open the modal
#' @param tas             \code{reactive} which stores the initial transactions 
#'                        object created by \code{\link{Read_csv}} function
#' @param db              \code{chr} of database file (full path and file name)
#' 
#' @return \code{TRUE}
#' 
#' @examples 
#'
#' 
#' @export
#'
EnteringModal <- function(input, output, session, open_modal, tas, db) 
{
  ns <- session$ns
  
  # reactive values
  status <- reactiveValues(page = 1, err = FALSE, msg = "", tas = NULL)
  
  # before Page 1 (Page 0)
  # run Read method to identify new accounts
  observeEvent(open_modal(), status$page <- 1)
  observeEvent(list(open_modal(), tas()), {
    if (!is.null(tas())) status$tas <- Read(tas())
  })
  
  # Page 1
  # show new accounts in a table with option to make owner name changes
  # upon click replace old owner names with the ones entered and 
  # try to run Predict method
  # if successful update tas object
  output$newAccounts <- DT::renderDataTable({
    if (is.null(status$tas$NewAccounts) || nrow(status$tas$NewAccounts) < 1) {
      dt <- data.frame("No" = "No", "New" = "New", "Accounts" = "Accounts")
      return(Table(dt, class = "stripe hover", dom = "t")) 
    }
    nas <- status$tas$NewAccounts
    txtIns <- vapply(
      1:nrow(nas), 
      function(idx) as.character(div(class = ns("chown"), 
            textInput(ns(paste0("chown", idx)), NULL, nas$owner[idx]))),
      character(1)
    )
    nas$owner <- txtIns
    Table(nas, esc = FALSE)
  })
  observeEvent(input$btnNext, {
    if( status$page == 1 ){
      status$err <- FALSE
      status$msg <- ""
      tas <- status$tas
      if( nrow(tas$NewAccounts) > 0){
        txtIns <- sapply(
          1:nrow(tas$NewAccounts), 
          function(idx) as.character(input[[paste0("chown", idx)]]),
          character(1)
        )
        tas$NewAccounts$owner <- txtIns 
      }
      res <- try(Predict(tas))
      if (inherits(res, "try-error")) {
        status$err <- TRUE
        status$msg <- attr(res, "condition")$message
      } else {
        status$tas <- res
      }
    }
  })
  
  # Page 2
  # predictions are shown with option to correct them
  
  
  observeEvent(status$tas,print(status$tas))
  
  
  # multiple pages logic
  output$errormsg <- renderUI(div(class = ns("errmsg"), status$msg))
  observeEvent(status$page, {
    shinyjs::toggleState(id = "btnPrev", condition = status$page == 3)
    shinyjs::toggleState(id = "btnNext", condition = status$page < 4)
    shinyjs::hide(selector = ".page")
    shinyjs::show(sprintf("page%s", status$page))
  })
  observeEvent(input$btnPrev, status$page <- status$page + -1)
  observeEvent(input$btnNext, if (!status$err) status$page <- status$page + 1)
  
  return(TRUE)
}
