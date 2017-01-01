#' UploadModalUI
#'
#' UploadModal is a module for a modal with multiple pages for entering transactions into the database.
#' 
#' This is the UI part of the module.
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
#' @export
#'
UploadModalUI <- function( id, open_modal ) 
{
  ns <- NS(id)
  
  # building the modal
  modal <- shinyBS::bsModal(id = ns("modal"), title = "Test Modal", 
                              trigger = open_modal, size = "large", 
    shinyjs::hidden(tagList(
      div(class = "page", id = ns("page1"), 
        h3("New Accounts"), 
        p("These accounts are not in the database yet.
          Please check if you want to enter them like this."),
        p(strong("Some Accounts")),
        p("With a click on ", strong("Next"), 
          " the new accounts are entered into the database.
          If you want to abort, just close this window. 
          Nothing has been entered yet.")
      ),
      div(class = "page", id = ns("page2"), 
        h3("Predictions"), 
        p("The learning machine predicted the following types
          for each prediction.
          Please check if they are correct.
          If not correct them."),
        p(strong("Predictions")),
        p("With a click on ", strong("Next"), 
          " these transactions are not entered into the database yet.
          If you want to abort you can do that by closing this window.
          So far, only the new accounts from the previous page were entered.")
      ),
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
          ". So far, only the new accounts from the previous page were entered.")
      ),
      div(class = "page", id = ns("page4"), 
        h3("Finish"), 
        p("Transactions were successfully entered into the database.")  
      )
    )),
    actionButton(ns("btnPrev"), "< Previous"),
    actionButton(ns("btnNext"), "Next >")
  )
  
  return(modal)
}












#' UploadModal
#'
#' UploadModal is a module for a modal with multiple pages for entering transactions into the database.
#' 
#' This is the server logic of the module.
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
UploadModal <- function( input, output, session, db ) 
{
  # status variable
  status <- reactiveValues(page = 1, readError = "", predictError = "", 
                           duplicatedError = "", enterError = "")
  
  # multiple pages logic
  observeEvent(status$page, {
    shinyjs::toggleState(id = "btnPrev", condition = status$page == 3)
    shinyjs::toggleState(id = "btnNext", condition = status$page < 4)
    shinyjs::hide(selector = ".page")
    shinyjs::show(sprintf("page%s", status$page))
  })
  observeEvent(input$btnPrev, status$page <- status$page + -1)
  observeEvent(input$btnNext, status$page <- status$page + 1)
  
  return(TRUE)
}
