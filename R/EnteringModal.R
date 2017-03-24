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
EnteringModalUI <- function(id, open_modal, helps) 
{
  ns <- NS(id)
  
  # styles
  style <- tags$head(tags$style(
    HTML(paste0("
      .", ns("errmsg"), " { color: red; }
      .", ns("chown"), 
          " { margin-bottom: 0px; margin-top: -5px; 
            width: 250px; height: 30px; }
      .", ns("pred"), " { margin-left: 5%; }
      .", ns("pred_dl"), " { display: inline-block; margin-left: 20px }
      .", ns("pred_select"), 
          " { display: inline-block; vertical-align: top; 
            margin-top: 35px; width: 200px; }"
    ))
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
        HTML(helps$newAccounts_head),
        DT::dataTableOutput(ns("newAccounts")),
        HTML(helps$newAccounts_foot)
      ),
      
      # Page 2
      div(class = "page", id = ns("page2"), 
        h3("Predictions"), 
        HTML(helps$predictions_head),
        uiOutput(ns("predictions")),
        HTML(helps$predictions_foot)
      ),
      
      # Page 3
      div(class = "page", id = ns("page3"), 
        h3("Duplicated"),
        HTML(helps$duplicated_head),
        p(strong("Duplicates")),
        HTML(helps$duplicated_foot)
      ),
      
      # Page 4
      div(class = "page", id = ns("page4"), 
        h3("Finish"), 
        HTML(helps$finish)
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
  status <- reactiveValues(page = 1, err = FALSE, msg = "", 
                           tas = NULL, types = character())
  
  # before Page 1 (Page 0)
  # read transactions table and read unique types
  # run Read method to identify new accounts
  # always set page to 1 when opening modal
  observeEvent(open_modal(), status$page <- 1)
  observeEvent(list(open_modal(), tas()), {
    status$types <- .Get_types(db)
    if (!is.null(tas())) { status$tas <- Read(tas()) }
  })
  
  # create a datatable showing new accounts if there are some
  # show as textinput, so user can change them
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
  
  # create datatable showing predictions
  # show as selectize input, so user can change them
  # selectize has unique type values found in db as options
  # but user can also enter new one
  output$predictions <- renderUI({
    if (nrow(status$tas$Prediction) < 1) {
      el <- p("There are no predictions to show", class = "placeholder")
      return(el)
    }
    preds <- status$tas$Prediction
    els <- lapply(
      1:nrow(preds),
      function(idx) { 
        select <- selectizeInput(
          ns(paste0("pred", idx)), 
          NULL, 
          status$types, 
          preds$`predicted type`[idx],
          multiple = TRUE,
          options = list(maxItems = 1, create = TRUE)
        )
        decList <- tags$dl(
          tags$dt(preds$date[idx]),
          tags$dd(preds$value[idx], preds$currency[idx]),
          tags$dt("From"),
          tags$dd(preds$payor_owner[idx]),
          tags$dt("To"),
          tags$dd(preds$payee_owner[idx]),
          tags$dt("Reference"),
          tags$dd(preds$reference[idx]),
          tags$dt("Entry"),
          tags$dd(preds$entry[idx])
        )
        tagList(div(
          class = ns("pred"), 
          div(select, class = ns("pred_select")), 
          div(decList, class = ns("pred_dl"))
        ))
      }
    )
    tagList(els)
  })
  
  # Modal Pages
  # procedure functions can be executed with click on Next button
  # page number decides which methods are executed
  # reactive values status holds the objects
  observeEvent(input$btnNext, {
    status$err <- FALSE
    status$msg <- ""
    tas <- status$tas
  
    # Page 1
    # before click user sees table with new accounts found in transactions
    # he was able to change them by entering new owner names
    # upon click text inputs for owner name changes are read
    # current owner names are replaced with user input
    # Predict method is run in try, if successful update tas object
    # if not page is not incremented and msg is shown
    if (status$page == 1) {
      if (nrow(tas$NewAccounts) > 0) {
        txtIns <- vapply(
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
      
    # Page 2
    # before click user sees table with predictions
    # user can change predictions via text inputs
    # upon click user inputs are read, current predctions are replaced
    # Duplicated method is run in try, if successful update tas object
    # if not page is not incremented, msg is shown
    if (status$page == 2) {
      print("Page 2 executing")
      txtIns <- vapply(
        1:nrow(tas$Prediction), 
        function(idx) {
          type <- input[[paste0("pred", idx)]]
          if (length(type) != 1) {
            return("")
          } else {
            return(as.character(type))
          }
        },
        character(1)
      )
      tas$Prediction$`predicted type` <- txtIns
      res <- try(Duplicated(tas))
      if (inherits(res, "try-error")) {
        status$err <- TRUE
        status$msg <- attr(res, "condition")$message
      } else {
        status$tas <- res
      }
    }
  })
  
  
  
  
  observeEvent(status$tas,print(status$tas))
  
  
  # multiple pages logic
  # show error message if there is one
  # enable/disable "prev" "next" buttons depending on page
  # de-/increment page if not error happened
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
