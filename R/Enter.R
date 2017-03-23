#' Enter
#'
#' Generic function for inserting new rows in database.
#' \code{\link{Enter.Transactions}} is used with a
#' \emph{Transactions} object (created with 
#' \code{\link{Duplicated.Transactions}})
#' to insert transactions into the database.
#' 
#' @family procedures
#' 
#' @export
#'
Enter <- function(x, ...) {
  UseMethod("Enter", x)
}





#' Enter.Transactions
#'
#' This function uses a \emph{Transactions} object as created with 
#' \code{\link{Duplicated.Transactions}} and
#' inserts it into the \emph{transactions} table of the database.
#' The \emph{Prediction} list object is used for this.
#' Transactions marked as duplicated are not entered.
#' This can be controlled with the \code{bool} column of the \emph{Duplicated} 
#' list element.
#' 
#' \enumerate{
#'     \item \emph{Duplicated} rows as indicated by the \code{bool} column of 
#'     the eponymous list element are removed
#'     \item \emph{Columns} are adjusted to database table
#'     \item Correct \emph{formatting} of columns is ensured
#'     \item Data is \emph{inserted} into \emph{transactions} table of database
#' }
#'
#' @family procedures
#'
#' @param x          \code{Transactions} object 
#'                   (created with \code{\link{Duplicated.Transactions}})
#' 
#' @return 
#' \code{TRUE}
#' 
#' @examples 
#' db <- "test.db"
#' Create_testDB(db)
#' 
#' params <- list(
#'   nFeats = 200, 
#'   DDL = FALSE, 
#'   time = list(start = as.Date("2010-1-1"), end = as.Date("2011-1-1"))
#' )
#' InsertBLOB("Params", params, db)
#' Update_Predictor(db)
#' 
#' f <- system.file("extdata", "test_transactions.csv", package = "abacus")
#' cols <- list(name = 6, iban = 7, bic = 8, date = 3, 
#'              reference = 5, entry = 4, value = 9, currency = 10)
#' tas <- Read_csv("giro", f, cols, db)
#' tas <- Read(tas)
#' tas$NewAccounts$owner[3] <- "New Owner 3"
#' tas <- Predict(tas)
#' 
#' tas <- Duplicated(tas)
#' m <- cbind(tas$Prediction,tas$Duplicated)
#' names(m)[15:16] <- c("exists in db", "with type")
#' DT::datatable(m)
#' 
#' Enter(tas)
#' 
#' @export
#'
Enter.Transactions <- function(x)
{
  if (!"Duplicated" %in% names(x)) {
    stop("Find duplicated transactions with Duplicated first")
  }
  stopifnot(nrow(x$Duplicated) == nrow(x$Prediction))
  
  # remove marked rows
  df <- x$Prediction[!x$Duplicated$bool, ]
  
  # only keep these cols
  cols <- c("payor_id", "payee_id", "date", "reference", 
            "entry", "value", "currency", "type")
  df <- df[, names(df) %in% cols]
  
  # ensure formats
  forms <- c("int", "int", "chr", "chr", "chr", "int", "chr", "chr")
  for( i in 1:length(cols) ){
    f <- switch(forms[i], int = as.integer, chr = as.character, date = as.Date)
    df[[cols[i]]] <- f(df[[cols[i]]])
  }
  
  return(Insert(df, "transactions", x$db))
}

