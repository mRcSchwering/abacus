#' Duplicated
#'
#' Generic function for finding duplicates.
#' \code{\link{Duplicated.Transactions}} is used to check whether transactions of a
#' \emph{Transactions} object (created with \code{\link{Predict.Transactions}})
#' are already present in the database.
#' 
#' @family procedures
#' 
#' @export
#'
Duplicated <- function (x, ...) {
  UseMethod("Duplicated", x)
}





#' Duplicated.Transactions
#'
#' This function uses a \emph{Transactions} object as created with \code{\link{Predict.Transactions}} and
#' compares it with the database transactions.
#' Whether some transactions already exist in the database is returned in form of a dataframe.
#' 
#' \enumerate{
#'     \item Unique \emph{dates} are read from new transactions in object \code{x}.
#'     \item \emph{Select} of transactions with same dates from database.
#'     \item \emph{Comparison} of new with selected transactions 
#'     (columns: payor_id, payee_id, date, reference, entry, value, currency)
#'     \item \emph{Identical} rows are noted and then 
#'     \item \emph{Returned} in form of a \code{data.frame} with \code{nrow} new transactions and two
#'     columns bool and type. Bool says whether same row in transactions (in \code{x}) was found in the database,
#'     type says what the
#' }
#'
#' @family procedures
#'
#' @param x          \code{Transactions} object (created with \code{\link{Predict.Transactions}})
#' 
#' @return 
#' \code{Transactions} object, a list of 4 elements:
#' \itemize{
#'     \item \emph{Transactions} a data.frame of the file that was read
#'     \item \emph{Reference} a data.frame of the reference account
#'     \item \emph{db} character of database used
#'     \item \emph{Prediction} a data.frame of transactions as they should be entered into the database
#'     \item \emph{Duplicated} a data.frame of length \code{nrow} \emph{Transactions} with columns 
#'     \emph{bool} and \emph{type}. \emph{bool} says for every row in \emph{Transactions} whether is exists in
#'     the database already. If so, \emph{type} says what type this already existing row (in the database) has.
#' }
#' 
#' @examples 
#' db <- "test.db"
#' Create_testDB(db)
#' 
#' params <- list(nFeats = 200, DDL = FALSE, time = list(start = as.Date("2010-1-1"), end = as.Date("2011-1-1")))
#' InsertBLOB("Params", params, db)
#' Update_Predictor(db)
#' 
#' f <- system.file("extdata", "test_transactions.csv", package = "abacus")
#' cols <- list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10)
#' tas <- Read_csv("giro", f, cols, db)
#' tas <- Read(tas)
#' tas$NewAccounts$owner[3] <- "New Owner 3"
#' tas <- Predict(tas)
#' 
#' tas <- Duplicated(tas)
#' m <- cbind(tas$Transactions,tas$Duplicated)
#' names(m)[9:10] <- c("exists in db", "with type")
#' DT::datatable(m)
#' 
#' @export
#'
Duplicated.Transactions <- function( x )
{
  if( !"Prediction" %in% names(x) ) stop("Please predict transaction types with function Predict first")
  preds <- x$Prediction$`predicted type`
  if( any(is.null(preds)) || any(preds == "") ) stop("Please enter a type for each transaction first")
  
  # select transactions with same date
  test <- x$Prediction
  time <- as.list(unique(test$date))
  names(time) <- rep("date", length(time))
  ref <- Select("transactions", x$db, eq = time)
  
  # find duplicated transactions according to following columns
  if( nrow(ref) > 0 ){
    cols <- c("payor_id", "payee_id", "date", "reference", "entry", "value", "currency")
    bl <- logical()
    tp <- character()
    
    # test each row of test against all rows of ref
    for( i in 1:nrow(test) ){
      m <- lapply(cols, function(x) test[[x]][i] == ref[[x]])
      m <- apply(do.call(cbind, m), 1, function(x) !any(!x))
      bl <- append(bl, any(m))
      tp <- append(tp, if( any(m) ) ref$type[which(m)[1]] else "")
    } 
    
    # if none duplicated return these arrays
  } else {
    bl <- rep(FALSE, nrow(test))
    tp <- rep("", nrow(test))
  }
  
  # add to object
  x[["Duplicated"]] <- data.frame(bool = bl, type = tp)
  names(x$Prediction)[14] <- "type"
  return(x)
}

