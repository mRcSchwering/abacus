#' Read
#'
#' Generic function for reading objects.
#' \code{\link{Read.Transactions}} is used to read a \emph{Transactions} object (created with \code{\link{Read_csv}} for example).
#' 
#' @family procedures
#' 
#' @export
#'
Read <- function (x, ...) {
  UseMethod("Read", x)
}





#' Read
#'
#' This procedure identifies accounts occuring in a \emph{Transactions} object which cannot be found in the associated database.
#' It then adds these accounts to the \emph{Transactions} object and returns it.
#'
#' @family procedures
#'
#' @param x          \code{Transactions} object (created with \code{\link{Read_csv}} for example)
#' 
#' @return \code{Transactions} object, a list of 4 elements:
#' \itemize{
#'     \item \emph{Transactions} a data.frame of the file that was read
#'     \item \emph{Reference} a data.frame of the reference account
#'     \item \emph{db} character of database used
#'     \item \emph{NewAccounts} a data.frame of accounts that appear in \emph{Transactions}, but not in the database
#' }
#' 
#' @examples 
#' db <- "test.db"
#' Create_testDB(db)
#' 
#' f <- system.file("extdata", "test_transactions.csv", package = "abacus")
#' cols <- list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10)
#' tas <- Read_csv("giro", f, cols, db)
#' tas <- Read(tas)
#'
#' @export
#'
Read.Transactions <- function( x )
{
  tas <- x$Transactions
  acc <- tas[!duplicated(tas[, c("iban", "bic")]), c("name", "iban", "bic")]
  acc <- acc[!Intersect(acc, "accounts", x$db), c("name", "iban", "bic")]
  names(acc)[1] <- "owner"
  x[["NewAccounts"]] <- acc
  return(x)
}

