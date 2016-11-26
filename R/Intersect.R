#' Intersect
#'
#' Convenience function for comparing a \code{data.frame} with table entries of a database.
#' This is used to see if certain entries already exist in the database.
#' 
#' What columns exactly are compared depends on the \code{table} value.
#' \itemize{
#'    \item \emph{accounts} the table is searched for equal values in both columns \emph{bic} and \emph{iban}.
#'          A \code{bool} vector indicating for each row in \code{data} whether it exists in \code{table}.
#'    \item \emph{storage} or \emph{personalAccounts} the table is searched for equal values in columns \emph{name} and
#'          \emph{type} respectively. A \code{bool} vector indicating for each row in \code{data} whether it exists in \code{table}.
#' }
#' 
#'
#' @family SQLite handler functions
#'
#' @param data                 \code{data.frame} whose entries are compared to a database table
#' @param table                \code{chr} of database table name
#' @param db                   \code{chr} full file name with path of database
#'
#' @return \code{bool} vector or list of \code{bool} vector and a \code{data.frame}
#'
#' @examples
#' db <- "db/test.db"
#' Create_testDB(db)
#' df <- Select("transactions", "db/test.db", le = list(payor_id = 2), eq = list(type = c("food", "purchase")))
#'
#' @export
#'
Intersect <- function( data, table, db )
{
  stopifnot(table %in% c("accounts", "transactions", "cashflow", "storage"))
  stopifnot(inherits(data, "data.frame"))
  
  data[] <- lapply(data, as.character)
  dub <- NULL
  
  # nrow data << nrow table
  if( table == "accounts" ){
    for( i in 1:nrow(data) ){
      res <- Select(table, db, eq = as.list(data[i, c("iban", "bic")]), all_and = TRUE)
      dub <- append(dub, if( nrow(res) < 1 ) FALSE else TRUE)
    }  
    
    # nrows = very small
  } else if( table == "storage" || table == "personalAccounts" ){
    name <- switch(table, storage = "name", personalAccounts = "type")
    res <- Select("storage", db)[[name]]
    dub <- data[[name]] %in% res
    
    # nrows = big and conditions not unique
  } 
  
  return(dub)
}

