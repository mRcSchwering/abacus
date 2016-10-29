#' Insert
#'
#' Convenience function to write a \code{data.frame} or \code{matrix} into a database table.
#' The first column id will be added and incremented automatically.
#'
#' @family SQLite handler functions
#'
#' @param data                 \code{data.frame} or \code{matrix} data to be written into db table, without id (id will be auto incremented)
#' @param table                \code{chr} of table name
#' @param db                   \code{chr} of database name/file from current directory
#' @param enforce_foreign_keys \code{bool} (\code{TRUE}) whether to enforce rules on foreign keys
#'
#' @return chr HTML for creating ui elements.
#'
#' @examples
#' df <- data.frame(owner = "B. Clinton", iban = "IR98000020018267384", bic = "IR875TW78", type = "donations account")
#' Insert(df, "accounts", "db/mydb.db")
#'
#' @export
#'
Insert <- function( data, table, db, enforce_foreign_keys = TRUE )
{
  if( !(class(data) %in% c("matrix", "data.frame")) ) stop("data must be class matrix or data.frame")
  
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db)
  if( enforce_foreign_keys ) RSQLite::dbSendQuery(con, "PRAGMA foreign_keys = ON;")
  
  for( i in 1:nrow(data) ){
    query <- paste0(
      "INSERT INTO ", table, 
      " (id, ", paste(colnames(data), collapse = ", "), ") ",
      "VALUES (NULL, ", paste0("'", data[i, ], "'", collapse = ", "), ");" 
    )
    RSQLite::dbClearResult(RSQLite::dbSendQuery(con, query))
  }
  
  RSQLite::dbDisconnect(con)
  return(TRUE)
} 
