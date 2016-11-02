#' Insert
#'
#' Convenience function to write a \code{data.frame} or \code{matrix} into a database table.
#' The first column id will be added and incremented automatically if \code{add_id} is enabled.
#'
#' @family SQLite handler functions
#'
#' @param data                 \code{data.frame} of data to be written into db table
#' @param table                \code{chr} of table name
#' @param db                   \code{chr} full file name with path of database
#' @param enforce_foreign_keys \code{bool} (\code{TRUE}) whether to enforce rules on foreign keys
#' @param add_id               \code{bool} (\code{FALSE}) whether 1st column named \emph{id} with values \code{NULL} should be added
#'                             (will be autoincremented in table \emph{accounts})
#'
#' @return chr HTML for creating ui elements.
#'
#' @examples
#' df <- data.frame(owner = "B. Clinton", iban = "IR98000020018267384", bic = "IR875TW78", type = "donations account")
#' Insert(df, "accounts", "db/mydb.db", add_id = TRUE)
#'
#' @export
#'
Insert <- function( data, table, db, add_id = FALSE, enforce_foreign_keys = TRUE )
{
  if( class(data) != "data.frame" ) stop("data must be class data.frame")
  
  # connect, set PRAGMA
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db)
  if( enforce_foreign_keys ) RSQLite::dbGetQuery(con, "PRAGMA foreign_keys = ON;")
  
  # write in db
  cols <- paste0("@", names(data), collapse = ", ")
  if( add_id ) cols <- paste("NULL", cols, sep = ", ")
  RSQLite::dbGetPreparedQuery(con, sprintf("INSERT INTO %s VALUES (%s)", table, cols), data)
  
  # disconnect
  RSQLite::dbDisconnect(con)
  return(TRUE)
} 
