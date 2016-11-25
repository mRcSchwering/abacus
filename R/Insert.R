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
#' @return \code{TRUE} if successful
#'
#' @examples
#' db <- "db/test.db"
#' Create_testDB(db)
#' df <- data.frame(owner = "B. Clinton", iban = "IR98000020018267384", bic = "IR875TW78", type = "donations account")
#' Insert(df, "accounts", "db/test.db", add_id = TRUE)
#'
#' @export
#'
Insert <- function( data, table, db, add_id = FALSE, enforce_foreign_keys = TRUE )
{
  stopifnot(inherits(data, "data.frame"))
  
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






#' InsertBLOB
#'
#' Convenience function to write any kind of object into a SQLite table.
#'
#' @family SQLite handler functions
#'
#' @param name                 \code{chr} name of BLOB
#' @param data                 any object that is to be stored as BLOB
#' @param db                   \code{chr} full file name with path of database
#' @param table                \code{chr} table name ("storage")
#'
#' @return \code{TRUE} if successful
#'
#' @examples
#' db <- "db/test.db"
#' Create_testDB(db)
#' x <- list(a = 1:5, b = list(c = c("a", "b")))
#' InsertBLOB("test2", x, db)
#' SelectBLOB("test2", db)
#'
#' @export
#'
InsertBLOB <- function( name, data, db, table = "storage" )
{
  stopifnot(inherits(name, "character"), length(name) == 1)
  
  # prepare data
  data <- list(serialize(data, NULL))
  data <- data.frame(name = name, data = I(data), stringsAsFactors = FALSE)
  
  # write in db
  res <- abacus::Insert(data, table, db)
  return(res)
} 

