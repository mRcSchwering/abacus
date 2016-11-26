#' Update
#'
#' Convenience function to update tables of a SQLite database.
#' For each row of the updates (\code{data}), the function looks for rows in the database table which have the same values
#' as \code{data} for the columns defined in \code{check}. These rows are updated with the information of that \code{data} row.
#'
#' @family SQLite handler functions
#'
#' @param data                 \code{data.frame} of data to be written into db table
#' @param table                \code{chr} of table name
#' @param check                \code{chr} of column name(s) for which updated rows have to have equal content 
#' @param db                   \code{chr} full file name with path of database
#' @param enforce_foreign_keys \code{bool} (\code{TRUE}) whether to enforce rules on foreign keys
#'
#' @return \code{TRUE} if successful
#'
#' @examples
#' db <- "db/test.db"
#' Create_testDB(db)
#' df <- data.frame(id = 3:4, owner = rep("test",2), iban = rep("test",2), bic = rep("test",2), stringsAsFactors = FALSE)
#' Update(df, "accounts", "id", db)
#' Select("accounts", db, eq = list(id = c(3,4)))
#'
#' @export
#'
Update <- function( data, table, check, db, enforce_foreign_keys = TRUE )
{
  stopifnot(inherits(data, "data.frame"))
  stopifnot(check %in% names(data))
  
  # conditions
  dates <- grepl("date|day", names(data))
  if( any(dates) ) data[, dates] <- as.character(as.Date(data[, dates]))
  wheres <- sapply(check, function(x) sprintf("%s = '%s'", x, data[[x]]))
  if( !is.null(dim(wheres)) ) wheres <- apply(wheres, 1, paste, collapse = " AND ")
  
  # update row by row
  for( i in 1:nrow(data) ){
    
    # query
    cols <- paste0(names(data), " = ?", collapse = ", ")
    query <- sprintf("UPDATE %s SET %s WHERE %s", table, cols, wheres[i])

    # write db
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db)
    if( enforce_foreign_keys ) RSQLite::dbGetQuery(con, "PRAGMA foreign_keys = ON;")
    RSQLite::dbGetPreparedQuery(con, query, data[i, ])
    RSQLite::dbDisconnect(con)
  }
  return(TRUE)
} 





#' UpdateBLOB
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
#' d <- list(test = "some test")
#' InsertBLOB("test3", list(a = 1:10, b = list(a = letters)), db)
#' UpdateBLOB("test3", d, db)
#' d2 <- SelectBLOB("test3", db)
#'
#' @export
#'
UpdateBLOB <- function( name, data, db, table = "storage" )
{
  stopifnot(inherits(name, "character"), length(name) == 1)
  
  # prepare data
  data <- list(serialize(data, NULL))
  data <- data.frame(name = name, data = I(data), stringsAsFactors = FALSE)
  
  # write in db
  res <- abacus::Update(data, table, "name", db)
  return(res)
} 

