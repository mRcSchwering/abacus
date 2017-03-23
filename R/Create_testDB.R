#' Create_testDB
#'
#' This function creates a test database with 100 accounts and 1000 
#' transactions for trying out stuff.
#'
#' @family test.db functions
#'
#' @param dbName               \code{chr} (\code{"test.db"}) name of database /
#'                             file (in current working dir)
#' 
#' @return \code{TRUE} if successful
#'
#' @export
#'
Create_testDB <- function(dbName = "test.db")
{
  # get paths right
  dbName <- file.path(getwd(), dbName)
  
  # read database schema
  schema <- readLines(system.file("extdata", "database_schema", 
                                  package = "abacus"))
  schema <- schema[schema != ""]
  
  # create commands
  idx <- which(grepl(";", schema))
  idx <- rbind(idx, c(1, (idx + 1)[-length(idx)]))
  cmds <- apply(idx, 2, function(x) paste(schema[x[2]:x[1]], collapse = "\n"))
  
  # create database
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbName)
  for (i in cmds) DBI::dbGetQuery(con, i)
  
  # insert test data
  Insert(abacus::accounts, "accounts", dbName, add_id = TRUE)
  Insert(abacus::personalAccounts, "personalAccounts", dbName)
  Insert(abacus::transactions, "transactions", dbName)
  
  # disconnect
  RSQLite::dbDisconnect(con)
  return(TRUE)
} 
