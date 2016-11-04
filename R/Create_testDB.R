#' Create_testDB
#'
#' This function creates a test database with 100 accounts and 1000 transactions.
#'
#' @family test functions
#'
#' @param dbPath               \code{chr} (\code{"~/data"}) path to database directory 
#' @param dbName               \code{chr} (\code{"test"}) name of database / file (\code{.db} will be added)
#' 
#' @return \code{TRUE} if successful
#'
#' @export
#'
Create_testDB <- function( dbPath = "~/data", dbName = "test" )
{
  # get paths right
  dbPath <- normalizePath(dbPath)
  dbName <- file.path(dbPath, paste0(dbName, ".db"))
  
  # read database schema
  schema <- readLines(system.file("extdata", "database_schema", package = "abacus"))
  schema <- schema[schema != ""]
  
  # create commands
  idx <- which(grepl(";", schema))
  idx <- rbind(idx, c(1, (idx + 1)[-length(idx)]))
  cmds <- apply(idx, 2, function(x) paste(schema[x[2]:x[1]], collapse = "\n"))
  
  # create database
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbName)
  for(i in cmds) RSQLite::dbGetQuery(con, i)
  
  # insert test data
  Insert(abacus::accounts, "accounts", dbName, add_id = TRUE)
  Insert(abacus::personalAccounts, "personalAccounts", dbName)
  Insert(abacus::transactions, "transactions", dbName)
  
  # disconnect
  RSQLite::dbDisconnect(con)
  return(TRUE)
} 
