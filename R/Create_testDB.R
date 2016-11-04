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
  schema <- scan(system.file("extdata", "database_schema", package = "abacus"), what = "", blank.lines.skip = TRUE)
  cmds <- character(8)
  n <- 1
  for( i in schema ){
    cmds[n] <- paste(cmds[n], i)
    if( grepl("\\;", i) ) n <- n + 1
  }
  
  # create database
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbName)
  for( i in cmds ) RSQLite::dbGetQuery(con, i)
  
  # insert test data
  Insert(abacus::accounts, "accounts", dbName, add_id = TRUE)
  Insert(abacus::transactions, "transactions", dbName)
  
  # disconnect
  RSQLite::dbDisconnect(con)
  return(TRUE)
} 
