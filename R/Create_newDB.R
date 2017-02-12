#' Create_newDB
#'
#' This function creates a new empty database.
#'
#' @family basic functions
#'
#' @param dbName               \code{chr} (\code{"my.db"}) name of database / file (and path)
#' 
#' @return \code{TRUE} if successful
#'
#' @export
#'
Create_newDB <- function( dbName = "my.db" )
{
  # read database schema
  schema <- readLines(system.file("extdata", "database_schema", package = "abacus"))
  schema <- schema[schema != ""]
  
  # create commands
  idx <- which(grepl(";", schema))
  idx <- rbind(idx, c(1, (idx + 1)[-length(idx)]))
  cmds <- apply(idx, 2, function(x) paste(schema[x[2]:x[1]], collapse = "\n"))
  
  # create database
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbName)
  for(i in cmds) DBI::dbGetQuery(con, i)
  RSQLite::dbDisconnect(con)
  
  # enter basic settings
  settings <- list(
    upload = list(
      default = list(
        col = list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10),
        type = "giro", date = "%d.%m.%Y", colSep = "\t", decSep = ",", head = TRUE, skip = 0, nMax = -1
      )
    )
  )
  InsertBLOB("Settings", settings, dbName)
  
  return(TRUE)
} 
