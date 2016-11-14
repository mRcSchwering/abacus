#' Select
#'
#' Convenience function for \code{SELECT} queries. 
#' One of the 6 table names can be given with optional \code{WHERE} statements.
#' In case transactions, capital or personalAccounts is selected, a \code{INNER JOIN} with accounts will be done (2 joins for transactions).
#' 
#' With \code{where} a condition can be specified.
#' It is given as list where an element name defines the column and a character value its condition.
#' If the name is \code{date}, \code{start_day}, or \code{end_day}, it is interpredted as date in a standard unambiguous format.
#' If it has 1 element, it will be compared with \code{=}, if it has 2 with \code{>=} and \code{<=} combined with \code{AND}.
#' Any other names will be interpreted as a character and directly compared using \code{=}. 
#' Several values, within one list element will be combined with \code{OR}.
#' Conditions of several list elements will be combined with \code{AND}.
#'
#' @family SQLite handler functions
#'
#' @param table                \code{chr} of table name, will automatically be \code{INNER JOIN}ed in case relation exists
#' @param db                   \code{chr} full file name with path of database
#' @param where                \code{list} (\code{NULL}) specifying \code{WHERE} arguments (see details) or \code{NULL}
#' @param enforce_foreign_keys \code{bool} (\code{TRUE}) whether to enforce rules on foreign keys
#' @param check_query          \code{bool} (\code{FALSE}) whether to just return the SQL query without actually sending it
#'
#' @return \code{TRUE} if successful
#'
#' @examples
#' df <- data.frame(owner = "B. Clinton", iban = "IR98000020018267384", bic = "IR875TW78", type = "donations account")
#' Insert(df, "accounts", "db/mydb.db", add_id = TRUE)
#'
#' @export
#'

Select("storage", "db/test.db")


Select <- function( table, db, where = NULL, enforce_foreign_keys = TRUE, check_query = FALSE )
{
  stopifnot(table %in% c("accounts", "transactions", "capital", "personalAccounts", "cashflow", "storage"))
  
  # database
  tab <- switch(table, accounts = "acc", transactions = "tra", capital = "cap", personalAccounts = "per", cashflow = "cas", storage = "sto")
  ref <- "accounts"
  rels <- list(tra = c("payor", "payee"), cap = "account", pers = "account")
  cols <- list(
    acc = c("id", "owner", "iban", "bic"),
    tra = c("payor", "payee", "date", "reference", "entry", "value", "currency", "type"),
    cap = c("account", "date", "value", "currency"),
    per = c("account", "type"),
    cas = c("start_day", "end_day", "value", "currency", "category", "comment"),
    sto = c("name", "data")
  )
  
  # columns
  columns <- switch (tab,
    acc = data.frame(column = cols$acc, as = cols$acc, stringsAsFactors = FALSE),
    tra = data.frame(column = c(paste("payor", cols$acc, sep = "."), paste("payee", cols$acc, sep = "."), cols$tra[-c(1,2)]),
                     as = c(paste("payor", cols$acc, sep = "_"), paste("payee", cols$acc, sep = "_"), cols$tra[-c(1,2)]), stringsAsFactors = FALSE),
    cap = data.frame(column = c(paste("account", cols$acc, sep = "."), cols$cap[-1]),
                     as = c(paste("account", cols$acc, sep = "_"), cols$cap[-1]), stringsAsFactors = FALSE),
    per = data.frame(column = c(paste("account", cols$acc, sep = "."), cols$per[-1]),
                     as = c(paste("account", cols$acc, sep = "_"), cols$per[-1]), stringsAsFactors = FALSE),
    cas = data.frame(column = cols$cas, as = cols$cas, stringsAsFactors = FALSE),
    sto = data.frame(column = cols$sto, as = cols$sto, stringsAsFactors = FALSE)
  )  
  columns <- paste(columns$column, "AS", columns$as, collapse = ", ")
  
  # joins
  joins <- switch (tab,
    acc = table,
    tra = paste(c(table, sprintf("%1$s AS %2$s ON %2$s.id = %3$s.%2$s", ref, rels$tra, table)), collapse = " INNER JOIN "),
    cap = paste(c(table, sprintf("%1$s AS %2$s ON %2$s.id = %3$s.%2$s", ref, rels$cap, table)), collapse = " INNER JOIN "),
    per = paste(c(table, sprintf("%1$s AS %2$s ON %2$s.id = %3$s.%2$s", ref, rels$per, table)), collapse = " INNER JOIN "),
    cas = table,
    sto = table
  )
  
  # wheres
  wheres <- ""
  if( !is.null(where) ){
    
    # date, start_day, end_day
    if( any(names(where) %in% c("date", "start_day", "end_day")) ){
      dates <- intersect(names(where), c("date", "start_day", "end_day"))
      for( i in dates ){
        if( length(where[[i]]) < 2 ){
          date <- as.character(as.Date(where[[i]]))
          wheres <- append(wheres, sprintf("%s = '%s'", i, date))
        } else {
          date <- as.character(sort(as.Date(where[[i]])))
          wheres <- append(wheres, sprintf("%1$s >= '%2$s' AND %1$s <= '%3$s'", i, date[1], date[2]))
        }
      }
    }
    
    # any kind of string
    if( any(!names(where) %in% c("date", "start_day", "end_day")) ){
      strings <- setdiff(names(where), c("date", "start_day", "end_day"))
      for( i in strings ){
        wheres <- append(wheres, paste(sprintf("%s = '%s'", i, where[[i]]), collapse = " OR "))
      }
    }

    # combine
    wheres <- sprintf("WHERE %s", paste(wheres[-1], collapse = " AND "))
  }

  # query
  query <- sprintf("SELECT %s FROM %s %s", columns, joins, wheres)
  if( check_query ) return(query)

  # connect, set PRAGMA
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db)
  if( enforce_foreign_keys ) RSQLite::dbGetQuery(con, "PRAGMA foreign_keys = ON;")
  
  # get query
  res <- RSQLite::dbGetQuery(con, query)
  
  # disconnect
  RSQLite::dbDisconnect(con)
  return(res)
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
#' Insert("test", list(a = 1:10, b = mtcars), "db/mydb.db")
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

