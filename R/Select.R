#' Select
#'
#' Convenience function for \code{SELECT} queries. 
#' One of the 6 table names can be given with optional \code{WHERE} statements.
#' In case transactions, capital or personalAccounts is selected, a \code{INNER JOIN} with accounts will be done (2 joins for transactions).
#' 
#' A \code{WHERE} condition can be added with arguments \code{eq} (equal), \code{ge} (greater-equal), \code{le} (lesser-equal).
#' Conditions are given as list for the desired relation. The name of a list element defines a column and its value the value.
#' Multiple \code{ge} and \code{se} conditions are combined by \code{AND} and \code{eq} conditions by \code{OR}.
#' 
#' You can set \code{check_query} to \code{TRUE} to see the query.
#'
#' @family SQLite handler functions
#'
#' @param table                \code{chr} of table name, will automatically be \code{INNER JOIN}ed in case relation exists
#' @param db                   \code{chr} full file name with path of database
#' @param eq                   \code{list} (\code{NULL}) defining a condition with \code{=} (equal). . 
#'                              Element name specifies column, value its value (see details)
#' @param ge                   \code{list} (\code{NULL}) defining a condition with \code{>=} (greater-equal). 
#'                              Element name specifies column, value its value (see details)
#' @param le                   \code{list} (\code{NULL}) defining a condition with \code{<=} (lesser-equal). . 
#'                              Element name specifies column, value its value (see details)
#' @param enforce_foreign_keys \code{bool} (\code{TRUE}) whether to enforce rules on foreign keys
#' @param check_query          \code{bool} (\code{FALSE}) whether to just return the SQL query without actually sending it
#'
#' @return \code{data.frame} of table
#'
#' @examples
#' Create_testDB("./db")
#' df <- Select("transactions", "db/test.db", le = list(payor_id = 2), eq = list(type = c("food", "purchase")))
#'
#' @export
#'
Select <- function( table, db, eq = NULL, ge = NULL, le = NULL, enforce_foreign_keys = TRUE, check_query = FALSE )
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
  
  # conditions
  where <- NULL
  if( any(!c(is.null(eq), is.null(ge), is.null(le))) ){
    ands <- NULL
    ors <- NULL
    
    # dates
    idx <- which(grepl("date|day", names(eq)))
    for( i in idx ) ors <- append(ors, sprintf("%s = '%s'", names(eq)[i], as.character(as.Date(eq[[i]]))))
    eq[idx] <- NULL
    idx <- which(grepl("date|day", names(ge)))
    for( i in idx ) ands <- append(ands, sprintf("%s >= '%s'", names(ge)[i], as.character(as.Date(ge[[i]]))))
    ge[idx] <- NULL
    idx <- which(grepl("date|day", names(le)))
    for( i in idx ) ands <- append(ands, sprintf("%s <= '%s'", names(le)[i], as.character(as.Date(le[[i]]))))
    le[idx] <- NULL
    
    # ids
    idx <- which(grepl("id", names(eq)))
    for( i in idx ) ors <- append(ors, sprintf("%s = '%s'", names(eq)[i], eq[[i]]))
    eq[idx] <- NULL
    idx <- which(grepl("id", names(ge)))
    for( i in idx ) ands <- append(ands, sprintf("%s >= '%s'", names(ge)[i], ge[[i]]))
    ge[idx] <- NULL
    idx <- which(grepl("id", names(le)))
    for( i in idx ) ands <- append(ands, sprintf("%s <= '%s'", names(le)[i], le[[i]]))
    le[idx] <- NULL

    # the rest (strings, only eq)
    if(length(eq) > 0) for(i in 1:length(eq)) ors <- append(ors, sprintf("%s = '%s'", names(eq)[i], eq[[i]]))

    # combine
    if(!is.null(ands)) ands <- paste(ands, collapse = " AND ")
    if(!is.null(ors)) ors <- paste(ors, collapse = " OR ")
    where <- paste(c(ands, ors), collapse = " AND ")
  }

  # query
  query <- if(is.null(where)) sprintf("SELECT %s FROM %s", columns, joins) else sprintf("SELECT %s FROM %s WHERE %s", columns, joins, where)
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






#' SelectBLOB
#'
#' Convenience function to retrieve any kind of BLOB stored in table \emph{storage} of SQLite database
#'
#' @family SQLite handler functions
#'
#' @param name                 \code{chr} name of BLOB
#' @param db                   \code{chr} full file name with path of database
#'
#' @return \code{R} object selected
#'
#' @examples
#' Create_testDB("./db")
#' x <- list(a = 1:5, b = list(c = c("a", "b")))
#' InsertBLOB("test2", x, db)
#' SelectBLOB("test2", db)
#'
#' @export
#'
SelectBLOB <- function( name, db )
{
  stopifnot(inherits(name, "character"), length(name) == 1)
  
  # select
  df <- Select("storage", db, eq = list(name = name))
  
  # retrieve data
  obj <- unserialize(df$data[[1]])
  return(obj)
} 

