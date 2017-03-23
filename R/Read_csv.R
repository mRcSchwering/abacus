#' Read_csv
#'
#' This function is used to read a \emph{csv} file and create a 
#' \emph{Transactions} object from it.
#' This object contains the transactions in a standardized form and information
#' about the reference account.
#' 
#' \enumerate{
#'  \item \emph{reference account} information is retrieved from the database
#'  \item \emph{file} is read as a table as specified in arguments
#'  \item table is \emph{converted} as specified with arguments \code{columns} 
#'  and \code{dateFormat}
#'  \item \emph{Transactions} object is returned
#' }
#'
#' @family procedures
#'
#' @param ref          \code{chr} for the \emph{type} of the reference account 
#'                     of the uploaded file
#' @param fileName     \code{chr} for the file to be read 
#'                     (should be a \emph{csv})
#' @param columns      \code{list} which defines which columns of the read 
#'                     table (from file) contain which information:
#'                     It must specify \emph{name}, \emph{iban}, \emph{bic}, 
#'                     \emph{date}, \emph{reference}, \emph{entry}, 
#'                     \emph{value}, \emph{currency} as list elements in this 
#'                     order, with their according columns in the table as 
#'                     \code{int} values.
#' @param db           \code{chr} the database used / file name and path of 
#'                      database
#' @param head         \code{bool} (=\code{TRUE}) whether the first line of the 
#'                     file defines column names of the table
#' @param colSep       \code{chr} (=\code{"\\t"}) seperator for columns in 
#'                     the table
#' @param decSep       \code{chr} (=\code{","}) decimal seperator for numbers
#' @param quoteChar    \code{chr} (=\code{"\\""}) symbol used for quotes in 
#'                     table
#' @param commentChar  \code{chr} (=\code{""}) symbol introducing comments 
#'                     in the table
#' @param nSkip        \code{int} (=\code{0}) how many lines to skip when 
#'                     reading the file (from top to bottom)
#' @param nMax         \code{int} (=\code{-1}) maximum number of lines the read 
#'                     (negative to read all lines of file)
#' @param dateFormat   \code{chr} (=\code{"\%d.\%m.\%Y"} for"13.03.1990") date 
#'                     format to use for converting the date column
#' 
#' @return \code{Transactions} object which is a list of 3 elements:
#' \itemize{
#'     \item \emph{Transactions} a data.frame of the file that was read
#'     \item \emph{Reference} a data.frame of the reference account
#'     \item \emph{db} character of database used
#' }
#' 
#' @examples 
#' db <- "test.db"
#' Create_testDB(db)
#' 
#' f <- system.file("extdata", "test_transactions.csv", package = "abacus")
#' cols <- list(name = 6, iban = 7, bic = 8, date = 3, 
#'              reference = 5, entry = 4, value = 9, currency = 10)
#' tas <- Read_csv("giro", f, cols, db)
#'
#' @export
#'
Read_csv <- function(ref, fileName, columns, db, 
                     head = TRUE, colSep = "\t", decSep = ",", 
                     quoteChar = "\"", commentChar = "", 
                     nSkip = 0, nMax = -1, dateFormat = "%d.%m.%Y")
{
  stopifnot(
    length(ref) == 1, 
    length(fileName) == 1, 
    length(db) == 1
  )
  stopifnot(
    inherits(ref, "character"), 
    inherits(fileName, "character"), 
    inherits(db, "character")
  )
  stopifnot(
    inherits(columns, "list"), 
    names(columns) == c("name", "iban", "bic", "date", 
                        "reference", "entry", "value", "currency")
  )
  
  # get reference Account information
  bool <- Intersect(data.frame(type = ref), "personalAccounts", db)
  if (!identical(TRUE, bool)) {
    stop("There is no personal account with type ", ref)
  }
  ref <- Select("personalAccounts", db, eq = list(type = ref))
  
  # read and format table
  csv <- read.csv(
    fileName, 
    header = head, 
    sep = colSep, 
    quote = quoteChar, 
    dec = decSep, 
    comment.char = commentChar, 
    skip = nSkip, 
    nrows = nMax, 
    stringsAsFactors = FALSE
  )
  df <- as.data.frame(
    do.call(cbind, lapply(columns, function(x) csv[, x])), 
    stringsAsFactors = FALSE
  )
  f <- function(x) { any(is.null(x)) || any(is.na(x)) || any(x == "")  }
  df <- df[!apply(df, 1, f), ]
  df$value <- as.numeric(df$value)
  df$date <- as.Date(df$date, dateFormat)
  df$iban <- toupper(df$iban)
  df$bic <- toupper(df$bic)
  
  # form Object
  out <- list(Transactions = df, Reference = ref, db = db)
  class(out) <- "Transactions"
  return(out)
}



