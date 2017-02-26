#' Predict
#'
#' Generic function for making a prediction with certain objects.
#' \code{\link{Predict.Transactions}} is used to predict transaction types of a 
#' \emph{Transactions} object (created with \code{\link{Read.Transactions}} for example).
#' 
#' @family procedures
#' 
#' @export
#'
Predict <- function (x, ...) {
  UseMethod("Predict", x)
}





#' Predict.Transactions
#'
#' This function uses a machine learning method to classify each type of transaction for a \emph{Transactions} object
#' as created with \code{\link{Read.Transactions}} for example.
#' A table with the actual transactions and the predicted types is returned.
#' 
#' \enumerate{
#'     \item If there are \emph{new accounts} (not existent in database yet), these accounts are written into the database.
#'     Also, if the user did make changes to these accounts, these changes are applied to the actual transactions
#'     accordingly.
#'     \item A complete \emph{transactions} table is created by combining the original transactions table with all
#'     information about the accounts. Numeric values are converted to Integers in this step (* 100).
#'     \item If a \emph{Prediction Model} was already created, it is read from the database
#'     \item \emph{Feature Extraction} and \emph{Prediction} are done using the model
#'     
#' }
#'
#' @family procedures
#'
#' @param x          \code{Transactions} object (created with \code{\link{Read.Transactions}} for example)
#' 
#' @return 
#' \code{Transactions} object, a list of 4 elements:
#' \itemize{
#'     \item \emph{Transactions} a data.frame of the file that was read
#'     \item \emph{Reference} a data.frame of the reference account
#'     \item \emph{db} character of database used
#'     \item \emph{Prediction} a data.frame of transactions with additional columns of account information.
#'           It includes a column with the predicted type of each transaction.
#' }
#' 
#' @examples 
#' db <- "test.db"
#' Create_testDB(db)
#' 
#' f <- system.file("extdata", "test_transactions.csv", package = "abacus")
#' cols <- list(name = 6, iban = 7, bic = 8, date = 3, reference = 5, entry = 4, value = 9, currency = 10)
#' tas <- Read_csv("giro", f, cols, db)
#' tas <- Read(tas)
#' 
#' params <- list(nFeats = 200, DDL = FALSE, time = list(start = as.Date("2010-1-1"), end = as.Date("2011-1-1")))
#' InsertBLOB("Params", params, db)
#' Update_Predictor(db)
#' 
#' pred <- Predict(tas)
#'
#' @export
#'
Predict.Transactions <- function( x )
{
  if(!"NewAccounts" %in% names(x)) stop("Use Read method with Transactions object first")
  if(any(nchar(x$NewAccounts$owner) < 1)) stop("At least 1 owner has no name")
  if(any(grepl("[^0-9A-Za-z ]", x$NewAccounts$owner))) stop("Please only use letters and numbers as owner names")
  if(any(is.na(x$Transactions$value))) stop("NA's in transaction values")
  
  # insert new accounts and overwrite the relevant values in Transactions table
  # in case the user made changes to these new accounts
  if( nrow(x$NewAccounts) > 0 ){
    Insert(x$NewAccounts, "accounts", x$db, add_id = TRUE)
    for( i in 1:nrow(x$NewAccounts) ){
      idx <- cbind(x$NewAccounts$iban[i] == x$Transactions$iban, x$NewAccounts$bic[i] == x$Transactions$bic)
      idx <- which(apply(idx, 1, function(y) sum(y) > 1))
      x$Transactions$name[idx] <- x$NewAccounts$owner[i] 
    }
  }
  x$NewAccounts <- NULL
  
  # build df by combining transactions with accounts
  # sign of value decides payor and payee
  l <- lapply(1:nrow(x$Transactions), function(i) {
    r <- x$Transactions[i, ]
    sgn <- r$value > 0
    acc <- Select("accounts", x$db, eq = list(iban = r$iban, bic = r$bic), all_and = TRUE)
    data.frame(
      payor_id = if(sgn) acc$id else x$Reference$account_id,
      payor_owner = if(sgn) acc$owner else x$Reference$account_owner,
      payor_iban = if(sgn) acc$iban else x$Reference$account_iban,
      payor_bic = if(sgn) acc$bic else x$Reference$account_bic,
      payee_id = if(!sgn) acc$id else x$Reference$account_id,
      payee_owner = if(!sgn) acc$owner else x$Reference$account_owner,
      payee_iban = if(!sgn) acc$iban else x$Reference$account_iban,
      payee_bic = if(!sgn) acc$bic else x$Reference$account_bic,
      date = r$date,
      reference = r$reference,
      entry = r$entry,
      value = as.integer(abs(r$value) * 100),
      currency = toupper(r$currency),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, l)
  
  # if prediction cannot be done, return transactions
  nms <- c("Model", "FeatureList")
  if( any(!Intersect(data.frame(name = nms), "storage", x$db)) ){
    df[["predicted type"]] <- ""
    x[["Prediction"]] <- df
    return(x)
  }
  
  # feature extraction and prediction
  pas <- Select("personalAccounts", x$db)
  model <- SelectBLOB("Model", x$db)
  feats <- SelectBLOB("FeatureList", x$db)
  rs <- FeatureExtraction(df, pas)
  pred <- Prediction(model, rs$ABT, rs$FeatureList, feats)
  
  # propose prediction
  df <- cbind(df, pred$class)
  names(df)[14] <- "predicted type"
  
  x[["Prediction"]] <- df
  return(x)
}

