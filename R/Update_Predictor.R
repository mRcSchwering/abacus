#' Update_Predictor
#'
#' The algorithm updates the prediction model according to the current database.
#' 
#' \enumerate{
#'  \item Hyperparameters for the predictor are read from the \emph{storage} table (\emph{Params}). 
#'        They contain the maximum number of features to use (\emph{nFeats}), whether to only do a diagonal discriminant
#'        analysis (\emph{DDL}), and a \emph{time} intervall defining \emph{start} and \emph{end} date of transactions
#'        to be included in the training data.
#'  \item Table \emph{transactions} is read according to hyperparameters and INNER JOINed with \emph{accounts}.
#'  \item Table \emph{personalAccounts} is read and INNER JOINed with \emph{accounts}.
#'  \item A \code{\link{FeatureExtraction}} is done with the tables.
#'  \item A \code{\link{Training}} is done with the resulting analytics base table.
#'  \item Table \emph{Storage} is updated with the resulting \emph{Model} and \emph{FeatureList}.
#' }
#' 
#' If there are to few transactions used for training, the predcitor might not work.
#' It depends on how well the different types (labels) are represented, 
#' but as a rule of thumb there should be a minimum of 20 transactions.
#'
#' @family procedures
#'
#' @param db   \code{chr} the database used / file name and path of database
#' 
#' @return \code{TRUE} if sucessful, otherwise a \code{chr} message where the algorithm stopped.
#' 
#' @examples 
#' db <- "test.db"
#' Create_testDB(db)
#' 
#' params <- list(nFeats = 200, DDL = FALSE, time = list(start = as.Date("2010-1-1"), end = as.Date("2011-1-1")))
#' InsertBLOB("Params", params, db)
#' Update_Predictor(db)
#' feats <- SelectBLOB("FeatureList", db)
#' model <- SelectBLOB("Model", db)
#'
#' @export
#'
Update_Predictor <- function( db )
{
  table <- "storage"
  if( identical(Intersect(data.frame(name = "Params"), table, db), FALSE) ) stop("Hyperparameters not set yet.")
  params <- SelectBLOB("Params", db, table = table)
  
  # select transactions according to parameters
  if( !is.null(params$time$year) ){
    d <- as.POSIXlt(Sys.Date())
    leDate <- list(date = as.Date(d))
    d$year <- d$year - params$time$year
    geDate <- list(date = as.Date(d))
  } else {
    geDate <- if( is.null(params$time$start) ) NULL else list(date = params$time$start)
    leDate <- if( is.null(params$time$end) ) NULL else list(date = params$time$end) 
  }
  tas <- Select("transactions", db, ge = geDate, le = leDate)
  if( nrow(tas) < 1 ) return("No transactions in database for the provided time intervall")
  
  # select personal Accounts
  pas <- Select("personalAccounts", db)
  if( nrow(pas) < 1 ) return("No personalAccounts in database")
  
  # Feature extraction and training
  rs <- FeatureExtraction(tas, pas)
  rs <- Training(rs$ABT, tas$type, rs$FeatureList, n_max = params$nFeats, diag = params$DDL)
  
  # Insert/Update Predictor
  ids <- c("Model", "FeatureList")
  exst <- Intersect(data.frame(name = ids), table, db)
  for( i in 1:length(ids) ){
    if( exst[i] ){
      UpdateBLOB(ids[i], rs[[ids[i]]], db, table = "storage")
    } else {
      InsertBLOB(ids[i], rs[[ids[i]]], db, table = "storage")
    }
  }
  
  return(TRUE)
}



