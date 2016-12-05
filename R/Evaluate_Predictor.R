#' Evaluate_Predictor
#'
#' The algorithm updates the prediction model according to the current database and then does
#' a n-fold cross validation with the same settings.
#' 
#' \enumerate{
#'  \item Hyperparameters for the predictor are read from the \emph{storage} table (\emph{Params}). 
#'  Then, \code{\link{Update_Predictor}} is run with these parameters.
#'  \item Table \emph{transactions} is read according to hyperparameters and INNER JOINed with \emph{accounts}.
#'  \item Table \emph{personalAccounts} is read and INNER JOINed with \emph{accounts}.
#'  \item A \code{\link{FeatureExtraction}} is done with the tables.
#'  \item A \code{\link{CV}} (cross validation) is done to calculate an error estimate.
#'  \item \code{\link{sda::sda.ranking}} is run if there are more features than the maximum number of features
#'  specified in the Hyperparameters. This is a ranking based on correlation-adjusted t scores.
#'  With this ranking features are selected during the \code{\link{Training}} of the predictor.
#'  \item Table \emph{Storage} is updated with the resulting \emph{Err} and \emph{Ranking}.
#' }
#' 
#' If there are to few transactions used for training (per corss validation round), 
#' the predcitor might not work.
#' It depends on how well the different types (labels) are represented, 
#' but as a rule of thumb there should be a minimum of 20 transactions.
#' So, with a 5-fold cross validation that is at least 100 transactions.
#'
#' @family procedures
#'
#' @param db      \code{chr} the database used / file name and path of database
#' @param nFold   \code{int} (=5) for n-fold cross validation
#' 
#' @return \code{TRUE} if sucessful, otherwise a \code{chr} message where the algorithm stopped.
#' 
#' @examples 
#' db <- "test.db"
#' Create_testDB(db)
#' 
#' params <- list(nFeats = 200, DDL = FALSE, time = list(start = as.Date("2010-1-1"), end = as.Date("2011-1-1")))
#' InsertBLOB("Params", params, db)
#' Evaluate_Predictor(db)
#' err <- SelectBLOB("Err", db)
#' ranks <- SelectBLOB("Ranking", db)
#'
#' @export
#'
Evaluate_Predictor <- function( db, nFold = 5 )
{
  Update_Predictor(db)
  params <- SelectBLOB("Params", db)
  
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
  
  pas <- Select("personalAccounts", db)
  if( nrow(pas) < 1 ) return("No personalAccounts in database")
  
  rs <- FeatureExtraction(tas, pas)
  err <- CV(rs$ABT, rs$FeatureList, tas$type, k = nFold, n_max = params$nFeats, diag = params$DDL)
  ranks <- sda::sda.ranking(rs$ABT, tas$type, diagonal = params$DDL, verbose = FALSE)
  if( nrow(ranks) > params$nFeats ) ranks <- ranks[1:params$nFeats, ]
  rs <- list(Err = err, Ranking = ranks)
  
  ids <- c("Err", "Ranking")
  exst <- Intersect(data.frame(name = ids), "storage", db)
  for( i in 1:length(ids) ){
    if( exst[i] ){
      UpdateBLOB(ids[i], rs[[ids[i]]], db, table = "storage")
    } else {
      InsertBLOB(ids[i], rs[[ids[i]]], db, table = "storage")
    }
  }
  
  return(TRUE)
}



