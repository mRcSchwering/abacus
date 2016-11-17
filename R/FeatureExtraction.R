#' FeatureExtraction
#'
#' This function does a feature extraction to create an analytics base table from the \emph{transactions}
#' and the \emph{personalAccounts} tables.
#' The features are mostly binary (occurence of a word) or discrete (count of a word) and very sparse.
#' 
#' The analytics base table (\emph{ABT}) containes feature values as \code{int}.
#' Columns represent features, rows data points (= transactions).
#' The ABT is accompanied by a \emph{FeatureList} which describes the features.
#' It is necessary for conversion between different ABTs.
#'
#' @family test.db functions
#'
#' @param ta               \code{data.frame} of \emph{transactions} joined with \emph{accounts} as returned 
#'                         by \code{\link{Select("transactions", ...)}} 
#' @param pa               \code{data.frame} of \emph{personalAccounts} joined with \emph{accounts} as returned 
#'                         by \code{\link{Select("personalAccounts", ...)}} 
#' 
#' @return \code{list} of 2 objects
#' \itemize{
#'    \item \code{int matrix} of class \emph{ABT}, an analytics base table containing features as columns, data points as rows
#'    \item \code{data.frame} of class \emph{FeatureList}, a description of \emph{ABT} columns with names and values
#' }
#'
#' @export
#'
FeatureExtraction <- function( ta, pa )
{
  env <- environment()
  
  # country code
  x <- sort(unique(strtrim(ta$payor_iban, width = 2)))
  payor_country <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payor_iban)))
  names(payor_country) <- tolower(x)
  
  x <- sort(unique(strtrim(ta$payee_iban, width = 2)))
  payee_country <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payee_iban)))
  names(payee_country) <- tolower(x)
  
  # bank code
  x <- sort(unique(strtrim(ta$payor_bic, width = 4)))
  payor_bank <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payor_bic)))
  names(payor_bank) <- tolower(x)
  
  x <- sort(unique(strtrim(ta$payee_bic, width = 4)))
  payee_bank <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payee_bic)))
  names(payee_bank) <- tolower(x)
  
  # owners
  x <- sort(unique(tolower(ta$payor_owner)))
  payor_owner <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payor_owner)))
  names(payor_owner) <- x
  
  x <- sort(unique(tolower(ta$payee_owner)))
  payee_owner <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payee_owner)))
  names(payee_owner) <- x
  
  # own accounts
  x <- sort(pa$account_id[pa$account_id %in% ta$payor_id])
  payor_pa <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payor_id)))
  names(payor_pa) <- x
  
  x <- sort(pa$account_id[pa$account_id %in% ta$payee_id])
  payee_pa <- lapply(x, function(x) grepl(tolower(x), tolower(ta$payee_id)))
  names(payee_pa) <- x
  
  # reference
  reference <- sort(gsub("[^a-z ]", "", tolower(ta$reference)))
  x <- unlist(strsplit(unique(reference), " "))
  reference <- lapply(x, function(x) grepl(tolower(x), reference))
  names(reference) <- x
  
  # entry
  entry <- sort(gsub("[^a-z ]", "", tolower(ta$entry)))
  x <- unlist(strsplit(unique(entry), " "))
  entry <- lapply(x, function(x) grepl(tolower(x), entry))
  names(entry) <- x
  
  # value
  value <- list(mod5 = ta$value %% 500 == 0, mod50 = ta$value %% 5000 == 0, mod100 = ta$value %% 10000 == 0, ge100 = ta$value >= 10000)
  
  # return feature list and abt
  feats <- c( "payor_country", "payee_country", "payor_bank", "payee_bank", "payor_owner", "payee_owner", "payor_pa", "payee_pa", 
              "reference", "entry", "value")
  features <- data.frame( name = unlist(lapply( feats, function(x) rep(x, length(get(x, envir = env))) )), 
                          value = unlist(lapply( feats, function(x) names(get(x, envir = env)) )) )
  abt <- do.call(cbind, unlist(lapply(feats, get, envir = env), recursive = FALSE))
  colnames(abt) <- paste(features$name, features$value, sep = ":")
  mode(abt) <- "integer"
  class(abt) <- "ABT"
  class(features) <- "FeatureList"
  return(list(ABT = abt, FeatureList = features))
}
