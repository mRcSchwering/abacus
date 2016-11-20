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
#' @family machine learning
#'
#' @param ta               \code{data.frame} of \emph{transactions} joined with \emph{accounts} as returned 
#'                         by \code{\link{Select}("transactions", ...)} 
#' @param pa               \code{data.frame} of \emph{personalAccounts} joined with \emph{accounts} as returned 
#'                         by \code{\link{Select}("personalAccounts", ...)} 
#' 
#' @return \code{list} of 2 objects
#' \itemize{
#'    \item \emph{ABT} \code{int matrix} analytics base table containing features as columns, data points as rows
#'    \item \emph{FeatureList} \code{data.frame} description of \emph{ABT} columns with names and values
#' }
#'
#' @examples 
#' Create_testDB("./db")
#' ta <- Select("transactions", "db/test.db")
#' pa <- Select("personalAccounts", "db/test.db")
#' res <- FeatureExtraction(ta, pa)
#'
#' @export
#'
FeatureExtraction <- function( ta, pa )
{
  env <- environment()
  
  # country code
  x <- sort(unique(strtrim(gsub("[^0-9a-z]", "", tolower(ta$payor_iban)), width = 2)))
  payor_country <- lapply(x, function(x) grepl(x, gsub("[^0-9a-z]", "", tolower(ta$payor_iban))))
  names(payor_country) <- x
  
  x <- sort(unique(strtrim(gsub("[^0-9a-z]", "", tolower(ta$payee_iban)), width = 2)))
  payee_country <- lapply(x, function(x) grepl(x, gsub("[^0-9a-z]", "", tolower(ta$payee_iban))))
  names(payee_country) <- x
  
  # bank code
  x <- sort(unique(strtrim(gsub("[^0-9a-z]", "", tolower(ta$payor_bic)), width = 4)))
  payor_bank <- lapply(x, function(x) grepl(x, gsub("[^0-9a-z]", "", tolower(ta$payor_bic))))
  names(payor_bank) <- x
  
  x <- sort(unique(strtrim(gsub("[^0-9a-z]", "", tolower(ta$payee_bic)), width = 4)))
  payee_bank <- lapply(x, function(x) grepl(x, gsub("[^0-9a-z]", "", tolower(ta$payee_bic))))
  names(payee_bank) <- x
  
  # owners
  x <- sort(unique(gsub("[^a-z0-9 ]", "", tolower(ta$payor_owner))))
  payor_owner <- lapply(x, function(x) grepl(tolower(x), gsub("[^a-z 0-9]", "", tolower(ta$payor_owner))))
  names(payor_owner) <- x
  
  x <- sort(unique(gsub("[^a-z0-9 ]", "", tolower(ta$payee_owner))))
  payee_owner <- lapply(x, function(x) grepl(tolower(x), gsub("[^a-z 0-9]", "", tolower(ta$payee_owner))))
  names(payee_owner) <- x
  
  # own accounts
  x <- sort(unique(pa$account_id[pa$account_id %in% ta$payor_id]))
  payor_pa <- lapply(x, function(x) x == ta$payor_id)
  names(payor_pa) <- x
  
  x <- sort(unique(pa$account_id[pa$account_id %in% ta$payee_id]))
  payee_pa <- lapply(x, function(x) x == ta$payee_id)
  names(payee_pa) <- x
  
  # reference
  reference <- gsub("[^a-z ]", "", tolower(ta$reference))
  x <- sort(unique(unlist(strsplit(reference, " "))))
  reference <- lapply(x, function(x) grepl(x, reference))
  names(reference) <- x
  
  # entry
  entry <- gsub("[^a-z ]", "", tolower(ta$entry))
  x <- sort(unique(unlist(strsplit(entry, " "))))
  entry <- lapply(x, function(x) grepl(x, entry))
  names(entry) <- x
  
  # value
  ta$value <- as.integer(as.character(ta$value))
  value <- list(mod5 = ta$value %% 500 == 0, mod50 = ta$value %% 5000 == 0, mod100 = ta$value %% 10000 == 0, ge100 = ta$value >= 10000)
  # return feature list and abt
  feats <- c( "payor_country", "payee_country", "payor_bank", "payee_bank", "payor_owner", "payee_owner", "payor_pa", "payee_pa", 
              "reference", "entry", "value")
  features <- data.frame( name = unlist(lapply( feats, function(x) rep(x, length(get(x, envir = env))) )), 
                          value = unlist(lapply( feats, function(x) names(get(x, envir = env)) )) )
  abt <- do.call(cbind, unlist(lapply(feats, get, envir = env), recursive = FALSE))
  colnames(abt) <- paste(features$name, features$value, sep = ":")
  mode(abt) <- "integer"
  
  return(list(ABT = abt, FeatureList = features))
}
