#' 100 fake bank accounts
#'
#' This is used to create a test data base. IBAN and BIC do not necessarily make sense.
#'
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'   \item{owner}{random names}
#'   \item{iban}{random IBANs, don't necessarily make sense}
#'   \item{bic}{random BICs, don't necessarily make sense}
#'   \item{type}{a label for type}
#' }
"accounts" 


#' 1000 fake transactions
#'
#' This is used to create a test data base. They reference the table \emph{accounts}.
#'
#' @format A data frame with 1000 rows and 7 variables:
#' \describe{
#'   \item{payor}{id of payor account, references \emph{accounts}}
#'   \item{payee}{id of payee account, references \emph{accounts}}
#'   \item{date}{date of transaction}
#'   \item{reference}{reference text}
#'   \item{entry}{booking entry}
#'   \item{value}{in cents (decimal currency)}
#'   \item{currency}{currency code}
#' }
"transactions" 
