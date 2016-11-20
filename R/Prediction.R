#' Prediction
#'
#' This function trains a shrinkage discriminant analysis (sda) classifier using James-Stein-type shrinkage estimation.
#' It returns the trained model, a feature ranking and a data.frame describing the features used for the model.
#' 
#' This function uses the \emph{sda} package for training and ranking of a sda classifier.
#' Shrinkage intensity for correlation matrix, variances, and frequencies is estimated from the data.
#' With \code{diag} set to \code{TRUE} only the diagonal of the covariance matrix is used.
#' This speeds up the process and uses less memory.
#'
#' @family machine learning
#'
#' @param model            \code{sda} object containing a sda model
#' @param abt              \code{num matrix} containing test data with rows as observations and columns as features
#' @param feats            \code{data.frame} with columns \emph{name} and \emph{value} which identifies the features (columns)
#'                         of the test data
#' @param ref              \code{data.frame} with columns \emph{name} and \emph{value} which identifies the features (columns)
#'                         expected by the \code{model}
#' @param verb             \code{bool} (=\code{FALSE}) verbose, if true sda predict messages will be printed
#' 
#' @return \code{list} of 3 objects
#' \itemize{
#'    \item \emph{class} \code{factor} containing predicted classes for test data
#'    \item \emph{posterior} \code{num matrix} containing posterior probabilities of each class for test data
#' }
#' 
#' @examples 
#' abt1 <- matrix(sample(0:1, 1000*100, replace = TRUE), 1000, 100)
#' feats1 <- data.frame(name = "test", value = 1:100)
#' labs1 <- sample(0:1, 1000, replace = TRUE)
#' model <- Training(abt1, labs1, feats1, ranking = TRUE)
#' str(model)
#' plot(model$Ranking)
#' abt2 <- matrix(sample(0:1, 1000*100, replace = TRUE), 1000, 100)
#' feats2 <- data.frame(name = "test", value = 100:1)
#' labs2 <- sample(0:1, 1000, replace = TRUE)
#' pred <- Prediction(model$Model, abt2, feats2, model$FeatureList)
#' str(pred)
#' sum(pred$class == labs2)
#'
#' @export
#'
Prediction <- function( model, abt, feats, ref, verb = FALSE )
{
  abt <- Convert(abt, feats, ref)
  pred <- sda::predict.sda(model, abt, verbose = verb)
  
  return(pred)
}


