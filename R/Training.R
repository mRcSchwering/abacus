#' Training
#'
#' This function trains a shrinkage discriminant analysis (sda) classifier using James-Stein-type shrinkage estimation.
#' It returns the trained model and a data.frame describing the features used for the model.
#' 
#' The \emph{sda} package is used for training and ranking of a sda classifier.
#' Shrinkage intensity for correlation matrix, variances, and frequencies is estimated from the data.
#' With \code{diag} set to \code{TRUE} only the diagonal of the covariance matrix is used.
#' This speeds up the process and uses less memory.
#' 
#' A maximum number of features for the model can be set with \code{n_max}.
#' This can also be used to set a limit to speed and memory for the process.
#' Features are ranked by correlation adjusted t scores and only the top \code{n_max} will be used for the model.
#'
#' @family machine learning
#'
#' @param abt              \code{num matrix} containing training data with rows as observations and columns as features
#' @param labs             \code{vector} defining class labels of rows in training data
#' @param feats            \code{data.frame} with columns \emph{name} and \emph{value} which identifies the features (columns)
#'                         of \code{abt} with \code{chr} values
#' @param n_max            \code{int} (=\code{200}) maximum number of features to use for the model. 
#'                         Features will be chosen by correlation adjusted t scores.
#' @param diag             \code{bool} (=\code{FALSE}) if true DDA instead of LDA is done
#' @param verb             \code{bool} (=\code{FALSE}) verbose, if true sda training messages will be printed
#' 
#' @return \code{list} of 2 objects
#' \itemize{
#'    \item \emph{Model} \code{sda} object containing the trained model
#'    \item \emph{FeatureList} \code{data.frame} description of features as used in the model
#' }
#' 
#' @examples 
#' abt1 <- matrix(sample(0:1, 1000*100, replace = TRUE), 1000, 100)
#' feats1 <- data.frame(name = "test", value = 1:100)
#' labs1 <- sample(0:1, 1000, replace = TRUE)
#' model <- Training(abt1, labs1, feats1, n_max = 20)
#' str(model)
#' abt2 <- matrix(sample(0:1, 1000*100, replace = TRUE), 1000, 100)
#' feats2 <- data.frame(name = "test", value = 100:1)
#' labs2 <- sample(0:1, 1000, replace = TRUE)
#' pred <- Prediction(model$Model, abt2, feats2, model$FeatureList)
#' str(pred)
#' sum(pred$class == labs2)
#'
#' @export
#'
Training <- function( abt, labs, feats, n_max = 200, diag = FALSE, verb = FALSE )
{
  stopifnot(length(labs) == nrow(abt), ncol(abt) == nrow(feats))
  
  # select features
  if( nrow(feats) > n_max ){
    oldFeats <- feats
    ranks <- sda::sda.ranking(abt, labs, diagonal = diag, verbose = verb)
    feats <- oldFeats[ranks[1:n_max, 1],]
    abt <- Convert(abt, oldFeats, feats)
  }
  
  # fit model
  model <- sda::sda(abt, labs, diagonal = diag, verbose = verb)
  
  return(list(Model = model, FeatureList = feats))
}


