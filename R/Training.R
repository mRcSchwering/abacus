#' Training
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
#' @param abt              \code{num matrix} containing training data with rows as observations and columns as features
#' @param labs             \code{vector} defining class labels of rows in training data
#' @param feats            \code{data.frame} with columns \emph{name} and \emph{value} which identifies the features (columns)
#'                         of \code{abt} with \code{chr} values
#' @param verb             \code{bool} (=\code{FALSE}) verbose, if true sda training messages will be printed
#' @param diag             \code{bool} (=\code{FALSE}) if true DDA instead of LDA is done
#' @param ranking          \code{bool} (=\code{FALSE}) if true a feature ranking by CAT scores is done
#' 
#' @return \code{list} of 3 objects
#' \itemize{
#'    \item \emph{Model} \code{sda} object containing the trained model
#'    \item \emph{Ranking} \code{sda.ranking} object showing a feature ranking by CAT scores (if \code{ranking} was true)
#'    \item \emph{FeatureList} \code{data.frame} description of features as used in the model
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
Training <- function( abt, labs, feats, verb = FALSE, diag = FALSE, ranking = FALSE )
{
  stopifnot(length(labs) == nrow(abt))
  
  ranks <- if(ranking) sda::sda.ranking(abt, labs, diagonal = diag, verbose = verb) else NULL
  model <- sda::sda(abt, labs, diagonal = diag, verbose = verb)
  
  return(list(Model = model, Ranking = ranks, FeatureList = feats))
}


