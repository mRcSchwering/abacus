#' CV
#'
#' This function trains a shrinkage discriminant analysis (sda) classifier as in \code{\link{Training}} in a \code{k}-fold 
#' cross validation.
#' A \code{data.frame} with class labels and predictions is returned which can be used for prediction power estimation.
#'
#' @family machine learning
#'
#' @param abt              \code{num matrix} containing training data with rows as observations and columns as features
#' @param labs             \code{vector} defining class labels of rows in training data
#' @param feats            \code{data.frame} with columns \emph{name} and \emph{value} which identifies the features (columns)
#'                         of \code{abt} with \code{chr} values
#' @param ...              arguments passed to \code{link{Training}}
#' 
#' @return \code{data.frame} with class labels and predictions by the classifier
#' 
#' @examples 
#' abt1 <- matrix(sample(0:1, 1000*100, replace = TRUE), 1000, 100)
#' feats1 <- data.frame(name = "test", value = 1:100)
#' labs1 <- sample(0:1, 1000, replace = TRUE)
#' err <- CV(abt1, feats1, labs1)
#' (acc <- sum(err$class == err$prediction) / nrow(err) * 100)
#'
#' @export
#'
CV <- function( abt, feats, labs, k = 5, ... )
{
  stopifnot(ncol(abt) == nrow(feats), length(labs) == nrow(abt))
  if(k < 1) stop("k must be greater 0 you idiot!")
  
  # error estimation
  err <- data.frame(class = character(), prediction = character())  
  
  # for in k
  ks <- sample(1:k, nrow(abt), replace = TRUE)
  for( i in 1:k ){
    
    # data in test and train
    test <- abt[ks == i, ]
    train <- abt[ks != i, ]
    testlabs <- labs[ks == i]
    trainlabs <- labs[ks != i]
    
    # train and predict
    model <- Training(train, trainlabs, feats, ...)
    pred <- Prediction(model$Model, test, feats, model$FeatureList)
    
    # save results
    err <- rbind(err, data.frame(class = testlabs, prediction = pred$class))
  }
  
  return(err)
}


