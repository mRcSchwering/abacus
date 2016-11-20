#' Convert
#'
#' This functions converts a \code{matrix} according to a supplied \code{data.frame}.
#' This can be used to convert an analytics base table into a structure which matches to a saved model.
#' 
#' Matrix columns are identified with a \code{data.frame} with columns \emph{name} and \emph{value}.
#' \emph{name}:\emph{value} must be unique in the \code{data.frame}.
#' Columns of matrix \code{abt} are identified by \code{feats}. 
#' \code{abt} is converted into a matrix of the form described with \code{ref}.
#' Columns identified in \code{ref} not appearing in \code{feats} will be filled with 0's.
#'
#' @family machine learning
#'
#' @param abt              \code{num matrix} which should be converted
#' @param feats            \code{data.frame} with columns \emph{name} and \emph{value} which identifies the columns
#'                         of \code{abt} with \code{chr} values
#' @param ref              \code{data.frame} with columns \emph{name} and \emph{value} which identifies the columns
#'                         of the desired matrix with \code{chr} values (columns missing in \code{abt} will be 0)
#' 
#' @return \code{num matrix} with columns as identified in \code{ref}
#' 
#' @examples 
#' abt1 <- matrix(1:20, 5,4)
#' feats1 <- data.frame(name = letters[1:4], value = letters[1:4])
#' feats2 <- data.frame(name = rep(letters[1:2], 2), value = rep(letters[1:2], each = 2))
#' Convert(abt1, feats1, feats2)
#'
#' @export
#'
Convert <- function( abt, feats, ref )
{
  stopifnot(nrow(feats) == ncol(abt))
  stopifnot(colnames(ref) == c("name", "value"), colnames(feats) == c("name", "value"))
  
  idx <- numeric()
  ref <- paste(ref$name, ref$value, sep = ":")
  feats <- paste(feats$name, feats$value, sep = ":")
  for( i in ref ) idx <- append(idx, if(i %in% feats) which(feats == i) else 0)
  idx
  
  blank <- rep(0, nrow(abt))
  
  out <- do.call(cbind, lapply(idx, function(x) if(x == 0) blank else abt[, x]))
  colnames(out) <- ref
  return(out)
}


