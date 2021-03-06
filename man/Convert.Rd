% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Convert.R
\name{Convert}
\alias{Convert}
\title{Convert}
\usage{
Convert(abt, feats, ref)
}
\arguments{
\item{abt}{\code{num matrix} which should be converted}

\item{feats}{\code{data.frame} with columns \emph{name} and 
\emph{value} which identifies the columns
of \code{abt} with \code{chr} values}

\item{ref}{\code{data.frame} with columns \emph{name} and 
\emph{value} which identifies the columns
of the desired matrix with \code{chr} values 
(columns missing in \code{abt} will be 0)}
}
\value{
\code{num matrix} with columns as identified in \code{ref}
}
\description{
This functions converts a \code{matrix} according to a supplied 
\code{data.frame}.
This can be used to convert an analytics base table into a structure which 
matches to a saved model.
}
\details{
Matrix columns are identified with a \code{data.frame} with columns 
\emph{name} and \emph{value}.
\emph{name}:\emph{value} must be unique in the \code{data.frame}.
Columns of matrix \code{abt} are identified by \code{feats}. 
\code{abt} is converted into a matrix of the form described with \code{ref}.
Columns identified in \code{ref} not appearing in \code{feats} will be 
filled with 0's.
}
\examples{
abt1 <- matrix(1:20, 5,4)
feats1 <- data.frame(
  name = letters[1:4], 
  value = letters[1:4]
)
feats2 <- data.frame(
  name = rep(letters[1:2], 2), 
  value = rep(letters[1:2], each = 2)
)
Convert(abt1, feats1, feats2)

}
\seealso{
Other machine learning: \code{\link{CV}},
  \code{\link{FeatureExtraction}},
  \code{\link{Prediction}}, \code{\link{Training}}
}
