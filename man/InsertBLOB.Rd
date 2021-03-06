% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Insert.R
\name{InsertBLOB}
\alias{InsertBLOB}
\title{InsertBLOB}
\usage{
InsertBLOB(name, data, db, table = "storage")
}
\arguments{
\item{name}{\code{chr} name of BLOB}

\item{data}{any object that is to be stored as BLOB}

\item{db}{\code{chr} full file name with path of database}

\item{table}{\code{chr} table name ("storage")}
}
\value{
\code{TRUE} if successful
}
\description{
Convenience function to write any kind of object into a SQLite table.
}
\examples{
db <- "db/test.db"
Create_testDB(db)
x <- list(a = 1:5, b = list(c = c("a", "b")))
InsertBLOB("test2", x, db)
SelectBLOB("test2", db)

}
\seealso{
Other SQLite handler functions: \code{\link{Insert}},
  \code{\link{Intersect}}, \code{\link{SelectBLOB}},
  \code{\link{Select}}, \code{\link{UpdateBLOB}},
  \code{\link{Update}}
}
