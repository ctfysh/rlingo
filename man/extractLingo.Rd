\name{extractLingo}
\alias{extractLingo}
\title{Extract Lingo Results}
\usage{
  extractLingo(res)
}
\arguments{
  \item{res}{the result of \code{\link{runLingo}}.}
}
\value{
  A list which includes "solution", "objective",
  "iterations", "values", "price".
}
\description{
  Extract Lingo results into R list.
}
\details{
  This function is used to translate Lingo results into a
  list which can be easily used by R.
}

