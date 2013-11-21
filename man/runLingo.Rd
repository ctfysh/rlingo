\name{runLingo}
\alias{runLingo}
\title{Run Lingo Codes}
\usage{
  runLingo(file, lingo = "d:/LINGO10/RunLingo.exe",
    intern = TRUE, ...)
}
\arguments{
  \item{file}{the filename of SQL codes.}

  \item{lingo}{the directory of executive Lingo file.}

  \item{intern}{a logical (not NA) which indicates whether
  to capture the output of the command as an R character
  vector.}

  \item{...}{arguments to be passed to
  \code{\link{system}}.}
}
\value{
  The results of Lingo as character vector.
}
\description{
  Run Lingo Codes and return the results.
}
\details{
  This function is used to execute Lingo codes by
  RunLingo.exe and get the results by system function in R.
}

