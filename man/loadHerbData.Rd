% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HerbLeafAnalyzer.R
\name{loadHerbData}
\alias{loadHerbData}
\title{Load Herb Data}
\usage{
loadHerbData(filename)
}
\arguments{
\item{filename}{A string specifying the path to the .RData file to be loaded.}
}
\value{
A data frame containing herb data.
}
\description{
Loads the specified .RData file containing herb data.
}
\examples{
data <- loadHerbData("herbData.RData")
}
