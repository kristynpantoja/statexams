% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kristynFunctions.R
\name{makeQA_Normal}
\alias{makeQA_Normal}
\title{Make a Normal Probability Question}
\usage{
makeQA_Normal(variable = "X", mean = NULL, sd = NULL,
  interval = NULL, tail = NULL)
}
\arguments{
\item{variable}{name of variable (default name is "X")}

\item{mean}{mean of Normal distribution (default is 0)}

\item{sd}{standard deviation of Normal distribution (default is 1)}

\item{interval}{interval or tail probability. Interval if a vector with 2 elements; tail if scalar.}

\item{tail}{If `interval` is scalar value, must specify which tail probability: "left" or "right"}
}
\value{
a vector with first element as question, second element as correct answer, and other elements as other answer choices.
}
\description{
Make a Normal Probability Question
}
\examples{
makeQA_Normal()
}
