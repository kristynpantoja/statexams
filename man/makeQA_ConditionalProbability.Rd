% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamesFunctions.R
\name{makeQA_ConditionalProbability}
\alias{makeQA_ConditionalProbability}
\title{Generate conditional probability question}
\usage{
makeQA_ConditionalProbability(proportion = NULL, condprob = NULL,
  type = NULL)
}
\arguments{
\item{proportion}{Splits the population into 3 groups with this proportion.  Must sum to 1}

\item{condprob}{Gives a conditional probability for each group}

\item{type}{(1, 2).  1:  Total probability question  2: Bayes Theorem question}
}
\value{
a vector with first element as question, second element as correct answer, and other elements as other answer choices.
}
\description{
Generate conditional probability question
}
\examples{
makeQA_ConditionalProbability(type = 2)
 "A company buys resistors from vendor companies A, B, and C.
 30\% come from company A  40\% come from comapny B, and 30\% come
 from company C.  If a resistor comes from company A, there is a
 67\% chance that it is within tolerance.  If a resistor comes from
 company B, there is a 57\% chance that it is within tolerance.
 If a resistor comes from company C, there is a 82\% chance that
 it is within tolerance.  If a randomly selected resistor is
 within tolerance, what is the probability it came from company A?"
 "0.298"
 "0.67"
 "0.675"
 "0.687"
 "2.06"

}
