% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sqp_construct.R
\name{sqp_construct}
\alias{sqp_construct}
\alias{sqp_construct_}
\title{Construct an SQP tibble manually}
\usage{
sqp_construct(question_name, metrics)

sqp_construct_(question_name, metrics)
}
\arguments{
\item{question_name}{a character string that will be used as the question name}

\item{metrics}{a list containing new SQP metrics. Currently it only
supports quality, reliability and validity. Can also specify one of the metrics
and the remaining are set to NA by default}
}
\value{
a \code{\link[tibble]{tibble}} of one row with the supplied metrics. It also has
class \code{sqp} for further manipulations within the \code{sqpr} package.
}
\description{
\code{sqp_construct} is designed to to create SQP tibbles
by manually inserting new metrics such as quality or validity.
}
\details{
\code{sqp_construct_} is useful if you're interested in programming
with \code{sqpr} rather than using it interactively. If you want to use
\code{sqp_construct} inside a function, use the equivalent \code{sqp_construct_}
which uses standard evaluation.
}
\examples{

sqp_construct(new_question, list(quality = 0.3))

sqp_construct(new_question, list(quality = 0.3, validity = 0.2))

# Note that specifying a column which is not availabe in SQP data
# will throw an error

# sqp_construct(new_question, list(random_col = 0.3, validity = 0.2))
# Error: One or more of the specified metrics don't match the SQP column names

# Currently only quality, reliability and validity are allowed.

}