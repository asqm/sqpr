% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sqp.R
\name{get_sqp}
\alias{get_sqp}
\title{Extract variable estimates from the SQP 3.0 prediction algorithm}
\usage{
get_sqp(study, question_name, country, lang, all_columns = FALSE,
  authorized = TRUE)
}
\arguments{
\item{study}{string with the name of the study. Upper and lower cases
are ignored and regular expressions are supported.}

\item{question_name}{A vector of strings specifying the variable names. A
search is performed for similar names from all variables in
the study. Upper or lower case is ignored and regular
expressions are supported.}

\item{country}{the country where the question was applied in two letter ISO
code. See the ISO two letter country name list
\href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{here}
for all options. Upper or lower case is ignored.}

\item{lang}{the language the question should be in, in a three
letter character. See the ISO three letter country name list
\href{https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes}{here}
for all options. Upper or lower case is ignored. This should be
a language spoken in \code{country}.}

\item{all_columns}{a logical stating whether to extract all available
columns from the SQP 3.0 database. See the details section for a list of all
possible variables.}

\item{authorized}{\code{TRUE} to return \strong{only} the authorized
prediction or \code{FALSE} to return all available predictions. If set to
\code{FALSE} a warning is issued reminding the user to pick one prediction
for each variable based on the \code{user_id} and \code{user_username}
columns.}
}
\value{
\code{\link{get_sqp}} returns a \code{\link[tibble]{tibble}} with the
predictions. The number of columns depends on the \code{all_columns}
argument.
}
\description{
Extract variable estimates from the SQP 3.0 prediction algorithm
}
\details{
\code{get_sqp} is a simple wrapper around \code{\link{find_questions}},
\code{\link{find_studies}} and \code{\link{get_estimates}} for a direct
downloading strategy of the SQP data. For a lower-level approach use
a combination of these functions to extract SQP data.

SQP predictions can be both 'authorized' predictions, which are
performed by the SQP 3.0 software, and 'crowd-sourced' predictions which are
added to the database by other users. By default, \code{sqp_data}
always returns the 'authorized' prediction when it is available. When
it is not, it returns the first non-authorized prediction, and so on.
If the user wants to choose a specific prediction, then
\code{authorized = FALSE} will return all available predictions for each
question.

If \code{authorized = FALSE} and \code{all_columns = FALSE},
\code{get_sqp} raises an error because there is no way of
disentangling which prediction is authorized/unauthorized without the
additional \code{user_id} column (observed when \code{all_columns = TRUE}).

\code{sqp_data} returns a four column \code{\link[tibble]{tibble}} with
the question name and the estimates for \code{quality}, \code{reliability}
and \code{validity}. However, if \code{all_columns} is set to \code{TRUE}
the returned \code{\link[tibble]{tibble}} contains new columns. Below you can
find the descriptionof of all columns:

\itemize{
\item question: the literal name of the question in the questionnaire of the study
\item question_id: the API internal ID of the question
\item id: this is the coding ID, that is, the coding of the authorized prediction
\item created: Date of the API request
\item routing_id: Version of the coding scheme applied to get that prediction.
\item authorized: Whether it is an 'authorized' prediction or not. See the details section
\item complete: Whether all fields of the coding are complete
\item user_id: The id of the user that crowd-sourced the prediction
\item error: Whether there was an error in making the prediction. For an example,
 see \url{http://sqp.upf.edu/loadui/#questionPrediction/12552/42383}
\item errorMessage: The error message, if there was an error
\item reliability: The strenght between the true score factor and the observed
 variable or 1 - proportion random error in the observed variance. Computed as
 the squared of the reliability coefficient
\item validity: The strength between the latent concept factor and the
 true score factor or 1 - proportion method error variance in the true
 score variance. Computed as the square of the validity coefficient
\item quality: The strength between the latent concept factor and the
 observed variable or 1 - proportion of random and method error variance
 in the latent concept's variance. Computed as the product of reliability
  and validity.
\item reliabilityCoefficient: The effect between the true score factor and
 the observed variable
\item validityCoefficient: The effect between the latent concept factor and
 the true score factor
\item methodEffectCoefficient: The effect between the method factor and the
 true score factor
\item qualityCoefficient: It is computed as the square root of the quality
\item reliabilityCoefficientInterquartileRange: Interquartile range for the reliability coefficient
\item validityCoefficientInterquartileRange: Interquartile range for the validity coefficient
\item qualityCoefficientInterquartileRange: Interquartile range for the quality coefficient
\item reliabilityCoefficientStdError: Predicted standard error of the reliability coefficient
\item validityCoefficientStdError: Predicted standard error of the validity coefficient
\item qualityCoefficientStdError: Predicted standard error of the quality coefficient
}
}
\examples{

\dontrun{

# Log in with sqp_login first. See ?sqp_login
sqp_login()

# 'es' and 'spa' here stands for Spain and Spanish
get_sqp("ESS Round 4", "tvtot", "es", "spa")


get_sqp(
  "ESS Round 4",
  c("tvtot", "ppltrst", "pplfair"),
  "se",
  "swe"
)

# Sweden-Swedish
get_sqp(
  "ESS Round 4",
  c("tvtot", "ppltrst", "pplfair"),
  "se",
  "swe"
)

# Germany-German
get_sqp(
  "ESS Round 1",
  c("vote", "trstplt"),
  "de",
  "deu",
  all_columns = TRUE
)

}

}
\seealso{
\code{\link{sqp_login}} for logging in to the SQP 3.0 API through R
and \code{\link{find_questions}}, \code{\link{find_studies}} and
\code{\link{get_estimates}} for the lower-level approach of extracting
estimates.
}
