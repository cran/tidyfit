% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.genetic.R
\name{.fit.genetic}
\alias{.fit.genetic}
\title{Genetic algorithm with linear regression fitness evaluator for \code{tidyfit}}
\usage{
\method{.fit}{genetic}(self, data = NULL)
}
\arguments{
\item{self}{a tidyFit R6 class.}

\item{data}{a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).}
}
\value{
A fitted tidyFit class model.
}
\description{
Fits a linear regression with variable selection using a genetic algorithm on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
}
\details{
\strong{Hyperparameters:}

\emph{None. Cross validation not applicable.}

\strong{Important method arguments (passed to \code{\link{m}})}
\itemize{
\item statistic
\item populationSize
\item numGenerations
\item minVariables
\item maxVariables
}

The function provides a wrapper for \code{gaselect::genAlg}. See \code{?genAlg} for more details.

\strong{Implementation}

Control arguments are passed to gaselect::genAlgControl (the function automatically identifies which arguments are for the control object, and which for gaselect::genAlg).

gaselect::evaluatorLM is used as the evaluator with the relevant arguments automatically identified by the function.
}
\examples{
# Load data
data <- tidyfit::Factor_Industry_Returns

# Generally used inside 'regress' function
fit <- regress(data, Return ~ ., m("genetic", statistic = "BIC"),
               .mask = c("Date", "Industry"))
coef(fit)

}
\references{
Kepplinger D (2023). \emph{gaselect: Genetic Algorithm (GA) for Variable Selection from High-Dimensional Data}. R package version 1.0.21, \url{https://CRAN.R-project.org/package=gaselect}.
}
\seealso{
\code{\link{.fit.lm}}, \code{\link{.fit.bayes}} and \code{\link{m}} methods
}
\author{
Johann Pfitzinger
}
