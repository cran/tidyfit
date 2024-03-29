% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.nnet.R
\name{.fit.nnet}
\alias{.fit.nnet}
\title{Neural Network regression for \code{tidyfit}}
\usage{
\method{.fit}{nnet}(self, data = NULL)
}
\arguments{
\item{self}{a 'tidyFit' R6 class.}

\item{data}{a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).}
}
\value{
A fitted 'tidyFit' class model.
}
\description{
Fits a single-hidden-layer neural network regression on a 'tidyFit' \code{R6} class.
The function can be used with \code{\link{regress}} and \code{\link{classify}}.
}
\details{
\strong{Hyperparameters:}
\itemize{
\item \code{size} \emph{(number of units in the hidden layer)}
\item \code{decay} \emph{(parameter for weight decay)}
\item \code{maxit} \emph{(maximum number of iterations)}
}

\strong{Important method arguments (passed to \code{\link{m}})}

The function provides a wrapper for \code{nnet::nnet.formula}. See \code{?nnet} for more details.

\strong{Implementation}

For \code{\link{regress}}, linear output units (\code{linout=True}) are used, while \code{\link{classify}} implements
the default logic of  \code{nnet} (\code{entropy=TRUE} for 2 target classes and \code{softmax=TRUE} for more classes).
}
\examples{
# Load data
data <- tidyfit::Factor_Industry_Returns

# Stand-alone function
fit <- m("nnet", Return ~ ., data)
fit

# Within 'regress' function
fit <- regress(data, Return ~ ., m("nnet", decay=0.5, size = 8),
               .mask = c("Date", "Industry"))

# Within 'classify' function
fit <- classify(iris, Species ~ ., m("nnet", decay=0.5, size = 8))

}
\author{
Phil Holzmeister
}
