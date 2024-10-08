% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.group_lasso.R
\name{.fit.group_lasso}
\alias{.fit.group_lasso}
\title{Grouped Lasso regression and classification for \code{tidyfit}}
\usage{
\method{.fit}{group_lasso}(self, data = NULL)
}
\arguments{
\item{self}{a 'tidyFit' R6 class.}

\item{data}{a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).}
}
\value{
A fitted 'tidyFit' class model.
}
\description{
Fits a linear regression or classification with a grouped L1 penalty on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
}
\details{
\strong{Hyperparameters:}
\itemize{
\item \code{lambda} \emph{(L1 penalty)}
}

\strong{Important method arguments (passed to \code{\link{m}})}

The Group Lasso regression is estimated using \code{gglasso::gglasso}. The 'group' argument is a named vector passed directly to \code{m()} (see examples). See \code{?gglasso} for more details. Only binomial classification is possible. Weights are ignored for classification.

\strong{Implementation}

Features are standardized by default with coefficients transformed to the original scale.

If no hyperparameter grid is passed (\code{is.null(control$lambda)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100. Note that the grid selection tools provided by \code{gglasso::gglasso} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
}
\examples{
# Load data
data <- tidyfit::Factor_Industry_Returns

groups <- setNames(c(1, 2, 2, 3, 3, 1), c("Mkt-RF", "SMB", "HML", "RMW", "CMA", "RF"))

# Stand-alone function
fit <- m("group_lasso", Return ~ ., data, lambda = 0.5, group = groups)
fit

# Within 'regress' function
fit <- regress(data, Return ~ ., m("group_lasso", lambda = c(0.1, 0.5), group = groups),
               .mask = c("Date", "Industry"))
coef(fit)

}
\references{
Yang Y, Zou H, Bhatnagar S (2020). \emph{gglasso: Group Lasso Penalized Learning Using a Unified BMD Algorithm}. R package version 1.5, \url{https://CRAN.R-project.org/package=gglasso}.
}
\seealso{
\code{\link{.fit.lasso}}, \code{\link{.fit.blasso}}, \code{\link{.fit.adalasso}} and \code{\link{m}} methods
}
\author{
Johann Pfitzinger
}
