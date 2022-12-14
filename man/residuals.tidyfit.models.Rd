% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residuals.tidyfit.models.R
\name{residuals.tidyfit.models}
\alias{residuals.tidyfit.models}
\title{Obtain residuals from models in a \code{tidyfit.models} frame}
\usage{
\method{residuals}{tidyfit.models}(object, ...)
}
\arguments{
\item{object}{\code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}}

\item{...}{currently not used}
}
\value{
A 'tibble'.
}
\description{
The function generates residuals for all models in a \code{tidyfit.models} frame and outputs a tidy frame.
}
\details{
The function uses the 'model_object' column in a \code{tidyfit.model} frame to return residuals for each model.
}
\examples{
data <- dplyr::group_by(tidyfit::Factor_Industry_Returns, Industry)
fit <- regress(data, Return ~ ., m("lm"), .mask = "Date")
resid(fit)

}
\seealso{
\code{\link{coef.tidyfit.models}}, \code{\link{predict.tidyfit.models}} and \code{\link{fitted.tidyfit.models}}
}
\author{
Johann Pfitzinger
}
