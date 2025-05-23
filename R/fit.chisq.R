#' @name .fit.chisq
#' @title Pearson's Chi-squared test for \code{tidyfit}
#' @description Calculates Pearson's Chi-squared test on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{classify}}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{stats::chisq.test}. See \code{?chisq.test} for more details.
#'
#' **Implementation**
#'
#' Results can be viewed using \code{coef}.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::mutate_at(data, dplyr::vars(-Date, -Industry), dplyr::ntile, n = 10)
#'
#' # Within 'classify' function
#' fit <- classify(data, Return ~ ., m("chisq"), .mask = c("Date", "Industry"))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.fit.cor}} and \code{\link{m}} methods
#'
#' @importFrom stats chisq.test
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.chisq <- function(
    self,
    data = NULL
) {

  if (!is.null(self$args$weights)) {
    warning("chisq cannot handle weights, weights are ignored", call. = FALSE)
  }

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]
  var_names <- colnames(x)

  ctr <- self$args[names(self$args) %in% methods::formalArgs(stats::chisq.test)]
  ctr <- ctr[!names(ctr) %in% c("x, y")]

  eval_fun_ <- function(...) {
    tests <- purrr::map(var_names, function(nam) stats::chisq.test(x[,nam], y, ...))
    names(tests) <- var_names
    class(tests) <- "custom.test"
    tests
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun, ctr)
  .store_on_self(self, res)
  invisible(self)
}

.coef.custom.test <- function(object, self = NULL, ...) {

  estimates <- purrr::map_dfr(object, broom::tidy, .id = "term")
  if (!"estimate" %in% colnames(estimates)) {
    estimates <- dplyr::mutate(estimates, estimate = NA)
  }

  return(estimates)

}
