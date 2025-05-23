#' @name .fit.gets
#' @title General-to-specific regression for \code{tidyfit}
#' @description Fits a general-to-specific (GETS) regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
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
#'  - \code{max.paths} *(Number of paths to search)*
#'
#' The function provides a wrapper for \code{gets::gets}. See \code{?gets} for more details.
#'
#' **Implementation**
#'
#' Print output is suppressed by default. Use 'print.searchinfo = TRUE' for print output.
#'
#' @author Johann Pfitzinger
#'
#' @references
#' Pretis F, Reade JJ, Sucarrat G (2018).
#' _Automated General-to-Specific (GETS) Regression Modeling and Indicator Saturation for Outliers and Structural Breaks._
#' Journal of Statistical Software 86(3), 1-44.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("gets", Return ~ `Mkt-RF` + HML + SMB, data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("gets"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.robust}}, \code{\link{.fit.glm}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.gets <- function(
    self,
    data = NULL
) {
  self$set_args(print.searchinfo = FALSE, overwrite = FALSE)
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  response_var <- all.vars(self$formula)[1]
  reg_data <- dplyr::tibble(!!response_var := mf[,1]) |>
    dplyr::bind_cols(dplyr::as_tibble(x)) |>
    dplyr::select(-any_of("(Intercept)")) |>
    data.frame()

  formula <- ifelse("(Intercept)" %in% colnames(x), "~ .", "~ 0 + .")
  formula <- stats::as.formula(paste(make.names(response_var), formula))

  ctr_lm <- self$args[names(self$args) %in% methods::formalArgs(stats::lm)]
  ctr_gets <- self$args[names(self$args) %in% methods::formalArgs(gets::getsm)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(gets::gets, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  mod_lm <- do.call(stats::lm, append(list(formula = formula, data = reg_data), ctr_lm))
  res <- do.call(eval_fun,
                 append(list(x = mod_lm), ctr_gets))
  .store_on_self(self, res)
  invisible(self)
}
