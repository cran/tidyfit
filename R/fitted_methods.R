# Generic methods to generate tidy fitted frames

#' @importFrom dplyr tibble

.fitted.default <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = fitted(object)
  )
  return(fitted)
}

.fitted.glmnet <- function(object, self = NULL, ...) {
  x <- object$call$x
  pred_mat <- stats::predict(object, x, type = "response", s = self$args$lambda)

  if (length(dim(pred_mat))==3) {
    class_vals <- dimnames(pred_mat)[[2]]
  } else {
    class_vals <- NULL
  }
  pred <- pred_mat %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(row_n = dplyr::row_number())
  if (self$args$family == "multinomial") {
    pred <- pred %>%
      tidyr::pivot_longer(-dplyr::any_of(c("row_n")),
                          names_to = c("class", "grid_id"),
                          values_to = "fitted",
                          names_sep = "\\.")
  } else {
    pred <- pred %>%
      tidyr::gather("grid_id", "fitted", -dplyr::any_of(c("row_n")))
  }
  pred <- pred %>%
    dplyr::select(-.data$row_n, -.data$grid_id)
  if (length(class_vals)==2) {
    pred <- pred %>%
      dplyr::filter(.data$class == sort(class_vals)[2]) %>%
      dplyr::select(-.data$class)
  }
  return(pred)
}

.fitted.mvr <- function(object, self = NULL, ...) {
  fitted <- dplyr::tibble(
    fitted = drop(fitted(object)[,,self$args$ncomp])
  )
  return(fitted)
}

.fitted.glmboost <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = drop(fitted(object))
  )
  return(fitted)
}

.fitted.shrinkTVP <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = colMeans(t(fitted(object)))
  )
  return(fitted)
}

.fitted.MSM.lm <- function(object, self = NULL, ...) {
  condMean <- object@Fit@CondMean
  probs <- object@Fit@smoProb[-1,]
  fitted <- dplyr::tibble(
    fitted = rowSums(condMean * probs)
  )
  return(fitted)
}

.fitted.cv.hfr <- function(object, self = NULL, ...) {
  fitted <- dplyr::tibble(
    fitted = drop(predict(object, kappa = self$args$kappa_grid))
  )
  return(fitted)

}

.fitted.bma <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = predict(object)
  )
  return(fitted)
}


