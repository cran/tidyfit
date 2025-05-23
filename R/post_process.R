
.post_process <- function(df, .return_slices, .return_grid, .cv, .tune_each_group,
                          .mask, .weights, gr_vars) {
  if (!.return_slices & .cv == "none") {
    df <- df |>
      dplyr::select(-"slice_id")
  }

  if (.cv != "none") {
    # Select optimal hyperparameter setting
    df_no_cv <- df |>
      dplyr::filter(!sapply(df$model_object, function(mod) mod$cv)) |>
      dplyr::select(-"slice_id")

    df <- df |>
      dplyr::filter(sapply(df$model_object, function(mod) mod$cv))

    if (nrow(df) == 0) {
      df <- df_no_cv
    } else {
      if (.tune_each_group) {
        df <- df |>
          dplyr::group_by(dplyr::across(dplyr::all_of(gr_vars)))
      }

      df_slices <- df |>
        dplyr::filter(.data$slice_id != "FULL")

      if (all(is.na(df_slices$metric)) & (length(unique(df_slices$grid_id)) > 1)) {
        df_slices <- df_slices |>
          dplyr::filter(.data$grid_id == .data$grid_id[1])
        warning("could not select optimal hyperparameter due to model errors. keeping the first hyperparameter set.", call. = FALSE)
      }

      if (!all(is.na(df_slices$metric)) & !.return_grid) {
        df_slices <- df_slices |>
          dplyr::group_by(.data$model, .data$grid_id, .add = TRUE) |>
          dplyr::mutate(metric = mean(.data$metric, na.rm = TRUE)) |>
          dplyr::ungroup("grid_id") |>
          dplyr::filter(.data$metric == min(.data$metric, na.rm = TRUE)) |>
          dplyr::filter(.data$grid_id == unique(.data$grid_id)[1])
      }

      if (.return_slices) {
        df <- df_slices |>
          dplyr::bind_rows(df_no_cv) |>
          dplyr::select(-"metric")
      } else {
        df <- df_slices |>
          dplyr::ungroup() |>
          dplyr::select(!!gr_vars, "grid_id", "model") |>
          dplyr::distinct() |>
          dplyr::left_join(df |> dplyr::ungroup() |> dplyr::filter(.data$slice_id == "FULL"), by = c(gr_vars, "grid_id", "model")) |>
          dplyr::bind_rows(df_no_cv) |>
          dplyr::select(-"metric", -"slice_id")
      }
    }
  }

  df <- .reassign_model_info(df)
  df <- .make_model_cols(df)
  col_ord <- c(gr_vars, "model", "estimator_fct", "size (MB)", "grid_id", "model_object", "settings", "errors", "warnings", "messages")
  df <- dplyr::relocate(df, any_of(col_ord))

  df <- tibble::new_tibble(df, class = "tidyfit.models",
                           structure = list(groups = gr_vars,
                                            mask = .mask,
                                            weights = .weights))

  return(df)
}
