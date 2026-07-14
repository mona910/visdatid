# Internal data inspection -------------------------------------------------

.inspect_data <- function(data, outlier_multiplier = 1.5) {
  .validate_data_frame(data)

  if (
    !is.numeric(outlier_multiplier) ||
    length(outlier_multiplier) != 1L ||
    is.na(outlier_multiplier) ||
    !is.finite(outlier_multiplier) ||
    outlier_multiplier <= 0
  ) {
    stop(
      "`outlier_multiplier` harus berupa satu angka positif.",
      call. = FALSE
    )
  }

  number_of_rows <- nrow(data)
  number_of_columns <- ncol(data)

  variable_summary <- lapply(
    names(data),
    function(variable_name) {
      variable <- data[[variable_name]]

      number_missing <- sum(is.na(variable))
      number_non_missing <- sum(!is.na(variable))

      percent_missing <- if (number_of_rows > 0L) {
        100 * number_missing / number_of_rows
      } else {
        NA_real_
      }

      number_unique <- length(
        unique(variable[!is.na(variable)])
      )

      all_missing <- number_non_missing == 0L

      constant <- (
        number_non_missing > 0L &&
          number_unique == 1L
      )

      is_numeric_variable <- is.numeric(variable)
      is_character_variable <- (
        is.character(variable) ||
          is.factor(variable)
      )

      number_blank <- 0L
      percent_blank <- NA_real_

      if (is_character_variable) {
        character_values <- as.character(variable)

        blank_values <- (
          !is.na(character_values) &
            trimws(character_values) == ""
        )

        number_blank <- sum(blank_values)

        percent_blank <- if (number_of_rows > 0L) {
          100 * number_blank / number_of_rows
        } else {
          NA_real_
        }
      }

      number_infinite <- NA_integer_
      number_zero <- NA_integer_
      number_negative <- NA_integer_
      mean_value <- NA_real_
      median_value <- NA_real_
      standard_deviation <- NA_real_
      skewness <- NA_real_
      kurtosis <- NA_real_
      number_outlier <- NA_integer_
      percent_outlier <- NA_real_

      if (is_numeric_variable) {
        finite_values <- variable[is.finite(variable)]

        number_infinite <- sum(
          is.infinite(variable),
          na.rm = TRUE
        )

        number_zero <- sum(
          variable == 0,
          na.rm = TRUE
        )

        number_negative <- sum(
          variable < 0,
          na.rm = TRUE
        )

        if (length(finite_values) > 0L) {
          mean_value <- mean(finite_values)
          median_value <- stats::median(finite_values)
        }

        if (length(finite_values) > 1L) {
          standard_deviation <- stats::sd(finite_values)
        }

        skewness <- .calc_skewness(variable)
        kurtosis <- .calc_kurtosis(variable)

        outlier_result <- .detect_outliers_iqr(
          variable,
          multiplier = outlier_multiplier
        )

        number_outlier <- sum(
          outlier_result,
          na.rm = TRUE
        )

        number_finite <- sum(is.finite(variable))

        percent_outlier <- if (number_finite > 0L) {
          100 * number_outlier / number_finite
        } else {
          NA_real_
        }
      }

      tibble::tibble(
        variable = variable_name,
        class = paste(class(variable), collapse = ", "),
        type = typeof(variable),
        numeric = is_numeric_variable,
        n = number_of_rows,
        n_non_missing = number_non_missing,
        n_missing = number_missing,
        pct_missing = percent_missing,
        n_unique = number_unique,
        all_missing = all_missing,
        constant = constant,
        n_blank = number_blank,
        pct_blank = percent_blank,
        n_infinite = number_infinite,
        n_zero = number_zero,
        n_negative = number_negative,
        mean = mean_value,
        median = median_value,
        sd = standard_deviation,
        skewness = skewness,
        skewness_direction = if (is_numeric_variable) {
          .skewness_direction(skewness)
        } else {
          NA_character_
        },
        skewness_category = if (is_numeric_variable) {
          .classify_skewness(skewness)
        } else {
          NA_character_
        },
        kurtosis = kurtosis,
        n_outlier = number_outlier,
        pct_outlier = percent_outlier
      )
    }
  )

  variable_summary <- dplyr::bind_rows(variable_summary)

  number_complete_rows <- sum(
    stats::complete.cases(data)
  )

  percent_complete_rows <- if (number_of_rows > 0L) {
    100 * number_complete_rows / number_of_rows
  } else {
    NA_real_
  }

  number_duplicated_rows <- sum(
    duplicated(data)
  )

  percent_duplicated_rows <- if (number_of_rows > 0L) {
    100 * number_duplicated_rows / number_of_rows
  } else {
    NA_real_
  }

  overview <- tibble::tibble(
    n_rows = number_of_rows,
    n_columns = number_of_columns,
    n_complete_rows = number_complete_rows,
    pct_complete_rows = percent_complete_rows,
    n_duplicated_rows = number_duplicated_rows,
    pct_duplicated_rows = percent_duplicated_rows,
    n_numeric_columns = sum(
      vapply(data, is.numeric, logical(1))
    ),
    n_non_numeric_columns = sum(
      !vapply(data, is.numeric, logical(1))
    )
  )

  result <- list(
    overview = overview,
    variables = variable_summary
  )

  class(result) <- c(
    "visdatid_inspection",
    "list"
  )

  result
}
