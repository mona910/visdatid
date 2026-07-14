# Internal correlation helpers --------------------------------------------

.eligible_numeric_data <- function(data) {
  .validate_data_frame(data)

  numeric_data <- data[
    vapply(
      data,
      is.numeric,
      logical(1)
    )
  ]

  if (ncol(numeric_data) < 2L) {
    stop(
      "`data` harus memiliki minimal dua variabel numerik.",
      call. = FALSE
    )
  }

  eligible <- vapply(
    numeric_data,
    function(x) {
      finite_values <- x[is.finite(x)]

      length(finite_values) >= 3L &&
        length(unique(finite_values)) >= 2L
    },
    logical(1)
  )

  excluded_variables <- names(numeric_data)[!eligible]
  eligible_data <- numeric_data[eligible]

  if (ncol(eligible_data) < 2L) {
    stop(
      paste(
        "Korelasi membutuhkan minimal dua variabel numerik",
        "dengan sedikitnya tiga nilai hingga dan variasi yang cukup."
      ),
      call. = FALSE
    )
  }

  list(
    data = eligible_data,
    excluded = excluded_variables
  )
}


.choose_correlation_method <- function(
    data,
    method = c(
      "auto",
      "pearson",
      "spearman",
      "kendall"
    )
) {
  method <- match.arg(method)

  if (method != "auto") {
    return(
      list(
        method = method,
        reason = paste0(
          "Metode ",
          method,
          " dipilih oleh pengguna."
        )
      )
    )
  }

  inspection <- .inspect_data(data)
  variable_summary <- inspection$variables

  strong_skewness <- any(
    abs(variable_summary$skewness) >= 1,
    na.rm = TRUE
  )

  substantial_outliers <- any(
    variable_summary$pct_outlier >= 5,
    na.rm = TRUE
  )

  if (strong_skewness || substantial_outliers) {
    return(
      list(
        method = "spearman",
        reason = paste(
          "Metode Spearman dipilih otomatis karena terdapat",
          "kemencengan kuat atau proporsi pencilan minimal 5%."
        )
      )
    )
  }

  list(
    method = "pearson",
    reason = paste(
      "Metode Pearson dipilih otomatis karena tidak terdeteksi",
      "kemencengan kuat atau proporsi pencilan minimal 5%."
    )
  )
}


.cor_test_pair <- function(
    x,
    y,
    method
) {
  valid_pair <- (
    is.finite(x) &
      is.finite(y)
  )

  number_complete <- sum(valid_pair)

  if (number_complete < 3L) {
    return(
      list(
        correlation = NA_real_,
        p_value = NA_real_,
        n_complete = number_complete
      )
    )
  }

  x_complete <- x[valid_pair]
  y_complete <- y[valid_pair]

  if (
    length(unique(x_complete)) < 2L ||
    length(unique(y_complete)) < 2L
  ) {
    return(
      list(
        correlation = NA_real_,
        p_value = NA_real_,
        n_complete = number_complete
      )
    )
  }

  test_result <- tryCatch(
    suppressWarnings(
      stats::cor.test(
        x_complete,
        y_complete,
        method = method,
        exact = FALSE
      )
    ),
    error = function(e) {
      NULL
    }
  )

  if (is.null(test_result)) {
    return(
      list(
        correlation = NA_real_,
        p_value = NA_real_,
        n_complete = number_complete
      )
    )
  }

  list(
    correlation = unname(
      test_result$estimate
    ),
    p_value = test_result$p.value,
    n_complete = number_complete
  )
}


.build_correlation_matrices <- function(
    data,
    method,
    adjust_method
) {
  variable_names <- names(data)
  number_variables <- ncol(data)

  correlation_matrix <- matrix(
    NA_real_,
    nrow = number_variables,
    ncol = number_variables,
    dimnames = list(
      variable_names,
      variable_names
    )
  )

  p_value_matrix <- correlation_matrix
  adjusted_p_matrix <- correlation_matrix

  n_complete_matrix <- matrix(
    NA_integer_,
    nrow = number_variables,
    ncol = number_variables,
    dimnames = list(
      variable_names,
      variable_names
    )
  )

  for (i in seq_len(number_variables)) {
    correlation_matrix[i, i] <- 1
    p_value_matrix[i, i] <- NA_real_
    adjusted_p_matrix[i, i] <- NA_real_

    n_complete_matrix[i, i] <- sum(
      is.finite(data[[i]])
    )

    if (i < number_variables) {
      for (j in seq.int(
        from = i + 1L,
        to = number_variables
      )) {
        pair_result <- .cor_test_pair(
          data[[i]],
          data[[j]],
          method = method
        )

        correlation_matrix[i, j] <-
          pair_result$correlation

        correlation_matrix[j, i] <-
          pair_result$correlation

        p_value_matrix[i, j] <-
          pair_result$p_value

        p_value_matrix[j, i] <-
          pair_result$p_value

        n_complete_matrix[i, j] <-
          pair_result$n_complete

        n_complete_matrix[j, i] <-
          pair_result$n_complete
      }
    }
  }

  upper_positions <- which(
    upper.tri(p_value_matrix) &
      is.finite(p_value_matrix),
    arr.ind = TRUE
  )

  if (nrow(upper_positions) > 0L) {
    raw_p_values <- p_value_matrix[
      upper_positions
    ]

    adjusted_values <- stats::p.adjust(
      raw_p_values,
      method = adjust_method
    )

    for (k in seq_len(nrow(upper_positions))) {
      row_position <- upper_positions[k, 1]
      column_position <- upper_positions[k, 2]

      adjusted_p_matrix[
        row_position,
        column_position
      ] <- adjusted_values[k]

      adjusted_p_matrix[
        column_position,
        row_position
      ] <- adjusted_values[k]
    }
  }

  list(
    correlation = correlation_matrix,
    p_value = p_value_matrix,
    adjusted_p = adjusted_p_matrix,
    n_complete = n_complete_matrix
  )
}


.correlation_variable_order <- function(
    correlation_matrix,
    cluster
) {
  if (
    !is.matrix(correlation_matrix) ||
    nrow(correlation_matrix) != ncol(correlation_matrix)
  ) {
    stop(
      "`correlation_matrix` harus berupa matriks persegi.",
      call. = FALSE
    )
  }

  original_order <- colnames(
    correlation_matrix
  )

  if (is.null(original_order)) {
    original_order <- as.character(
      seq_len(ncol(correlation_matrix))
    )
  }

  if (!cluster || length(original_order) <= 1L) {
    return(original_order)
  }

  clustering_matrix <- correlation_matrix

  clustering_matrix[
    !is.finite(clustering_matrix)
  ] <- 0

  diag(clustering_matrix) <- 1

  dissimilarity_matrix <- (
    1 - abs(clustering_matrix)
  )

  dissimilarity_matrix[
    dissimilarity_matrix < 0
  ] <- 0

  dissimilarity_matrix[
    dissimilarity_matrix > 1
  ] <- 1

  diag(dissimilarity_matrix) <- 0

  dimnames(dissimilarity_matrix) <- dimnames(
    clustering_matrix
  )

  distance_matrix <- stats::as.dist(
    dissimilarity_matrix
  )

  clustering_result <- stats::hclust(
    distance_matrix,
    method = "complete"
  )

  original_order[
    clustering_result$order
  ]
}
