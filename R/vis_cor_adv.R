#' Advanced correlation visualisation and diagnostics
#'
#' @description
#' `vis_cor_adv()` extends correlation visualisation by combining correlation
#' coefficients, pairwise sample sizes, adjusted significance tests,
#' strong-correlation flags, automatic method selection, and optional
#' hierarchical clustering.
#'
#' @param data A data frame containing at least two eligible numerical
#'   variables.
#' @param method Correlation method. One of `"auto"`, `"pearson"`,
#'   `"spearman"`, or `"kendall"`. With `"auto"`, Spearman correlation is used
#'   when at least one eligible variable has absolute skewness of at least one
#'   or an outlier percentage of at least five percent. Otherwise, Pearson
#'   correlation is used.
#' @param adjust_method A p-value adjustment method accepted by
#'   [stats::p.adjust()]. The default is `"BH"`.
#' @param alpha A number between zero and one specifying the adjusted
#'   significance threshold.
#' @param strong_threshold A number between zero and one specifying the
#'   absolute correlation threshold used to mark strong relationships.
#' @param cluster A logical value indicating whether variables should be
#'   ordered using hierarchical clustering based on
#'   `1 - abs(correlation)`.
#' @param show_values A logical value indicating whether correlation
#'   coefficients should be printed on the tiles.
#' @param show_significance A logical value indicating whether statistically
#'   significant correlations should receive an asterisk.
#' @param digits A nonnegative integer specifying the number of decimal places
#'   displayed in correlation labels.
#'
#' @return
#' A `ggplot2` object with two attributes:
#'
#' * `"correlation_table"`: a long-format table containing correlations,
#'   raw p-values, adjusted p-values, complete-pair sample sizes, significance,
#'   and strong-correlation indicators;
#' * `"correlation_diagnostics"`: the selected method, selection reason,
#'   thresholds, adjustment method, excluded variables, and plotting options.
#'
#' @details
#' Correlations are estimated using finite pairwise observations. P-values are
#' calculated for unique variable pairs and adjusted using the selected
#' multiple-testing method. The adjusted p-values are then mirrored across the
#' correlation matrix.
#'
#' A strong border indicates an absolute correlation equal to or greater than
#' `strong_threshold`. An asterisk indicates an adjusted p-value below
#' `alpha`.
#'
#' Automatic method selection is intended as an exploratory convenience,
#' rather than a replacement for substantive and methodological judgement.
#'
#' This function extends the correlation visualisation concept of
#' [visdat::vis_cor()] with significance diagnostics, multiplicity
#' adjustment, strong-correlation flags, and data-informed method selection.
#'
#' @examples
#' vis_cor_adv(mtcars)
#'
#' vis_cor_adv(
#'   mtcars,
#'   method = "spearman",
#'   strong_threshold = 0.6
#' )
#'
#' @seealso [visdat::vis_cor()], [vis_distribution()]
#'
#' @export
vis_cor_adv <- function(
    data,
    method = c(
      "auto",
      "pearson",
      "spearman",
      "kendall"
    ),
    adjust_method = "BH",
    alpha = 0.05,
    strong_threshold = 0.7,
    cluster = TRUE,
    show_values = TRUE,
    show_significance = TRUE,
    digits = 2L
) {
  .validate_data_frame(data)

  method <- match.arg(method)

  if (
    !is.character(adjust_method) ||
    length(adjust_method) != 1L ||
    is.na(adjust_method) ||
    !adjust_method %in% stats::p.adjust.methods
  ) {
    stop(
      paste0(
        "`adjust_method` harus merupakan salah satu metode berikut: ",
        paste(
          stats::p.adjust.methods,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(alpha) ||
    length(alpha) != 1L ||
    is.na(alpha) ||
    !is.finite(alpha) ||
    alpha <= 0 ||
    alpha >= 1
  ) {
    stop(
      "`alpha` harus berupa satu angka yang lebih besar dari 0 dan kurang dari 1.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(strong_threshold) ||
    length(strong_threshold) != 1L ||
    is.na(strong_threshold) ||
    !is.finite(strong_threshold) ||
    strong_threshold < 0 ||
    strong_threshold > 1
  ) {
    stop(
      "`strong_threshold` harus berupa satu angka antara 0 dan 1.",
      call. = FALSE
    )
  }

  logical_arguments <- list(
    cluster = cluster,
    show_values = show_values,
    show_significance = show_significance
  )

  for (argument_name in names(logical_arguments)) {
    argument_value <- logical_arguments[[argument_name]]

    if (
      !is.logical(argument_value) ||
      length(argument_value) != 1L ||
      is.na(argument_value)
    ) {
      stop(
        paste0(
          "`",
          argument_name,
          "` harus berupa satu nilai TRUE atau FALSE."
        ),
        call. = FALSE
      )
    }
  }

  if (
    !is.numeric(digits) ||
    length(digits) != 1L ||
    is.na(digits) ||
    !is.finite(digits) ||
    digits < 0 ||
    digits != as.integer(digits)
  ) {
    stop(
      "`digits` harus berupa satu bilangan bulat nonnegatif.",
      call. = FALSE
    )
  }

  eligible_result <- .eligible_numeric_data(
    data
  )

  numeric_data <- eligible_result$data
  excluded_variables <- eligible_result$excluded

  if (length(excluded_variables) > 0L) {
    warning(
      paste0(
        "Variabel berikut dikeluarkan karena data valid atau variasinya ",
        "tidak mencukupi: ",
        paste(
          excluded_variables,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  method_result <- .choose_correlation_method(
    numeric_data,
    method = method
  )

  selected_method <- method_result$method

  matrix_result <- .build_correlation_matrices(
    numeric_data,
    method = selected_method,
    adjust_method = adjust_method
  )

  variable_order <- .correlation_variable_order(
    matrix_result$correlation,
    cluster = cluster
  )

  variable_names <- names(numeric_data)

  plot_data <- expand.grid(
    row_variable = variable_names,
    column_variable = variable_names,
    stringsAsFactors = FALSE
  )

  row_index <- match(
    plot_data$row_variable,
    variable_names
  )

  column_index <- match(
    plot_data$column_variable,
    variable_names
  )

  matrix_index <- cbind(
    row_index,
    column_index
  )

  plot_data$correlation <-
    matrix_result$correlation[matrix_index]

  plot_data$p_value <-
    matrix_result$p_value[matrix_index]

  plot_data$adjusted_p_value <-
    matrix_result$adjusted_p[matrix_index]

  plot_data$n_complete <-
    matrix_result$n_complete[matrix_index]

  plot_data$diagonal <- (
    plot_data$row_variable ==
      plot_data$column_variable
  )

  plot_data$significant <- (
    !plot_data$diagonal &
      is.finite(
        plot_data$adjusted_p_value
      ) &
      plot_data$adjusted_p_value < alpha
  )

  plot_data$strong <- (
    !plot_data$diagonal &
      is.finite(
        plot_data$correlation
      ) &
      abs(
        plot_data$correlation
      ) >= strong_threshold
  )

  correlation_label <- ifelse(
    is.finite(plot_data$correlation),
    formatC(
      plot_data$correlation,
      format = "f",
      digits = as.integer(digits)
    ),
    "NA"
  )

  significance_marker <- ifelse(
    show_significance &
      plot_data$significant,
    "*",
    ""
  )

  plot_data$label_text <- paste0(
    correlation_label,
    significance_marker
  )

  plot_data$column_factor <- factor(
    plot_data$column_variable,
    levels = variable_order
  )

  plot_data$row_factor <- factor(
    plot_data$row_variable,
    levels = rev(variable_order)
  )

  plot_data <- tibble::as_tibble(
    plot_data
  )

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$column_factor,
      y = .data$row_factor,
      fill = .data$correlation
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.3
    ) +
    ggplot2::scale_fill_gradient2(
      limits = c(-1, 1),
      midpoint = 0,
      na.value = "grey90"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Diagnosis Korelasi Antarvariabel",
      subtitle = paste0(
        "Metode: ",
        tools::toTitleCase(selected_method),
        " | Penyesuaian p-value: ",
        adjust_method
      ),
      x = NULL,
      y = NULL,
      fill = "Korelasi",
      caption = paste0(
        if (show_significance) {
          paste0(
            "* adjusted p-value < ",
            alpha,
            ". "
          )
        } else {
          ""
        },
        "Garis tebal menunjukkan |korelasi| >= ",
        strong_threshold,
        ". ",
        method_result$reason
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  strong_data <- plot_data[
    plot_data$strong,
    ,
    drop = FALSE
  ]

  if (nrow(strong_data) > 0L) {
    plot <- plot +
      ggplot2::geom_tile(
        data = strong_data,
        mapping = ggplot2::aes(
          x = .data$column_factor,
          y = .data$row_factor
        ),
        inherit.aes = FALSE,
        fill = NA,
        colour = "black",
        linewidth = 0.8
      )
  }

  if (show_values) {
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(
          label = .data$label_text
        ),
        size = 3
      )
  }

  correlation_table <- plot_data[
    ,
    c(
      "row_variable",
      "column_variable",
      "correlation",
      "p_value",
      "adjusted_p_value",
      "n_complete",
      "significant",
      "strong"
    )
  ]

  attr(
    plot,
    "correlation_table"
  ) <- correlation_table

  attr(
    plot,
    "correlation_diagnostics"
  ) <- list(
    requested_method = method,
    selected_method = selected_method,
    method_reason = method_result$reason,
    adjust_method = adjust_method,
    alpha = alpha,
    strong_threshold = strong_threshold,
    cluster = cluster,
    variable_order = variable_order,
    excluded_variables = excluded_variables,
    show_values = show_values,
    show_significance = show_significance
  )

  plot
}
