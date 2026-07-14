#' Visualise and diagnose numerical distributions
#'
#' @description
#' `vis_distribution()` provides an exploratory diagnosis of numerical
#' variables. When `variable = NULL`, the function compares skewness across
#' all eligible numerical variables. When a variable name is supplied, the
#' function displays its histogram together with descriptive statistics,
#' outlier markers, and a cautious transformation recommendation.
#'
#' @param data A data frame.
#' @param variable A single character value containing the name of a numerical
#'   variable. Use `NULL` to display an overview of all numerical variables.
#' @param bins A positive integer specifying the number of histogram bins.
#'   This argument is used only when `variable` is supplied.
#' @param outlier_multiplier A positive number used as the multiplier in the
#'   interquartile-range outlier rule. The default is `1.5`.
#' @param show_outliers A logical value indicating whether detected outliers
#'   should be marked with a rug below the histogram.
#'
#' @return
#' A `ggplot2` object. The overview mode displays skewness across numerical
#' variables, whereas the detail mode displays a histogram for one variable.
#'
#' @details
#' Skewness is calculated using an adjusted sample estimator. Outliers are
#' identified using the interquartile-range rule:
#' `Q1 - multiplier * IQR` and `Q3 + multiplier * IQR`.
#'
#' Transformation recommendations are exploratory and should not be treated
#' as automatic preprocessing decisions. The analytical objective, measurement
#' scale, presence of outliers, and statistical method should also be
#' considered.
#'
#' @examples
#' vis_distribution(mtcars)
#'
#' vis_distribution(
#'   mtcars,
#'   variable = "mpg"
#' )
#'
#' @seealso [quality_report()]
#'
#' @export
vis_distribution <- function(
    data,
    variable = NULL,
    bins = 30L,
    outlier_multiplier = 1.5,
    show_outliers = TRUE
) {
  .validate_data_frame(data)

  if (
    !is.numeric(bins) ||
    length(bins) != 1L ||
    is.na(bins) ||
    !is.finite(bins) ||
    bins < 1 ||
    bins != as.integer(bins)
  ) {
    stop(
      "`bins` harus berupa satu bilangan bulat positif.",
      call. = FALSE
    )
  }

  if (
    !is.logical(show_outliers) ||
    length(show_outliers) != 1L ||
    is.na(show_outliers)
  ) {
    stop(
      "`show_outliers` harus berupa satu nilai TRUE atau FALSE.",
      call. = FALSE
    )
  }

  inspection <- .inspect_data(
    data,
    outlier_multiplier = outlier_multiplier
  )

  numeric_summary <- inspection$variables[
    inspection$variables$numeric,
    ,
    drop = FALSE
  ]

  if (nrow(numeric_summary) == 0L) {
    stop(
      "`data` tidak memiliki variabel numerik.",
      call. = FALSE
    )
  }

  if (is.null(variable)) {
    finite_skewness <- is.finite(
      numeric_summary$skewness
    )

    plot_data <- numeric_summary[
      finite_skewness,
      ,
      drop = FALSE
    ]

    excluded_variables <- numeric_summary$variable[
      !finite_skewness
    ]

    if (nrow(plot_data) == 0L) {
      stop(
        paste(
          "Skewness tidak dapat dihitung untuk seluruh",
          "variabel numerik dalam `data`."
        ),
        call. = FALSE
      )
    }

    plot_data$variable_order <- stats::reorder(
      plot_data$variable,
      plot_data$skewness
    )

    caption_text <- if (length(excluded_variables) > 0L) {
      paste0(
        "Variabel yang tidak ditampilkan karena variasi atau ",
        "jumlah data valid tidak mencukupi: ",
        paste(excluded_variables, collapse = ", "),
        "."
      )
    } else {
      paste(
        "Batas praktis: |skewness| < 0,5 relatif simetris;",
        "0,5 sampai kurang dari 1 sedang; minimal 1 kuat."
      )
    }

    plot <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$variable_order,
        y = .data$skewness,
        fill = .data$skewness_category
      )
    ) +
      ggplot2::geom_col(
        width = 0.72
      ) +
      ggplot2::geom_hline(
        yintercept = c(-1, -0.5, 0.5, 1),
        linetype = c(2, 3, 3, 2),
        linewidth = 0.4
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Diagnosis Kemencengan Variabel Numerik",
        subtitle = paste(
          "Nilai positif menunjukkan kemencengan kanan;",
          "nilai negatif menunjukkan kemencengan kiri."
        ),
        x = NULL,
        y = "Skewness",
        fill = "Kategori",
        caption = caption_text
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        panel.grid.major.y = ggplot2::element_blank()
      )

    return(plot)
  }

  if (
    !is.character(variable) ||
    length(variable) != 1L ||
    is.na(variable) ||
    !nzchar(variable)
  ) {
    stop(
      paste(
        "`variable` harus berupa satu nama variabel",
        "dalam bentuk karakter atau NULL."
      ),
      call. = FALSE
    )
  }

  if (!variable %in% names(data)) {
    stop(
      paste0(
        "Variabel `",
        variable,
        "` tidak ditemukan dalam `data`."
      ),
      call. = FALSE
    )
  }

  values <- data[[variable]]

  if (!is.numeric(values)) {
    stop(
      paste0(
        "Variabel `",
        variable,
        "` harus berupa variabel numerik."
      ),
      call. = FALSE
    )
  }

  finite_values <- is.finite(values)

  if (sum(finite_values) == 0L) {
    stop(
      paste0(
        "Variabel `",
        variable,
        "` tidak memiliki nilai numerik hingga yang dapat diplot."
      ),
      call. = FALSE
    )
  }

  outlier_result <- .detect_outliers_iqr(
    values,
    multiplier = outlier_multiplier
  )

  plot_data <- tibble::tibble(
    value = values[finite_values],
    outlier = outlier_result[finite_values]
  )

  summary_row <- numeric_summary[
    numeric_summary$variable == variable,
    ,
    drop = FALSE
  ]

  recommendation <- .suggest_transformation(
    values,
    summary_row$skewness
  )

  subtitle_text <- paste0(
    "n valid = ",
    summary_row$n_non_missing - summary_row$n_infinite,
    " | mean = ",
    format(
      round(summary_row$mean, 2),
      nsmall = 2
    ),
    " | median = ",
    format(
      round(summary_row$median, 2),
      nsmall = 2
    ),
    " | skewness = ",
    format(
      round(summary_row$skewness, 2),
      nsmall = 2
    ),
    " (",
    summary_row$skewness_direction,
    "; ",
    summary_row$skewness_category,
    ") | outlier = ",
    summary_row$n_outlier,
    " (",
    format(
      round(summary_row$pct_outlier, 1),
      nsmall = 1
    ),
    "%)"
  )

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$value
    )
  ) +
    ggplot2::geom_histogram(
      bins = as.integer(bins),
      linewidth = 0.3
    ) +
    ggplot2::geom_vline(
      xintercept = summary_row$mean,
      linetype = "dashed",
      linewidth = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = summary_row$median,
      linetype = "dotted",
      linewidth = 0.7
    ) +
    ggplot2::labs(
      title = paste(
        "Distribusi Variabel",
        variable
      ),
      subtitle = subtitle_text,
      x = variable,
      y = "Frekuensi",
      caption = recommendation
    ) +
    ggplot2::theme_minimal()

  if (
    show_outliers &&
    any(plot_data$outlier, na.rm = TRUE)
  ) {
    outlier_data <- plot_data[
      plot_data$outlier %in% TRUE,
      ,
      drop = FALSE
    ]

    plot <- plot +
      ggplot2::geom_rug(
        data = outlier_data,
        mapping = ggplot2::aes(
          x = .data$value
        ),
        inherit.aes = FALSE,
        sides = "b",
        linewidth = 0.6
      )
  }

  plot
}
