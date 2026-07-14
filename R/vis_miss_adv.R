# Internal missing-data helpers -------------------------------------------

.build_missing_summary <- function(data, threshold) {
  number_of_rows <- nrow(data)

  number_missing <- vapply(
    data,
    function(x) {
      sum(is.na(x))
    },
    integer(1)
  )

  percent_missing <- if (number_of_rows > 0L) {
    100 * number_missing / number_of_rows
  } else {
    rep(NA_real_, ncol(data))
  }

  tibble::tibble(
    variable = names(data),
    n_missing = unname(number_missing),
    pct_missing = unname(percent_missing),
    flagged = (
      !is.na(.data$pct_missing) &
        .data$pct_missing >= threshold
    )
  )
}


.systematic_row_indices <- function(
    number_of_rows,
    maximum_rows
) {
  if (number_of_rows <= maximum_rows) {
    return(seq_len(number_of_rows))
  }

  indices <- floor(
    (
      seq_len(maximum_rows) - 0.5
    ) *
      number_of_rows /
      maximum_rows
  ) + 1L

  unique(
    pmin(
      indices,
      number_of_rows
    )
  )
}


#' Advanced visualisation of missing-data patterns
#'
#' @description
#' `vis_miss_adv()` extends missing-data visualisation by combining a
#' cell-level missingness map with exact variable summaries, configurable
#' warning thresholds, deterministic sampling for large data, and optional
#' group-level missingness comparisons.
#'
#' @param data A data frame.
#' @param group An optional character value containing the name of a grouping
#'   variable. When supplied, the function displays missing percentages for
#'   every variable within each group.
#' @param threshold A number between 0 and 100. Variables or group-variable
#'   combinations with missing percentages equal to or above this value are
#'   marked with an asterisk. The default is `20`.
#' @param max_cells A positive integer specifying the approximate maximum
#'   number of cells displayed in the cell-level plot. If the data exceed this
#'   value, rows are selected systematically. Percentages are still calculated
#'   from the complete data. This argument is not used in grouped mode.
#' @param sort_by_missing A logical value indicating whether variables should
#'   be ordered from the highest to the lowest overall missing percentage.
#' @param show_labels A logical value indicating whether percentage labels
#'   should be displayed in grouped mode.
#'
#' @return
#' A `ggplot2` object.
#'
#' The plot contains the following attributes:
#'
#' * `"missing_summary"`: the missing-data summary used by the plot;
#' * `"sampling_info"`: information about row sampling;
#' * `"mode"`: either `"cell"` or `"grouped"`.
#'
#' @details
#' In cell mode, the displayed rows may be sampled systematically when the
#' number of cells exceeds `max_cells`. Missing percentages and warning flags
#' are always computed using the complete input data.
#'
#' In grouped mode, the grouping variable is used only to define groups and is
#' excluded from the variables whose missingness is visualised.
#'
#' This function extends the missing-data visualisation concept of
#' [visdat::vis_miss()] with threshold-based diagnostics, deterministic
#' downsampling, and exact group-level summaries.
#'
#' @examples
#' vis_miss_adv(airquality)
#'
#' vis_miss_adv(
#'   airquality,
#'   group = "Month",
#'   threshold = 15
#' )
#'
#' @seealso [visdat::vis_miss()], [quality_report()]
#'
#' @export
vis_miss_adv <- function(
    data,
    group = NULL,
    threshold = 20,
    max_cells = 100000L,
    sort_by_missing = TRUE,
    show_labels = TRUE
) {
  .validate_data_frame(data)

  if (nrow(data) == 0L) {
    stop(
      "`data` harus memiliki minimal satu baris.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(threshold) ||
    length(threshold) != 1L ||
    is.na(threshold) ||
    !is.finite(threshold) ||
    threshold < 0 ||
    threshold > 100
  ) {
    stop(
      "`threshold` harus berupa satu angka antara 0 dan 100.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(max_cells) ||
    length(max_cells) != 1L ||
    is.na(max_cells) ||
    !is.finite(max_cells) ||
    max_cells < 1 ||
    max_cells != as.integer(max_cells)
  ) {
    stop(
      "`max_cells` harus berupa satu bilangan bulat positif.",
      call. = FALSE
    )
  }

  if (
    !is.logical(sort_by_missing) ||
    length(sort_by_missing) != 1L ||
    is.na(sort_by_missing)
  ) {
    stop(
      "`sort_by_missing` harus berupa satu nilai TRUE atau FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.logical(show_labels) ||
    length(show_labels) != 1L ||
    is.na(show_labels)
  ) {
    stop(
      "`show_labels` harus berupa satu nilai TRUE atau FALSE.",
      call. = FALSE
    )
  }

  if (!is.null(group)) {
    if (
      !is.character(group) ||
      length(group) != 1L ||
      is.na(group) ||
      !nzchar(group)
    ) {
      stop(
        paste(
          "`group` harus berupa satu nama variabel",
          "dalam bentuk karakter atau NULL."
        ),
        call. = FALSE
      )
    }

    if (!group %in% names(data)) {
      stop(
        paste0(
          "Variabel kelompok `",
          group,
          "` tidak ditemukan dalam `data`."
        ),
        call. = FALSE
      )
    }
  }

  overall_summary <- .build_missing_summary(
    data,
    threshold = threshold
  )

  ordered_variables <- overall_summary$variable

  if (sort_by_missing) {
    ordered_variables <- overall_summary$variable[
      order(
        -overall_summary$pct_missing,
        overall_summary$variable
      )
    ]
  }

  # Cell-level visualisation -----------------------------------------------

  if (is.null(group)) {
    maximum_rows <- max(
      1L,
      floor(
        max_cells / ncol(data)
      )
    )

    row_indices <- .systematic_row_indices(
      number_of_rows = nrow(data),
      maximum_rows = maximum_rows
    )

    plot_data <- data[
      row_indices,
      ,
      drop = FALSE
    ]

    missing_matrix <- vapply(
      plot_data,
      is.na,
      logical(nrow(plot_data))
    )

    long_data <- tibble::tibble(
      row_position = rep(
        seq_len(nrow(plot_data)),
        times = ncol(plot_data)
      ),
      source_row = rep(
        row_indices,
        times = ncol(plot_data)
      ),
      variable = rep(
        names(plot_data),
        each = nrow(plot_data)
      ),
      missing = as.vector(missing_matrix)
    )

    long_data$status <- factor(
      ifelse(
        long_data$missing,
        "Missing",
        "Teramati"
      ),
      levels = c(
        "Teramati",
        "Missing"
      )
    )

    variable_labels <- paste0(
      overall_summary$variable,
      "\n",
      format(
        round(
          overall_summary$pct_missing,
          1
        ),
        nsmall = 1
      ),
      "%",
      ifelse(
        overall_summary$flagged,
        " *",
        ""
      )
    )

    names(variable_labels) <- overall_summary$variable

    long_data$variable_label <- factor(
      variable_labels[
        long_data$variable
      ],
      levels = variable_labels[
        ordered_variables
      ]
    )

    sampling_used <- (
      length(row_indices) < nrow(data)
    )

    caption_text <- paste0(
      "* Persentase missing mencapai atau melebihi ambang ",
      format(
        round(threshold, 1),
        nsmall = 1
      ),
      "%. Persentase dihitung dari seluruh data."
    )

    if (sampling_used) {
      caption_text <- paste0(
        caption_text,
        " Plot menampilkan ",
        length(row_indices),
        " dari ",
        nrow(data),
        " baris melalui sampling sistematis."
      )
    }

    plot <- ggplot2::ggplot(
      long_data,
      ggplot2::aes(
        x = .data$variable_label,
        y = .data$row_position,
        fill = .data$status
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_y_reverse() +
      ggplot2::labs(
        title = "Pola Missing Value",
        subtitle = paste0(
          "Data: ",
          nrow(data),
          " baris dan ",
          ncol(data),
          " variabel"
        ),
        x = NULL,
        y = "Urutan baris yang ditampilkan",
        fill = "Status",
        caption = caption_text
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

    attr(
      plot,
      "missing_summary"
    ) <- overall_summary

    attr(
      plot,
      "sampling_info"
    ) <- list(
      used = sampling_used,
      original_rows = nrow(data),
      plotted_rows = length(row_indices),
      max_cells = as.integer(max_cells),
      method = if (sampling_used) {
        "systematic"
      } else {
        "none"
      }
    )

    attr(
      plot,
      "mode"
    ) <- "cell"

    return(plot)
  }

  # Group-level visualisation ----------------------------------------------

  analysed_variables <- setdiff(
    names(data),
    group
  )

  if (length(analysed_variables) == 0L) {
    stop(
      paste(
        "`data` harus memiliki minimal satu variabel",
        "selain variabel kelompok."
      ),
      call. = FALSE
    )
  }

  group_values <- as.character(
    data[[group]]
  )

  group_values[
    is.na(data[[group]])
  ] <- "<NA>"

  group_levels <- unique(group_values)

  grouped_rows <- list()
  row_counter <- 1L

  for (group_level in group_levels) {
    group_index <- (
      group_values == group_level
    )

    group_size <- sum(group_index)

    for (variable_name in analysed_variables) {
      number_missing <- sum(
        is.na(
          data[[variable_name]][group_index]
        )
      )

      percent_missing <- (
        100 *
          number_missing /
          group_size
      )

      grouped_rows[[row_counter]] <- tibble::tibble(
        group_value = group_level,
        variable = variable_name,
        n_rows = group_size,
        n_missing = number_missing,
        pct_missing = percent_missing,
        flagged = percent_missing >= threshold
      )

      row_counter <- row_counter + 1L
    }
  }

  grouped_summary <- dplyr::bind_rows(
    grouped_rows
  )

  overall_analysed <- overall_summary[
    overall_summary$variable %in%
      analysed_variables,
    ,
    drop = FALSE
  ]

  ordered_group_variables <- overall_analysed$variable

  if (sort_by_missing) {
    ordered_group_variables <-
      overall_analysed$variable[
        order(
          -overall_analysed$pct_missing,
          overall_analysed$variable
        )
      ]
  }

  grouped_summary$variable_label <- factor(
    grouped_summary$variable,
    levels = ordered_group_variables
  )

  grouped_summary$group_label <- factor(
    grouped_summary$group_value,
    levels = rev(group_levels)
  )

  grouped_summary$label_text <- paste0(
    format(
      round(
        grouped_summary$pct_missing,
        1
      ),
      nsmall = 1
    ),
    "%",
    ifelse(
      grouped_summary$flagged,
      " *",
      ""
    )
  )

  plot <- ggplot2::ggplot(
    grouped_summary,
    ggplot2::aes(
      x = .data$variable_label,
      y = .data$group_label,
      fill = .data$pct_missing
    )
  ) +
    ggplot2::geom_tile(
      linewidth = 0.3
    ) +
    ggplot2::scale_fill_gradient(
      limits = c(0, 100)
    ) +
    ggplot2::labs(
      title = "Persentase Missing Value Menurut Kelompok",
      subtitle = paste0(
        "Variabel kelompok: ",
        group
      ),
      x = NULL,
      y = group,
      fill = "Missing (%)",
      caption = paste0(
        "* Persentase missing mencapai atau melebihi ambang ",
        format(
          round(threshold, 1),
          nsmall = 1
        ),
        "%. Seluruh persentase dihitung dari data lengkap."
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

  if (show_labels) {
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(
          label = .data$label_text
        ),
        size = 3
      )
  }

  attr(
    plot,
    "missing_summary"
  ) <- grouped_summary

  attr(
    plot,
    "sampling_info"
  ) <- list(
    used = FALSE,
    original_rows = nrow(data),
    plotted_rows = nrow(data),
    max_cells = as.integer(max_cells),
    method = "not applicable in grouped mode"
  )

  attr(
    plot,
    "mode"
  ) <- "grouped"

  plot
}
