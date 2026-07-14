#' Generate an actionable data-quality report
#'
#' @description
#' `quality_report()` inspects a data frame and returns a structured table of
#' detected data-quality problems. The report covers completeness,
#' consistency, uniqueness, validity, variability, and numerical
#' distribution.
#'
#' @param data A data frame.
#' @param id_col An optional character value containing the name of a variable
#'   that should uniquely identify rows. Use `NULL` when no identifier should
#'   be examined.
#' @param outlier_multiplier A positive number used as the multiplier in the
#'   interquartile-range outlier rule. The default is `1.5`.
#'
#' @return
#' A tibble with the following columns:
#'
#' * `scope`: whether the problem applies to the dataset or one variable.
#' * `variable`: the affected variable, or `NA` for dataset-level problems.
#' * `dimension`: the dimension of data quality.
#' * `issue`: the detected problem.
#' * `value`: the numerical or descriptive result.
#' * `severity`: `"rendah"`, `"sedang"`, or `"tinggi"`.
#' * `recommendation`: a suggested follow-up action.
#'
#' The returned object also inherits from class
#' `visdatid_quality_report`.
#'
#' @details
#' The default practical thresholds used in this function are:
#'
#' * missing or blank values: moderate at 5 percent and high at 20 percent;
#' * outliers: moderate at 1 percent and high at 5 percent;
#' * absolute skewness: moderate at 0.5 and high at 1.
#'
#' These thresholds are intended for preliminary diagnosis and should not
#' replace substantive knowledge, study design considerations, or formal
#' statistical assessment.
#'
#' A duplicated row means that an entire record repeats an earlier record.
#' A duplicated identifier means that a non-missing value in `id_col` occurs
#' more than once.
#'
#' @examples
#' quality_report(airquality)
#'
#' example_data <- data.frame(
#'   id = c(1, 2, 2, 4),
#'   score = c(10, 11, 12, 100),
#'   group = c("A", "B", "", NA)
#' )
#'
#' quality_report(
#'   example_data,
#'   id_col = "id"
#' )
#'
#' @seealso [vis_distribution()]
#'
#' @export
quality_report <- function(
    data,
    id_col = NULL,
    outlier_multiplier = 1.5
) {
  .validate_data_frame(data)

  if (!is.null(id_col)) {
    if (
      !is.character(id_col) ||
      length(id_col) != 1L ||
      is.na(id_col) ||
      !nzchar(id_col)
    ) {
      stop(
        paste(
          "`id_col` harus berupa satu nama variabel",
          "dalam bentuk karakter atau NULL."
        ),
        call. = FALSE
      )
    }

    if (!id_col %in% names(data)) {
      stop(
        paste0(
          "Variabel ID `",
          id_col,
          "` tidak ditemukan dalam `data`."
        ),
        call. = FALSE
      )
    }
  }

  inspection <- .inspect_data(
    data,
    outlier_multiplier = outlier_multiplier
  )

  overview <- inspection$overview
  variable_summary <- inspection$variables

  issues <- list()

  add_issue <- function(issue_row) {
    issues[[length(issues) + 1L]] <<- issue_row
  }

  # Dataset-level duplicated rows ------------------------------------------

  if (overview$n_duplicated_rows > 0L) {
    duplicate_severity <- .severity_from_percentage(
      overview$pct_duplicated_rows,
      moderate_threshold = 1,
      high_threshold = 5
    )

    add_issue(
      .new_quality_issue(
        scope = "dataset",
        variable = NA_character_,
        dimension = "keunikan",
        issue = "baris duplikat",
        value = .format_count_percentage(
          count = overview$n_duplicated_rows,
          percentage = overview$pct_duplicated_rows,
          total = overview$n_rows
        ),
        severity = duplicate_severity,
        recommendation = paste(
          "Periksa apakah baris yang identik merupakan pencatatan berulang",
          "atau observasi yang memang sah sebelum menghapusnya."
        )
      )
    )
  }

  # Optional identifier diagnosis -----------------------------------------

  if (!is.null(id_col)) {
    identifier <- data[[id_col]]

    valid_identifier <- !is.na(identifier)

    duplicated_identifier <- (
      duplicated(identifier) &
        valid_identifier
    )

    number_duplicated_identifier <- sum(
      duplicated_identifier
    )

    number_valid_identifier <- sum(
      valid_identifier
    )

    if (number_duplicated_identifier > 0L) {
      percent_duplicated_identifier <- (
        100 *
          number_duplicated_identifier /
          number_valid_identifier
      )

      add_issue(
        .new_quality_issue(
          scope = "variable",
          variable = id_col,
          dimension = "keunikan",
          issue = "ID duplikat",
          value = .format_count_percentage(
            count = number_duplicated_identifier,
            percentage = percent_duplicated_identifier,
            total = number_valid_identifier
          ),
          severity = "tinggi",
          recommendation = paste(
            "Periksa apakah setiap nilai ID seharusnya unik dan telusuri",
            "observasi yang menggunakan ID yang sama."
          )
        )
      )
    }
  }

  # Variable-level diagnosis ----------------------------------------------

  for (i in seq_len(nrow(variable_summary))) {
    variable_row <- variable_summary[
      i,
      ,
      drop = FALSE
    ]

    variable_name <- variable_row$variable
    total_rows <- variable_row$n

    # Entirely missing column

    if (isTRUE(variable_row$all_missing)) {
      add_issue(
        .new_quality_issue(
          scope = "variable",
          variable = variable_name,
          dimension = "kelengkapan",
          issue = "seluruh nilai missing",
          value = .format_count_percentage(
            count = variable_row$n_missing,
            percentage = variable_row$pct_missing,
            total = total_rows
          ),
          severity = "tinggi",
          recommendation = paste(
            "Variabel tidak memiliki informasi teramati.",
            "Telusuri sumber data atau pertimbangkan mengeluarkannya."
          )
        )
      )
    } else if (variable_row$n_missing > 0L) {
      missing_severity <- .severity_from_percentage(
        variable_row$pct_missing,
        moderate_threshold = 5,
        high_threshold = 20
      )

      missing_recommendation <- if (
        missing_severity == "tinggi"
      ) {
        paste(
          "Evaluasi mekanisme missing serta kelayakan imputasi,",
          "pengumpulan ulang, atau pengeluaran variabel."
        )
      } else if (
        missing_severity == "sedang"
      ) {
        paste(
          "Telusuri pola missing dan pilih metode penanganan",
          "yang sesuai dengan tujuan analisis."
        )
      } else {
        paste(
          "Periksa sumber missing dan dokumentasikan",
          "penanganannya sebelum analisis."
        )
      }

      add_issue(
        .new_quality_issue(
          scope = "variable",
          variable = variable_name,
          dimension = "kelengkapan",
          issue = "missing value",
          value = .format_count_percentage(
            count = variable_row$n_missing,
            percentage = variable_row$pct_missing,
            total = total_rows
          ),
          severity = missing_severity,
          recommendation = missing_recommendation
        )
      )
    }

    # Blank character values

    if (
      !is.na(variable_row$n_blank) &&
      variable_row$n_blank > 0L
    ) {
      blank_severity <- .severity_from_percentage(
        variable_row$pct_blank,
        moderate_threshold = 5,
        high_threshold = 20
      )

      add_issue(
        .new_quality_issue(
          scope = "variable",
          variable = variable_name,
          dimension = "konsistensi",
          issue = "string kosong",
          value = .format_count_percentage(
            count = variable_row$n_blank,
            percentage = variable_row$pct_blank,
            total = total_rows
          ),
          severity = blank_severity,
          recommendation = paste(
            "Periksa makna string kosong dan standardisasikan menjadi",
            "kategori yang benar atau missing value bila sesuai."
          )
        )
      )
    }

    # Constant column

    if (isTRUE(variable_row$constant)) {
      add_issue(
        .new_quality_issue(
          scope = "variable",
          variable = variable_name,
          dimension = "variabilitas",
          issue = "variabel konstan",
          value = paste0(
            variable_row$n_unique,
            " nilai unik"
          ),
          severity = "tinggi",
          recommendation = paste(
            "Variabel tidak memberikan variasi.",
            "Pertimbangkan mengeluarkannya apabila bukan identifier",
            "atau variabel administratif yang diperlukan."
          )
        )
      )
    }

    # Infinite numerical values

    if (
      isTRUE(variable_row$numeric) &&
      !is.na(variable_row$n_infinite) &&
      variable_row$n_infinite > 0L
    ) {
      infinite_percentage <- (
        100 *
          variable_row$n_infinite /
          total_rows
      )

      add_issue(
        .new_quality_issue(
          scope = "variable",
          variable = variable_name,
          dimension = "validitas",
          issue = "nilai tak hingga",
          value = .format_count_percentage(
            count = variable_row$n_infinite,
            percentage = infinite_percentage,
            total = total_rows
          ),
          severity = "tinggi",
          recommendation = paste(
            "Telusuri operasi yang menghasilkan Inf atau -Inf dan",
            "perbaiki sumber perhitungan sebelum analisis."
          )
        )
      )
    }

    # Numerical skewness

    if (
      isTRUE(variable_row$numeric) &&
      !identical(variable_name, id_col) &&
      is.finite(variable_row$skewness) &&
      abs(variable_row$skewness) >= 0.5
    ) {
      skewness_severity <- if (
        abs(variable_row$skewness) >= 1
      ) {
        "tinggi"
      } else {
        "sedang"
      }

      add_issue(
        .new_quality_issue(
          scope = "variable",
          variable = variable_name,
          dimension = "distribusi",
          issue = "kemencengan distribusi",
          value = paste0(
            format(
              round(variable_row$skewness, 2),
              nsmall = 2
            ),
            " (",
            variable_row$skewness_direction,
            "; ",
            variable_row$skewness_category,
            ")"
          ),
          severity = skewness_severity,
          recommendation = .suggest_transformation(
            data[[variable_name]],
            variable_row$skewness
          )
        )
      )
    }

    # Numerical outliers

    if (
      isTRUE(variable_row$numeric) &&
      !identical(variable_name, id_col) &&
      !is.na(variable_row$n_outlier) &&
      variable_row$n_outlier > 0L
    ) {
      outlier_severity <- .severity_from_percentage(
        variable_row$pct_outlier,
        moderate_threshold = 1,
        high_threshold = 5
      )

      finite_total <- sum(
        is.finite(data[[variable_name]])
      )

      add_issue(
        .new_quality_issue(
          scope = "variable",
          variable = variable_name,
          dimension = "distribusi",
          issue = "pencilan IQR",
          value = .format_count_percentage(
            count = variable_row$n_outlier,
            percentage = variable_row$pct_outlier,
            total = finite_total
          ),
          severity = outlier_severity,
          recommendation = paste(
            "Verifikasi pencilan terhadap sumber data dan konteks substantif.",
            "Jangan menghapus pencilan secara otomatis."
          )
        )
      )
    }
  }

  # Return an empty structured report when no issue is detected ------------

  if (length(issues) == 0L) {
    report <- .empty_quality_report()

    class(report) <- c(
      "visdatid_quality_report",
      class(report)
    )

    return(report)
  }

  report <- dplyr::bind_rows(issues)

  severity_rank <- match(
    report$severity,
    c("tinggi", "sedang", "rendah")
  )

  scope_rank <- match(
    report$scope,
    c("dataset", "variable")
  )

  report <- report[
    order(
      severity_rank,
      scope_rank,
      report$variable,
      report$dimension,
      report$issue,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(report) <- NULL

  class(report) <- c(
    "visdatid_quality_report",
    class(report)
  )

  report
}
