# Internal utility functions -----------------------------------------------

.validate_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    stop(
      "`data` harus berupa data frame.",
      call. = FALSE
    )
  }

  if (ncol(data) == 0L) {
    stop(
      "`data` harus memiliki minimal satu variabel.",
      call. = FALSE
    )
  }

  if (nrow(data) == 0L) {
    stop(
      "`data` harus memiliki minimal satu baris.",
      call. = FALSE
    )
  }

  variable_names <- names(data)

  if (
    is.null(variable_names) ||
    anyNA(variable_names) ||
    any(trimws(variable_names) == "")
  ) {
    stop(
      "Seluruh variabel dalam `data` harus memiliki nama.",
      call. = FALSE
    )
  }

  if (anyDuplicated(variable_names) > 0L) {
    stop(
      "Nama variabel dalam `data` harus unik.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.numeric_columns <- function(data) {
  .validate_data_frame(data)

  is_numeric <- vapply(
    data,
    is.numeric,
    logical(1)
  )

  data[is_numeric]
}


.calc_skewness <- function(x, na_rm = TRUE) {
  if (!is.numeric(x)) {
    stop(
      "`x` harus berupa vektor numerik.",
      call. = FALSE
    )
  }

  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop(
      "`na_rm` harus berupa satu nilai TRUE atau FALSE.",
      call. = FALSE
    )
  }

  if (na_rm) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(NA_real_)
  }

  x <- x[is.finite(x)]
  n <- length(x)

  if (n < 3L) {
    return(NA_real_)
  }

  standard_deviation <- stats::sd(x)

  if (
    !is.finite(standard_deviation) ||
    standard_deviation == 0
  ) {
    return(NA_real_)
  }

  standardized <- (
    x - mean(x)
  ) / standard_deviation

  n / ((n - 1) * (n - 2)) *
    sum(standardized^3)
}


.calc_kurtosis <- function(x, na_rm = TRUE) {
  if (!is.numeric(x)) {
    stop(
      "`x` harus berupa vektor numerik.",
      call. = FALSE
    )
  }

  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop(
      "`na_rm` harus berupa satu nilai TRUE atau FALSE.",
      call. = FALSE
    )
  }

  if (na_rm) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(NA_real_)
  }

  x <- x[is.finite(x)]
  n <- length(x)

  if (n < 4L) {
    return(NA_real_)
  }

  standard_deviation <- stats::sd(x)

  if (
    !is.finite(standard_deviation) ||
    standard_deviation == 0
  ) {
    return(NA_real_)
  }

  standardized <- (
    x - mean(x)
  ) / standard_deviation

  first_term <- (
    n * (n + 1)
  ) / (
    (n - 1) * (n - 2) * (n - 3)
  ) * sum(standardized^4)

  second_term <- (
    3 * (n - 1)^2
  ) / (
    (n - 2) * (n - 3)
  )

  first_term - second_term
}


.detect_outliers_iqr <- function(x, multiplier = 1.5) {
  if (!is.numeric(x)) {
    stop(
      "`x` harus berupa vektor numerik.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(multiplier) ||
    length(multiplier) != 1L ||
    is.na(multiplier) ||
    !is.finite(multiplier) ||
    multiplier <= 0
  ) {
    stop(
      "`multiplier` harus berupa satu angka positif.",
      call. = FALSE
    )
  }

  result <- rep(FALSE, length(x))

  # NA, NaN, Inf, dan -Inf tidak dihitung sebagai pencilan IQR.
  finite_values <- is.finite(x)
  result[!finite_values] <- NA

  if (sum(finite_values) < 4L) {
    return(result)
  }

  quartiles <- stats::quantile(
    x[finite_values],
    probs = c(0.25, 0.75),
    names = FALSE,
    na.rm = TRUE,
    type = 7
  )

  iqr_value <- quartiles[2] - quartiles[1]

  # Hanya berhenti apabila IQR tidak dapat dihitung.
  # IQR = 0 tetap diperiksa.
  if (!is.finite(iqr_value)) {
    return(result)
  }

  lower_limit <- quartiles[1] - multiplier * iqr_value
  upper_limit <- quartiles[2] + multiplier * iqr_value

  result[finite_values] <- (
    x[finite_values] < lower_limit |
      x[finite_values] > upper_limit
  )

  result
}


.classify_skewness <- function(skewness) {
  if (
    length(skewness) != 1L ||
    is.na(skewness) ||
    !is.finite(skewness)
  ) {
    return("tidak dapat dihitung")
  }

  absolute_skewness <- abs(skewness)

  if (absolute_skewness < 0.5) {
    return("relatif simetris")
  }

  if (absolute_skewness < 1) {
    return("kemencengan sedang")
  }

  "kemencengan kuat"
}


.skewness_direction <- function(skewness) {
  if (
    length(skewness) != 1L ||
    is.na(skewness) ||
    !is.finite(skewness)
  ) {
    return("tidak dapat ditentukan")
  }

  if (skewness > 0) {
    return("kanan")
  }

  if (skewness < 0) {
    return("kiri")
  }

  "simetris"
}

.suggest_transformation <- function(x, skewness) {
  if (!is.numeric(x)) {
    stop(
      "`x` harus berupa vektor numerik.",
      call. = FALSE
    )
  }

  finite_values <- x[is.finite(x)]

  if (
    length(finite_values) < 3L ||
    length(skewness) != 1L ||
    is.na(skewness) ||
    !is.finite(skewness)
  ) {
    return(
      paste(
        "Rekomendasi transformasi belum dapat diberikan",
        "karena data valid atau variasinya tidak mencukupi."
      )
    )
  }

  if (abs(skewness) < 0.5) {
    return(
      paste(
        "Distribusi relatif simetris;",
        "transformasi tidak menjadi prioritas."
      )
    )
  }

  if (skewness > 0 && all(finite_values >= 0)) {
    return(
      paste(
        "Distribusi menceng ke kanan.",
        "Periksa pencilan dan pertimbangkan transformasi",
        "log1p atau Yeo-Johnson apabila sesuai dengan tujuan analisis."
      )
    )
  }

  if (skewness > 0) {
    return(
      paste(
        "Distribusi menceng ke kanan dan memuat nilai negatif.",
        "Periksa pencilan dan pertimbangkan transformasi",
        "Yeo-Johnson apabila diperlukan."
      )
    )
  }

  paste(
    "Distribusi menceng ke kiri.",
    "Periksa batas atas dan pencilan, lalu pertimbangkan",
    "refleksi data atau transformasi Yeo-Johnson apabila diperlukan."
  )
}

.severity_from_percentage <- function(
    value,
    moderate_threshold,
    high_threshold
) {
  if (
    length(value) != 1L ||
    is.na(value) ||
    !is.finite(value)
  ) {
    return(NA_character_)
  }

  if (value >= high_threshold) {
    return("tinggi")
  }

  if (value >= moderate_threshold) {
    return("sedang")
  }

  "rendah"
}


.format_count_percentage <- function(
    count,
    percentage,
    total
) {
  paste0(
    count,
    " dari ",
    total,
    " (",
    format(
      round(percentage, 1),
      nsmall = 1
    ),
    "%)"
  )
}


.new_quality_issue <- function(
    scope,
    variable,
    dimension,
    issue,
    value,
    severity,
    recommendation
) {
  tibble::tibble(
    scope = as.character(scope),
    variable = as.character(variable),
    dimension = as.character(dimension),
    issue = as.character(issue),
    value = as.character(value),
    severity = as.character(severity),
    recommendation = as.character(recommendation)
  )
}


.empty_quality_report <- function() {
  tibble::tibble(
    scope = character(),
    variable = character(),
    dimension = character(),
    issue = character(),
    value = character(),
    severity = character(),
    recommendation = character()
  )
}
