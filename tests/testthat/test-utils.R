test_that(".validate_data_frame menerima data frame yang valid", {
  data_test <- data.frame(
    x = 1:3,
    y = c("A", "B", "C")
  )

  expect_invisible(
    .validate_data_frame(data_test)
  )
})


test_that(".validate_data_frame menolak input yang tidak valid", {
  expect_error(
    .validate_data_frame(1:5),
    "data frame"
  )

  expect_error(
    .validate_data_frame(data.frame()),
    "minimal satu variabel"
  )
})


test_that(".numeric_columns hanya mengambil variabel numerik", {
  data_test <- data.frame(
    usia = c(19, 20, 21),
    nilai = c(80, 85, 90),
    kelompok = c("A", "B", "A")
  )

  result <- .numeric_columns(data_test)

  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c("usia", "nilai")
  )
})


test_that(".calc_skewness menghasilkan nol untuk data simetris", {
  x <- c(-2, -1, 0, 1, 2)

  expect_equal(
    .calc_skewness(x),
    0,
    tolerance = 1e-12
  )
})


test_that(".calc_skewness mengenali kemencengan kanan", {
  x <- c(1, 1, 2, 2, 3, 12)

  expect_gt(
    .calc_skewness(x),
    0
  )
})


test_that(".calc_skewness menangani data konstan", {
  x <- rep(5, 10)

  expect_true(
    is.na(.calc_skewness(x))
  )
})


test_that(".calc_kurtosis menghasilkan angka untuk data valid", {
  x <- c(-3, -2, -1, 0, 1, 2, 3)

  result <- .calc_kurtosis(x)

  expect_type(result, "double")
  expect_true(is.finite(result))
})


test_that(".detect_outliers_iqr mengenali pencilan", {
  x <- c(1, 2, 2, 3, 3, 4, 100, NA)

  result <- .detect_outliers_iqr(x)

  expect_true(result[7])
  expect_true(is.na(result[8]))
  expect_equal(
    sum(result, na.rm = TRUE),
    1
  )
})


test_that(".classify_skewness memberikan kategori yang benar", {
  expect_equal(
    .classify_skewness(0.2),
    "relatif simetris"
  )

  expect_equal(
    .classify_skewness(0.7),
    "kemencengan sedang"
  )

  expect_equal(
    .classify_skewness(1.5),
    "kemencengan kuat"
  )
})


test_that(".skewness_direction menentukan arah yang benar", {
  expect_equal(
    .skewness_direction(1),
    "kanan"
  )

  expect_equal(
    .skewness_direction(-1),
    "kiri"
  )

  expect_equal(
    .skewness_direction(0),
    "simetris"
  )
})

test_that(".suggest_transformation mengenali distribusi simetris", {
  x <- c(-2, -1, 0, 1, 2)

  result <- .suggest_transformation(
    x,
    skewness = 0
  )

  expect_match(
    result,
    "relatif simetris"
  )
})


test_that(".suggest_transformation memberi saran untuk kemencengan kanan", {
  x <- c(0, 1, 2, 3, 20)

  result <- .suggest_transformation(
    x,
    skewness = 1.5
  )

  expect_match(
    result,
    "log1p"
  )

  expect_match(
    result,
    "pencilan"
  )
})


test_that(".suggest_transformation tidak menyarankan log untuk nilai negatif", {
  x <- c(-10, -2, -1, 0, 20)

  result <- .suggest_transformation(
    x,
    skewness = 1.2
  )

  expect_match(
    result,
    "nilai negatif"
  )

  expect_false(
    grepl("log1p", result)
  )
})


test_that(".suggest_transformation memberi saran untuk kemencengan kiri", {
  x <- c(-20, -3, -2, -1, 0)

  result <- .suggest_transformation(
    x,
    skewness = -1.4
  )

  expect_match(
    result,
    "menceng ke kiri"
  )

  expect_match(
    result,
    "refleksi"
  )
})

test_that(".severity_from_percentage mengklasifikasikan persentase", {
  expect_equal(
    .severity_from_percentage(
      2,
      moderate_threshold = 5,
      high_threshold = 20
    ),
    "rendah"
  )

  expect_equal(
    .severity_from_percentage(
      10,
      moderate_threshold = 5,
      high_threshold = 20
    ),
    "sedang"
  )

  expect_equal(
    .severity_from_percentage(
      25,
      moderate_threshold = 5,
      high_threshold = 20
    ),
    "tinggi"
  )
})


test_that(".format_count_percentage menghasilkan format yang benar", {
  result <- .format_count_percentage(
    count = 2,
    percentage = 20,
    total = 10
  )

  expect_equal(
    result,
    "2 dari 10 (20.0%)"
  )
})


test_that(".new_quality_issue menghasilkan satu baris laporan", {
  result <- .new_quality_issue(
    scope = "variable",
    variable = "usia",
    dimension = "kelengkapan",
    issue = "missing value",
    value = "2 dari 10 (20.0%)",
    severity = "tinggi",
    recommendation = "Periksa missing value."
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  expect_named(
    result,
    c(
      "scope",
      "variable",
      "dimension",
      "issue",
      "value",
      "severity",
      "recommendation"
    )
  )
})


test_that(".empty_quality_report memiliki struktur yang benar", {
  result <- .empty_quality_report()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)

  expect_named(
    result,
    c(
      "scope",
      "variable",
      "dimension",
      "issue",
      "value",
      "severity",
      "recommendation"
    )
  )
})

test_that(".validate_data_frame menolak data tanpa baris", {
  data_test <- data.frame(
    x = numeric()
  )

  expect_error(
    .validate_data_frame(data_test),
    "minimal satu baris"
  )
})


test_that(".validate_data_frame menolak nama variabel kosong", {
  data_test <- data.frame(
    x = 1:3
  )

  names(data_test) <- ""

  expect_error(
    .validate_data_frame(data_test),
    "harus memiliki nama"
  )
})


test_that(".validate_data_frame menolak nama variabel duplikat", {
  data_test <- data.frame(
    x = 1:3,
    y = 4:6
  )

  names(data_test) <- c("nilai", "nilai")

  expect_error(
    .validate_data_frame(data_test),
    "harus unik"
  )
})


test_that("nilai tak hingga tidak dihitung sebagai pencilan IQR", {
  x <- c(1, 2, 3, 4, Inf, -Inf)

  result <- .detect_outliers_iqr(x)

  expect_false(any(result[1:4]))
  expect_true(is.na(result[5]))
  expect_true(is.na(result[6]))

  expect_equal(
    sum(result, na.rm = TRUE),
    0
  )
})


test_that("pencilan tetap terdeteksi ketika IQR sama dengan nol", {
  x <- c(1, 1, 1, 1, 100)

  result <- .detect_outliers_iqr(x)

  expect_false(any(result[1:4]))
  expect_true(result[5])

  expect_equal(
    sum(result, na.rm = TRUE),
    1
  )
})
