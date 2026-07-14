test_that("quality_report menghasilkan struktur laporan yang benar", {
  data_test <- data.frame(
    id = c(1, 2, 2, 4, 5),
    score = c(1, 2, 2, 4, 100),
    group = c("A", "B", "B", "", NA),
    constant = rep("yes", 5)
  )

  result <- quality_report(
    data_test,
    id_col = "id"
  )

  expect_s3_class(
    result,
    "visdatid_quality_report"
  )

  expect_s3_class(
    result,
    "data.frame"
  )

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

  expect_gt(nrow(result), 0)
})


test_that("quality_report mendeteksi missing dan string kosong", {
  data_test <- data.frame(
    group = c("A", "B", "", NA),
    value = c(1, 2, 3, 4)
  )

  result <- quality_report(data_test)

  expect_true(
    any(result$issue == "missing value")
  )

  expect_true(
    any(result$issue == "string kosong")
  )

  group_issues <- result[
    result$variable == "group",
    ,
    drop = FALSE
  ]

  expect_true(
    all(group_issues$scope == "variable")
  )
})


test_that("quality_report mendeteksi variabel konstan", {
  data_test <- data.frame(
    constant = rep("yes", 5),
    varied = letters[1:5]
  )

  result <- quality_report(data_test)

  constant_issue <- result[
    result$issue == "variabel konstan",
    ,
    drop = FALSE
  ]

  expect_equal(
    constant_issue$variable,
    "constant"
  )

  expect_equal(
    constant_issue$severity,
    "tinggi"
  )
})


test_that("quality_report mendeteksi baris dan ID duplikat", {
  data_test <- data.frame(
    id = c(1, 1, 2),
    score = c(10, 10, 20),
    group = c("A", "A", "B")
  )

  result <- quality_report(
    data_test,
    id_col = "id"
  )

  expect_true(
    any(result$issue == "baris duplikat")
  )

  expect_true(
    any(result$issue == "ID duplikat")
  )

  id_issue <- result[
    result$issue == "ID duplikat",
    ,
    drop = FALSE
  ]

  expect_equal(
    id_issue$variable,
    "id"
  )

  expect_equal(
    id_issue$severity,
    "tinggi"
  )
})


test_that("quality_report mendeteksi skewness dan pencilan", {
  data_test <- data.frame(
    score = c(1, 2, 2, 3, 3, 4, 100)
  )

  result <- quality_report(data_test)

  expect_true(
    any(result$issue == "kemencengan distribusi")
  )

  expect_true(
    any(result$issue == "pencilan IQR")
  )

  skewness_issue <- result[
    result$issue == "kemencengan distribusi",
    ,
    drop = FALSE
  ]

  expect_match(
    skewness_issue$value,
    "kanan"
  )

  expect_match(
    skewness_issue$recommendation,
    "pencilan"
  )
})


test_that("quality_report mendeteksi nilai tak hingga", {
  data_test <- data.frame(
    value = c(1, 2, Inf, -Inf, 5)
  )

  result <- quality_report(data_test)

  expect_true(
    any(result$issue == "nilai tak hingga")
  )

  infinite_issue <- result[
    result$issue == "nilai tak hingga",
    ,
    drop = FALSE
  ]

  expect_equal(
    infinite_issue$severity,
    "tinggi"
  )
})


test_that("quality_report mendeteksi kolom seluruhnya missing", {
  data_test <- data.frame(
    empty = c(NA_real_, NA_real_, NA_real_),
    value = c(-1, 0, 1)
  )

  result <- quality_report(data_test)

  empty_issue <- result[
    result$issue == "seluruh nilai missing",
    ,
    drop = FALSE
  ]

  expect_equal(
    empty_issue$variable,
    "empty"
  )

  expect_equal(
    empty_issue$severity,
    "tinggi"
  )

  expect_false(
    any(
      result$variable == "empty" &
        result$issue == "missing value"
    )
  )
})


test_that("quality_report mengembalikan laporan kosong untuk data bersih", {
  data_test <- data.frame(
    id = 1:5,
    score = c(-2, -1, 0, 1, 2),
    group = letters[1:5]
  )

  result <- quality_report(data_test)

  expect_s3_class(
    result,
    "visdatid_quality_report"
  )

  expect_equal(
    nrow(result),
    0
  )

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


test_that("quality_report menolak id_col yang tidak valid", {
  expect_error(
    quality_report(
      mtcars,
      id_col = 1
    ),
    "nama variabel"
  )

  expect_error(
    quality_report(
      mtcars,
      id_col = "kolom_tidak_ada"
    ),
    "tidak ditemukan"
  )
})


test_that("quality_report menolak input bukan data frame", {
  expect_error(
    quality_report(1:10),
    "data frame"
  )
})


test_that("quality_report memvalidasi outlier_multiplier", {
  expect_error(
    quality_report(
      mtcars,
      outlier_multiplier = 0
    ),
    "angka positif"
  )
})


test_that("quality_report hanya menggunakan kategori severity yang sah", {
  data_test <- data.frame(
    score = c(1, 2, 2, 3, 3, 4, 100),
    group = c("A", "B", "", "C", "D", "E", NA)
  )

  result <- quality_report(data_test)

  expect_true(
    all(
      result$severity %in%
        c("rendah", "sedang", "tinggi")
    )
  )

  expect_false(
    any(is.na(result$recommendation))
  )

  expect_true(
    all(nzchar(result$recommendation))
  )
})

test_that("quality_report tidak mendiagnosis distribusi kolom ID", {
  data_test <- data.frame(
    id = c(1, 2, 3, 4, 100),
    score = c(-2, -1, 0, 1, 2)
  )

  result <- quality_report(
    data_test,
    id_col = "id"
  )

  id_distribution_issues <- result[
    result$variable == "id" &
      result$issue %in% c(
        "kemencengan distribusi",
        "pencilan IQR"
      ),
    ,
    drop = FALSE
  ]

  expect_equal(
    nrow(id_distribution_issues),
    0
  )
})
