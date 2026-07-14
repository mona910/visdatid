test_that(".inspect_data menghasilkan struktur yang benar", {
  data_test <- data.frame(
    id = c(1, 2, 2, 4, 5),
    score = c(1, 2, 2, 4, 100),
    group = c("A", "B", "B", "", NA),
    constant = rep("yes", 5)
  )

  result <- .inspect_data(data_test)

  expect_s3_class(
    result,
    "visdatid_inspection"
  )

  expect_type(result, "list")

  expect_named(
    result,
    c("overview", "variables")
  )

  expect_s3_class(
    result$overview,
    "data.frame"
  )

  expect_s3_class(
    result$variables,
    "data.frame"
  )
})


test_that(".inspect_data menghitung ringkasan dataset dengan benar", {
  data_test <- data.frame(
    id = c(1, 2, 2, 4, 5),
    score = c(1, 2, 2, 4, 100),
    group = c("A", "B", "B", "", NA),
    constant = rep("yes", 5)
  )

  result <- .inspect_data(data_test)

  expect_equal(
    result$overview$n_rows,
    5
  )

  expect_equal(
    result$overview$n_columns,
    4
  )

  expect_equal(
    result$overview$n_duplicated_rows,
    1
  )

  expect_equal(
    result$overview$n_numeric_columns,
    2
  )

  expect_equal(
    result$overview$n_non_numeric_columns,
    2
  )
})


test_that(".inspect_data mendeteksi missing dan blank", {
  data_test <- data.frame(
    group = c("A", "B", "", NA),
    value = c(1, 2, 3, 4)
  )

  result <- .inspect_data(data_test)

  group_result <- result$variables[
    result$variables$variable == "group",
  ]

  expect_equal(
    group_result$n_missing,
    1
  )

  expect_equal(
    group_result$pct_missing,
    25
  )

  expect_equal(
    group_result$n_blank,
    1
  )

  expect_equal(
    group_result$pct_blank,
    25
  )
})


test_that(".inspect_data mendeteksi kolom konstan", {
  data_test <- data.frame(
    constant = rep("yes", 5),
    varied = c("A", "B", "C", "D", "E")
  )

  result <- .inspect_data(data_test)

  constant_result <- result$variables[
    result$variables$variable == "constant",
  ]

  varied_result <- result$variables[
    result$variables$variable == "varied",
  ]

  expect_true(constant_result$constant)
  expect_false(varied_result$constant)
})


test_that(".inspect_data menghitung diagnosis numerik", {
  data_test <- data.frame(
    score = c(1, 2, 2, 3, 4, 100)
  )

  result <- .inspect_data(data_test)

  score_result <- result$variables[
    result$variables$variable == "score",
  ]

  expect_true(score_result$numeric)
  expect_gt(score_result$skewness, 0)
  expect_equal(
    score_result$skewness_direction,
    "kanan"
  )

  expect_equal(
    score_result$skewness_category,
    "kemencengan kuat"
  )

  expect_equal(
    score_result$n_outlier,
    1
  )
})


test_that(".inspect_data menghitung nilai nol dan negatif", {
  data_test <- data.frame(
    value = c(-2, -1, 0, 1, 2)
  )

  result <- .inspect_data(data_test)

  value_result <- result$variables[
    result$variables$variable == "value",
  ]

  expect_equal(
    value_result$n_zero,
    1
  )

  expect_equal(
    value_result$n_negative,
    2
  )
})


test_that(".inspect_data menangani kolom seluruhnya missing", {
  data_test <- data.frame(
    empty = c(NA_real_, NA_real_, NA_real_),
    value = c(1, 2, 3)
  )

  result <- .inspect_data(data_test)

  empty_result <- result$variables[
    result$variables$variable == "empty",
  ]

  expect_true(empty_result$all_missing)
  expect_false(empty_result$constant)
  expect_equal(empty_result$n_missing, 3)
  expect_true(is.na(empty_result$skewness))
})


test_that(".inspect_data memvalidasi outlier_multiplier", {
  data_test <- data.frame(
    value = 1:5
  )

  expect_error(
    .inspect_data(
      data_test,
      outlier_multiplier = 0
    ),
    "angka positif"
  )

  expect_error(
    .inspect_data(
      data_test,
      outlier_multiplier = NA_real_
    ),
    "angka positif"
  )
})
