test_that(".eligible_numeric_data memilih kolom yang layak", {
  data_test <- data.frame(
    x = 1:5,
    y = c(2, 4, 6, 8, 10),
    constant = rep(1, 5),
    group = letters[1:5]
  )

  result <- .eligible_numeric_data(
    data_test
  )

  expect_equal(
    names(result$data),
    c("x", "y")
  )

  expect_equal(
    result$excluded,
    "constant"
  )
})


test_that(".eligible_numeric_data membutuhkan dua variabel numerik", {
  data_test <- data.frame(
    x = 1:5,
    group = letters[1:5]
  )

  expect_error(
    .eligible_numeric_data(data_test),
    "minimal dua variabel numerik"
  )
})


test_that(".choose_correlation_method memilih Pearson otomatis", {
  data_test <- data.frame(
    x = -5:5,
    y = 2 * (-5:5),
    z = 3 * (-5:5) + 1
  )

  result <- .choose_correlation_method(
    data_test,
    method = "auto"
  )

  expect_equal(
    result$method,
    "pearson"
  )

  expect_match(
    result$reason,
    "Pearson"
  )
})


test_that(".choose_correlation_method memilih Spearman otomatis", {
  data_test <- data.frame(
    x = c(1:9, 100),
    y = c(2:10, 120),
    z = 10:1
  )

  result <- .choose_correlation_method(
    data_test,
    method = "auto"
  )

  expect_equal(
    result$method,
    "spearman"
  )

  expect_match(
    result$reason,
    "Spearman"
  )
})


test_that(".choose_correlation_method mempertahankan pilihan pengguna", {
  data_test <- data.frame(
    x = 1:5,
    y = 2:6
  )

  result <- .choose_correlation_method(
    data_test,
    method = "kendall"
  )

  expect_equal(
    result$method,
    "kendall"
  )

  expect_match(
    result$reason,
    "dipilih oleh pengguna"
  )
})


test_that(".cor_test_pair menghitung korelasi sempurna", {
  x <- 1:10
  y <- 2 * x

  result <- .cor_test_pair(
    x,
    y,
    method = "pearson"
  )

  expect_equal(
    result$correlation,
    1,
    tolerance = 1e-12
  )

  expect_lt(
    result$p_value,
    0.001
  )

  expect_equal(
    result$n_complete,
    10
  )
})


test_that(".cor_test_pair menggunakan pasangan lengkap", {
  x <- c(1, 2, NA, 4, Inf, 6)
  y <- c(2, 4, 6, NA, 10, 12)

  result <- .cor_test_pair(
    x,
    y,
    method = "pearson"
  )

  expect_equal(
    result$n_complete,
    3
  )

  expect_equal(
    result$correlation,
    1,
    tolerance = 1e-12
  )
})


test_that(".build_correlation_matrices menghasilkan matriks simetris", {
  data_test <- data.frame(
    x = 1:10,
    y = 2 * (1:10),
    z = rev(1:10)
  )

  result <- .build_correlation_matrices(
    data_test,
    method = "pearson",
    adjust_method = "BH"
  )

  expect_equal(
    result$correlation,
    t(result$correlation)
  )

  expect_equal(
    unname(
      diag(result$correlation)
    ),
    rep(1, 3)
  )

  expect_equal(
    result$correlation["x", "y"],
    1,
    tolerance = 1e-12
  )

  expect_equal(
    result$correlation["x", "z"],
    -1,
    tolerance = 1e-12
  )
})


test_that(".build_correlation_matrices menyesuaikan p-value", {
  data_test <- data.frame(
    x = 1:12,
    y = 2 * (1:12),
    z = rev(1:12)
  )

  result <- .build_correlation_matrices(
    data_test,
    method = "pearson",
    adjust_method = "BH"
  )

  adjusted_values <- result$adjusted_p[
    upper.tri(result$adjusted_p)
  ]

  adjusted_values <- adjusted_values[
    is.finite(adjusted_values)
  ]

  expect_true(
    all(adjusted_values >= 0)
  )

  expect_true(
    all(adjusted_values <= 1)
  )
})


test_that(".correlation_variable_order mempertahankan urutan tanpa klaster", {
  correlation_matrix <- stats::cor(
    mtcars[, c("mpg", "disp", "hp")]
  )

  result <- .correlation_variable_order(
    correlation_matrix,
    cluster = FALSE
  )

  expect_equal(
    result,
    c("mpg", "disp", "hp")
  )
})


test_that(".correlation_variable_order menghasilkan seluruh nama variabel", {
  correlation_matrix <- stats::cor(
    mtcars[, c("mpg", "disp", "hp", "wt")]
  )

  result <- .correlation_variable_order(
    correlation_matrix,
    cluster = TRUE
  )

  expect_setequal(
    result,
    c("mpg", "disp", "hp", "wt")
  )

  expect_equal(
    length(result),
    4
  )
})
