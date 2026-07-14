test_that("vis_cor_adv menghasilkan ggplot dan tabel korelasi", {
  result <- vis_cor_adv(
    mtcars,
    method = "pearson"
  )

  correlation_table <- attr(
    result,
    "correlation_table"
  )

  diagnostics <- attr(
    result,
    "correlation_diagnostics"
  )

  expect_s3_class(result, "ggplot")
  expect_s3_class(
    correlation_table,
    "data.frame"
  )

  expect_named(
    correlation_table,
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
  )

  expect_equal(
    diagnostics$selected_method,
    "pearson"
  )
})


test_that("vis_cor_adv menghitung korelasi yang benar", {
  data_test <- data.frame(
    x = 1:10,
    y = 2 * (1:10),
    z = rev(1:10)
  )

  result <- vis_cor_adv(
    data_test,
    method = "pearson",
    cluster = FALSE
  )

  correlation_table <- attr(
    result,
    "correlation_table"
  )

  xy_result <- correlation_table[
    correlation_table$row_variable == "x" &
      correlation_table$column_variable == "y",
    ,
    drop = FALSE
  ]

  xz_result <- correlation_table[
    correlation_table$row_variable == "x" &
      correlation_table$column_variable == "z",
    ,
    drop = FALSE
  ]

  expect_equal(
    xy_result$correlation,
    1,
    tolerance = 1e-12
  )

  expect_equal(
    xz_result$correlation,
    -1,
    tolerance = 1e-12
  )

  expect_true(xy_result$strong)
  expect_true(xz_result$strong)
})


test_that("vis_cor_adv menampilkan adjusted p-value yang valid", {
  result <- vis_cor_adv(
    mtcars,
    method = "spearman"
  )

  correlation_table <- attr(
    result,
    "correlation_table"
  )

  adjusted_values <- correlation_table$adjusted_p_value[
    is.finite(
      correlation_table$adjusted_p_value
    )
  ]

  expect_true(
    all(adjusted_values >= 0)
  )

  expect_true(
    all(adjusted_values <= 1)
  )
})


test_that("vis_cor_adv menggunakan metode otomatis", {
  data_test <- data.frame(
    x = c(1:9, 100),
    y = c(2:10, 120),
    z = 10:1
  )

  result <- vis_cor_adv(
    data_test,
    method = "auto"
  )

  diagnostics <- attr(
    result,
    "correlation_diagnostics"
  )

  expect_equal(
    diagnostics$requested_method,
    "auto"
  )

  expect_equal(
    diagnostics$selected_method,
    "spearman"
  )

  expect_match(
    diagnostics$method_reason,
    "Spearman"
  )
})


test_that("vis_cor_adv memberi tanda signifikansi", {
  data_test <- data.frame(
    x = 1:20,
    y = 3 * (1:20),
    z = rep(
      c(-1, 1),
      10
    )
  )

  result <- vis_cor_adv(
    data_test,
    method = "pearson",
    show_significance = TRUE
  )

  expect_true(
    any(
      grepl(
        "\\*",
        result$data$label_text
      )
    )
  )
})


test_that("vis_cor_adv dapat menyembunyikan tanda signifikansi", {
  data_test <- data.frame(
    x = 1:20,
    y = 3 * (1:20),
    z = rep(
      c(-1, 1),
      10
    )
  )

  result <- vis_cor_adv(
    data_test,
    method = "pearson",
    show_significance = FALSE
  )

  expect_false(
    any(
      grepl(
        "\\*",
        result$data$label_text
      )
    )
  )
})


test_that("vis_cor_adv menggunakan pasangan data lengkap", {
  data_test <- data.frame(
    x = c(1, 2, NA, 4, Inf, 6),
    y = c(2, 4, 6, NA, 10, 12),
    z = c(1, 2, 3, 4, 5, 6)
  )

  result <- vis_cor_adv(
    data_test,
    method = "pearson"
  )

  correlation_table <- attr(
    result,
    "correlation_table"
  )

  xy_result <- correlation_table[
    correlation_table$row_variable == "x" &
      correlation_table$column_variable == "y",
    ,
    drop = FALSE
  ]

  expect_equal(
    xy_result$n_complete,
    3
  )

  expect_equal(
    xy_result$correlation,
    1,
    tolerance = 1e-12
  )
})


test_that("vis_cor_adv mengecualikan variabel konstan", {
  data_test <- data.frame(
    x = 1:10,
    y = 2 * (1:10),
    constant = rep(1, 10)
  )

  expect_warning(
    result <- vis_cor_adv(
      data_test,
      method = "pearson"
    ),
    "dikeluarkan"
  )

  diagnostics <- attr(
    result,
    "correlation_diagnostics"
  )

  expect_equal(
    diagnostics$excluded_variables,
    "constant"
  )

  expect_false(
    "constant" %in%
      result$data$row_variable
  )
})


test_that("vis_cor_adv menolak data dengan variabel numerik tidak cukup", {
  data_test <- data.frame(
    x = 1:10,
    group = letters[1:10]
  )

  expect_error(
    vis_cor_adv(data_test),
    "minimal dua variabel numerik"
  )
})


test_that("vis_cor_adv memvalidasi adjust_method", {
  expect_error(
    vis_cor_adv(
      mtcars,
      adjust_method = "metode_salah"
    ),
    "adjust_method"
  )
})


test_that("vis_cor_adv memvalidasi alpha", {
  expect_error(
    vis_cor_adv(
      mtcars,
      alpha = 0
    ),
    "lebih besar dari 0"
  )

  expect_error(
    vis_cor_adv(
      mtcars,
      alpha = 1
    ),
    "kurang dari 1"
  )
})


test_that("vis_cor_adv memvalidasi strong_threshold", {
  expect_error(
    vis_cor_adv(
      mtcars,
      strong_threshold = -0.1
    ),
    "antara 0 dan 1"
  )

  expect_error(
    vis_cor_adv(
      mtcars,
      strong_threshold = 1.1
    ),
    "antara 0 dan 1"
  )
})


test_that("vis_cor_adv memvalidasi argumen logis", {
  expect_error(
    vis_cor_adv(
      mtcars,
      cluster = "ya"
    ),
    "TRUE atau FALSE"
  )

  expect_error(
    vis_cor_adv(
      mtcars,
      show_values = 1
    ),
    "TRUE atau FALSE"
  )

  expect_error(
    vis_cor_adv(
      mtcars,
      show_significance = NA
    ),
    "TRUE atau FALSE"
  )
})


test_that("vis_cor_adv memvalidasi digits", {
  expect_error(
    vis_cor_adv(
      mtcars,
      digits = -1
    ),
    "bilangan bulat nonnegatif"
  )

  expect_error(
    vis_cor_adv(
      mtcars,
      digits = 1.5
    ),
    "bilangan bulat nonnegatif"
  )
})


test_that("vis_cor_adv menyimpan urutan variabel", {
  result <- vis_cor_adv(
    mtcars[
      ,
      c(
        "mpg",
        "disp",
        "hp",
        "wt"
      )
    ],
    cluster = TRUE
  )

  diagnostics <- attr(
    result,
    "correlation_diagnostics"
  )

  expect_setequal(
    diagnostics$variable_order,
    c(
      "mpg",
      "disp",
      "hp",
      "wt"
    )
  )

  expect_true(
    diagnostics$cluster
  )
})
