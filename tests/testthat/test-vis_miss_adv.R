test_that("vis_miss_adv menghasilkan plot dan ringkasan", {
  data_test <- data.frame(
    x = c(1, NA, 3, NA),
    y = 1:4
  )

  result <- vis_miss_adv(
    data_test,
    threshold = 40
  )

  summary_result <- attr(
    result,
    "missing_summary"
  )

  expect_s3_class(result, "ggplot")
  expect_equal(attr(result, "mode"), "cell")
  expect_s3_class(summary_result, "data.frame")

  x_summary <- summary_result[
    summary_result$variable == "x",
    ,
    drop = FALSE
  ]

  expect_equal(x_summary$n_missing, 2)
  expect_equal(x_summary$pct_missing, 50)
  expect_true(x_summary$flagged)
})


test_that("vis_miss_adv menandai batas threshold secara inklusif", {
  data_test <- data.frame(
    x = c(1, NA, 3, 4),
    y = 1:4
  )

  result <- vis_miss_adv(
    data_test,
    threshold = 25
  )

  summary_result <- attr(
    result,
    "missing_summary"
  )

  x_summary <- summary_result[
    summary_result$variable == "x",
    ,
    drop = FALSE
  ]

  expect_true(x_summary$flagged)
})


test_that("vis_miss_adv melakukan sampling sistematis", {
  data_test <- data.frame(
    x = seq_len(100),
    y = seq_len(100),
    z = seq_len(100)
  )

  data_test$x[100] <- NA

  result <- vis_miss_adv(
    data_test,
    max_cells = 30
  )

  sampling_info <- attr(
    result,
    "sampling_info"
  )

  summary_result <- attr(
    result,
    "missing_summary"
  )

  expect_true(sampling_info$used)
  expect_equal(sampling_info$original_rows, 100)
  expect_equal(sampling_info$plotted_rows, 10)
  expect_equal(sampling_info$method, "systematic")

  x_summary <- summary_result[
    summary_result$variable == "x",
    ,
    drop = FALSE
  ]

  expect_equal(x_summary$n_missing, 1)
  expect_equal(x_summary$pct_missing, 1)
})


test_that("vis_miss_adv menghasilkan ringkasan menurut kelompok", {
  data_test <- data.frame(
    group = c("A", "A", "B", "B"),
    x = c(NA, 1, NA, NA),
    y = c(1, 2, 3, NA)
  )

  result <- vis_miss_adv(
    data_test,
    group = "group",
    threshold = 50
  )

  grouped_summary <- attr(
    result,
    "missing_summary"
  )

  expect_s3_class(result, "ggplot")
  expect_equal(attr(result, "mode"), "grouped")

  x_group_a <- grouped_summary[
    grouped_summary$group_value == "A" &
      grouped_summary$variable == "x",
    ,
    drop = FALSE
  ]

  x_group_b <- grouped_summary[
    grouped_summary$group_value == "B" &
      grouped_summary$variable == "x",
    ,
    drop = FALSE
  ]

  expect_equal(x_group_a$pct_missing, 50)
  expect_equal(x_group_b$pct_missing, 100)
  expect_true(x_group_a$flagged)
  expect_true(x_group_b$flagged)

  expect_false(
    any(
      grouped_summary$variable == "group"
    )
  )
})


test_that("vis_miss_adv menangani kelompok missing", {
  data_test <- data.frame(
    group = c("A", NA, "B"),
    x = c(1, NA, NA)
  )

  result <- vis_miss_adv(
    data_test,
    group = "group"
  )

  grouped_summary <- attr(
    result,
    "missing_summary"
  )

  expect_true(
    any(
      grouped_summary$group_value == "<NA>"
    )
  )
})


test_that("vis_miss_adv menangani data tanpa missing", {
  data_test <- data.frame(
    x = 1:5,
    y = letters[1:5]
  )

  result <- vis_miss_adv(data_test)

  summary_result <- attr(
    result,
    "missing_summary"
  )

  expect_s3_class(result, "ggplot")
  expect_true(
    all(summary_result$n_missing == 0)
  )
  expect_true(
    all(summary_result$pct_missing == 0)
  )
})


test_that("vis_miss_adv memvalidasi threshold", {
  expect_error(
    vis_miss_adv(
      airquality,
      threshold = -1
    ),
    "antara 0 dan 100"
  )

  expect_error(
    vis_miss_adv(
      airquality,
      threshold = 101
    ),
    "antara 0 dan 100"
  )
})


test_that("vis_miss_adv memvalidasi max_cells", {
  expect_error(
    vis_miss_adv(
      airquality,
      max_cells = 0
    ),
    "bilangan bulat positif"
  )

  expect_error(
    vis_miss_adv(
      airquality,
      max_cells = 10.5
    ),
    "bilangan bulat positif"
  )
})


test_that("vis_miss_adv memvalidasi group", {
  expect_error(
    vis_miss_adv(
      airquality,
      group = "tidak_ada"
    ),
    "tidak ditemukan"
  )

  expect_error(
    vis_miss_adv(
      airquality,
      group = 1
    ),
    "nama variabel"
  )
})


test_that("vis_miss_adv menolak data tanpa baris", {
  data_test <- data.frame(
    x = numeric()
  )

  expect_error(
    vis_miss_adv(data_test),
    "minimal satu baris"
  )
})


test_that("vis_miss_adv menolak data yang hanya berisi group", {
  data_test <- data.frame(
    group = c("A", "B", "C")
  )

  expect_error(
    vis_miss_adv(
      data_test,
      group = "group"
    ),
    "selain variabel kelompok"
  )
})


test_that("vis_miss_adv memvalidasi argumen logis", {
  expect_error(
    vis_miss_adv(
      airquality,
      sort_by_missing = "ya"
    ),
    "TRUE atau FALSE"
  )

  expect_error(
    vis_miss_adv(
      airquality,
      show_labels = 1
    ),
    "TRUE atau FALSE"
  )
})
