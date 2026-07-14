test_that("vis_distribution menghasilkan overview berbentuk ggplot", {
  result <- vis_distribution(mtcars)

  expect_s3_class(
    result,
    "ggplot"
  )

  expect_true(
    all(is.finite(result$data$skewness))
  )

  expect_true(
    all(result$data$numeric)
  )
})


test_that("vis_distribution menghasilkan histogram satu variabel", {
  result <- vis_distribution(
    mtcars,
    variable = "mpg",
    bins = 10
  )

  expect_s3_class(
    result,
    "ggplot"
  )

  expect_equal(
    result$labels$x,
    "mpg"
  )

  expect_match(
    result$labels$title,
    "mpg"
  )
})


test_that("vis_distribution menambahkan penanda outlier", {
  data_test <- data.frame(
    score = c(1, 2, 2, 3, 3, 4, 100)
  )

  result <- vis_distribution(
    data_test,
    variable = "score",
    show_outliers = TRUE
  )

  expect_s3_class(
    result,
    "ggplot"
  )

  expect_gte(
    length(result$layers),
    4
  )

  expect_match(
    result$labels$caption,
    "menceng ke kanan"
  )
})


test_that("vis_distribution menerima show_outliers FALSE", {
  data_test <- data.frame(
    score = c(1, 2, 2, 3, 3, 4, 100)
  )

  result <- vis_distribution(
    data_test,
    variable = "score",
    show_outliers = FALSE
  )

  expect_s3_class(
    result,
    "ggplot"
  )

  expect_equal(
    length(result$layers),
    3
  )
})


test_that("vis_distribution menolak data tanpa variabel numerik", {
  data_test <- data.frame(
    group = c("A", "B", "C")
  )

  expect_error(
    vis_distribution(data_test),
    "tidak memiliki variabel numerik"
  )
})


test_that("vis_distribution menolak nama variabel yang tidak ditemukan", {
  expect_error(
    vis_distribution(
      mtcars,
      variable = "variabel_tidak_ada"
    ),
    "tidak ditemukan"
  )
})


test_that("vis_distribution menolak variabel nonnumerik", {
  data_test <- data.frame(
    score = c(1, 2, 3),
    group = c("A", "B", "C")
  )

  expect_error(
    vis_distribution(
      data_test,
      variable = "group"
    ),
    "harus berupa variabel numerik"
  )
})


test_that("vis_distribution menolak nilai bins yang tidak valid", {
  expect_error(
    vis_distribution(
      mtcars,
      variable = "mpg",
      bins = 0
    ),
    "bilangan bulat positif"
  )

  expect_error(
    vis_distribution(
      mtcars,
      variable = "mpg",
      bins = 2.5
    ),
    "bilangan bulat positif"
  )
})


test_that("vis_distribution menolak show_outliers yang tidak logis", {
  expect_error(
    vis_distribution(
      mtcars,
      variable = "mpg",
      show_outliers = "ya"
    ),
    "TRUE atau FALSE"
  )
})


test_that("vis_distribution menangani variabel konstan secara informatif", {
  data_test <- data.frame(
    constant = rep(5, 10)
  )

  expect_error(
    vis_distribution(data_test),
    "Skewness tidak dapat dihitung"
  )

  detail_result <- vis_distribution(
    data_test,
    variable = "constant"
  )

  expect_s3_class(
    detail_result,
    "ggplot"
  )

  expect_match(
    detail_result$labels$caption,
    "belum dapat diberikan"
  )
})


test_that("vis_distribution menolak variabel tanpa nilai hingga", {
  data_test <- data.frame(
    value = c(NA_real_, Inf, -Inf)
  )

  expect_error(
    vis_distribution(
      data_test,
      variable = "value"
    ),
    "tidak memiliki nilai numerik hingga"
  )
})
