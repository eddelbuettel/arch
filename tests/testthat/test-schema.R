
test_that("arrow_schema() works with mostly defaults", {
  s <- arrow_schema("i")
  expect_s3_class(s, "arrowc_schema")
  s_data <- as.list(s)
  expect_identical(s_data$format, "i")
  expect_identical(s_data$flags, 0L)
  expect_null(s_data$metadata)
  expect_null(s_data$children)
  expect_null(s_data$dictionary)
  expect_null(s_data$name)
})

test_that("arrow_schema() works with values for all memebers", {
  s <- arrow_schema(
    "i",
    name = "name",
    metadata = list(key = as.raw(0x00)),
    dictionary = arrow_schema("Z"),
    flags = 1L,
    children = list(arrow_schema("d"))
  )

  s_data <- as.list(s)

  expect_identical(s_data$format, "i")
  expect_identical(s_data$flags, 1L)
  expect_null(s_data$metadata)
  expect_length(s_data$children, 1)
  expect_s3_class(s_data$children[[1]], "arrowc_schema")
  expect_s3_class(s_data$dictionary, "arrowc_schema")
  expect_identical(s_data$name, "name")
})

test_that("format(), print(), and str() methods for arrow_schema() work", {
  s <- arrow_schema("i")
  expect_match(format(s), "arrow_schema")
  expect_output(expect_identical(print(s), s), "arrow_schema")
  expect_output(expect_identical(str(s), s), "arrow_schema")
})

test_that("arrow_schema() list interface works", {
  s <- arrow_schema("i")
  expect_identical(s$format, "i")
  expect_identical(s[["format"]], "i")
  expect_identical(names(s), names(as.list(s)))
})