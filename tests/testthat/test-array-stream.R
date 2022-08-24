
test_that("arch array stream constructor returns an empty stream", {
  stream <- arch_array_stream()

  expect_identical(
    arch_schema_info(arch_array_stream_get_schema(stream)),
    arch_schema_info(arch_schema("n"))
  )

  expect_null(arch_array_stream_get_next(stream))
})

test_that("arch array stream constructor works for list()", {
  array <- as_arch_array(1:5)
  stream <- arch_array_stream(
    list(as_arch_array(1:5), as_arch_array(6:10))
  )

  expect_identical(
    arch_schema_info(arch_array_stream_get_schema(stream)),
    arch_schema_info(arch_schema("i"))
  )

  expect_identical(
    from_arch_array(arch_array_stream_get_next(stream)),
    1:5
  )

  expect_identical(
    from_arch_array(arch_array_stream_get_next(stream)),
    6:10
  )

  expect_null(arch_array_stream_get_next(stream))
})

test_that("arch array stream constructor works for a non-list", {
  stream <- arch_array_stream(1:5)

  expect_identical(
    arch_schema_info(arch_array_stream_get_schema(stream)),
    arch_schema_info(arch_schema("i"))
  )

  expect_identical(
    from_arch_array(arch_array_stream_get_next(stream)),
    1:5
  )

  expect_null(arch_array_stream_get_next(stream))
})

test_that("arch array stream constructor validates input", {
  expect_error(
    arch_array_stream(1:5, schema = arch_schema("+s")),
    "Expected 1 buffer"
  )

  expect_silent(
    arch_array_stream(1:5, schema = arch_schema("+s"), validate = FALSE)
  )
})

test_that("as_arch_array_stream for array works", {
  array <- as_arch_array(1:5)
  stream <- as_arch_array_stream(array)

  expect_identical(
    arch_schema_info(arch_array_stream_get_schema(stream)),
    arch_schema_info(arch_schema("i"))
  )

  expect_identical(
    from_arch_array(arch_array_stream_get_next(stream)),
    1:5
  )

  expect_null(arch_array_stream_get_next(stream))
})

test_that("as_arch_array_stream works for list()", {
  stream <- as_arch_array_stream(list(1:5, 6:10))

  expect_identical(
    arch_schema_info(arch_array_stream_get_schema(stream)),
    arch_schema_info(arch_schema("i"))
  )

  expect_identical(
    from_arch_array(arch_array_stream_get_next(stream)),
    1:5
  )

  expect_identical(
    from_arch_array(arch_array_stream_get_next(stream)),
    6:10
  )

  expect_null(arch_array_stream_get_next(stream))
})

test_that("as_arch_array_stream works for function", {
  factory <- function() {
    list(1:5, 6:10)
  }

  stream <- as_arch_array_stream(factory)

  expect_identical(
    arch_schema_info(arch_array_stream_get_schema(stream)),
    arch_schema_info(arch_schema("i"))
  )

  expect_identical(
    from_arch_array(arch_array_stream_get_next(stream)),
    1:5
  )

  expect_identical(
    from_arch_array(arch_array_stream_get_next(stream)),
    6:10
  )

  expect_null(arch_array_stream_get_next(stream))
})

test_that("as_arch_array_stream default method works", {
  stream <- as_arch_array_stream(1:5)

  expect_identical(
    arch_schema_info(arch_array_stream_get_schema(stream)),
    arch_schema_info(arch_schema("i"))
  )

  expect_identical(
    from_arch_array(arch_array_stream_get_next(stream)),
    1:5
  )

  expect_null(arch_array_stream_get_next(stream))
})

test_that("arch_array_stream_collect() works", {
  stream <- as_arch_array_stream(list(1:5, 6:10))
  expect_identical(arch_array_stream_collect(stream), 1:10)

  stream <- as_arch_array_stream(
    list(
      data.frame(a = 1, b = "two"),
      data.frame(a = 3, b = "four")
    )
  )
  expect_identical(
    arch_array_stream_collect(stream),
    data.frame(a = c(1, 3), b = c("two", "four"))
  )
})
