
test_that("arch_array_stream_function works", {
  i <- 0L
  stream <- arch_array_stream_function(arch_schema("i"), function() {
    i <<- i + 1L
    if (i <= 1) {
      i
    } else if(i <= 2) {
      as_arch_array(i)
    } else if (i <= 3) {
      as_arch_array(i)$array_data
    } else if (i <= 4) {
      stop("Four is a terrible number")
    } else {
      NULL
    }
  })

  expect_identical(
    arch_schema_info(arch_array_stream_get_schema(stream)),
    arch_schema_info(arch_schema("i"))
  )

  expect_identical(from_arch_array(arch_array_stream_get_next(stream)), 1L)
  expect_identical(from_arch_array(arch_array_stream_get_next(stream)), 2L)
  expect_identical(from_arch_array(arch_array_stream_get_next(stream)), 3L)
  expect_error(arch_array_stream_get_next(stream), "Four is a terrible number")
  expect_null(arch_array_stream_get_next(stream))
  expect_error(arch_array_stream_get_next(stream), "array stream is finished")
})

test_that("arch_array_stream_function validates get_next() against schema", {
  stream <- arch_array_stream_function(arch_schema("i"), function() {
    "one"
  })

  expect_error(arch_array_stream_get_next(stream), "Expected 2 buffers")

  stream <- arch_array_stream_function(arch_schema("i"), function() {
    "one"
  }, validate = FALSE)

  expect_silent(arch_array_stream_get_next(stream, validate = FALSE))
})
