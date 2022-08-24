
test_that("arch_array class works", {
  v <- arch_array()
  expect_s3_class(v, "arch_array")
  expect_identical(as_arch_array(v), v)
  expect_match(format(v), "arch_array n\\[0\\]")
  expect_output(print(v), "arch_array n\\[0\\]")
})

test_that("arch_array_validate works", {
  v <- arch_array()
  expect_identical(arch_array_validate(v), v)

  expect_error(
    arch_array(arch_schema("i"), arch_array_data()),
    "Expected 2 buffers"
  )

  expect_error(
    arch_array(arch_schema("n"), arch_array_data(children = list(arch_array_data()))),
    "does not match number of children of schema"
  )

  expect_error(
    arch_array(
      arch_schema("+s", children = list(arch_schema("u"))),
      arch_array_data(buffers = list(NULL), children = list(arch_array_data()))
    ),
    "Expected 3 buffers"
  )

  expect_error(
    arch_array(
      arch_schema("i", dictionary = arch_schema("u")),
      arch_array_data(buffers = list(NULL, 1L), dictionary = arch_array_data())
    ),
    "Expected 3 buffers"
  )

  expect_silent(arch_array(arch_schema("i"), arch_array_data(buffers = list(NULL, NULL))))
})

test_that("subset-assign on a array does validation", {
  v <- arch_array(arch_schema("i"), arch_array_data(buffers = list(NULL, 1L), null_count = 0, length = 1))
  expect_silent(v$schema <- arch_schema("i"))
  expect_error(v$schema <- arch_schema("u"), "Expected 3 buffers")
})
