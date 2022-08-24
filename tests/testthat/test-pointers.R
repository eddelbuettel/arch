
test_that("pointer constructors work for schema", {
  schema <- arch_schema("i")
  expect_identical(arch_schema_from_pointer(schema)$format, "i")

  schema_dbl <- arch_pointer_addr_dbl(schema)
  expect_identical(arch_schema_from_pointer(schema_dbl)$format, "i")

  schema_chr <- arch_pointer_addr_chr(schema)
  expect_identical(arch_schema_from_pointer(schema_chr)$format, "i")

  expect_error(arch_schema_from_pointer("not_a_number"), "could not be interpreted")
  expect_error(arch_schema_from_pointer(c(1, 2)), "Pointer must be")
})

test_that("pointer constructors work for array_data", {
  array_data <- arch_array_data()
  expect_identical(arch_array_data_from_pointer(array_data)$length, 0L)
})

test_that("pointer constructors work for array_stream", {
  stream <- arch_array_stream(schema = as_arch_schema("i"))
  expect_identical(
    arch_schema_info(arch_array_stream_get_schema(stream)),
    arch_schema_info(arch_schema("i"))
  )
})

test_that("arch_pointer_is_valid() works", {
  expect_true(arch_pointer_is_valid(arch_schema("i")))
  expect_true(arch_pointer_is_valid(arch_array_data()))
  expect_true(arch_pointer_is_valid(arch_array_stream()))

  expect_false(arch_pointer_is_valid(arch_allocate_schema()))
  expect_false(arch_pointer_is_valid(arch_allocate_array_data()))
  expect_false(arch_pointer_is_valid(arch_allocate_array_stream()))

  expect_error(arch_pointer_is_valid(NULL), "must inherit from")
})

test_that("arch_pointer_release() works", {
  ptr <- arch_schema("i")
  expect_true(arch_pointer_is_valid(ptr))
  arch_pointer_release(ptr)
  expect_false(arch_pointer_is_valid(ptr))

  ptr <- arch_array_data()
  expect_true(arch_pointer_is_valid(ptr))
  arch_pointer_release(ptr)
  expect_false(arch_pointer_is_valid(ptr))

  ptr <- arch_array_stream()
  expect_true(arch_pointer_is_valid(ptr))
  arch_pointer_release(ptr)
  expect_false(arch_pointer_is_valid(ptr))

  expect_error(arch_pointer_release(NULL), "must inherit from")
})

test_that("arch_pointer_move() works for schema", {
  ptr <- arch_schema("i")
  dst <- arch_allocate_schema()
  arch_pointer_move(ptr, dst)
  expect_false(arch_pointer_is_valid(ptr))
  expect_identical(dst$format, "i")

  expect_error(
    arch_pointer_move(ptr, dst),
    "`ptr_dst` is a valid struct ArrowSchema"
  )

  expect_error(
    arch_pointer_move(arch_allocate_schema(), ptr),
    "`ptr_src` is not a valid struct ArrowSchema"
  )
})

test_that("arch_pointer_move() works for array_data", {
  ptr <- arch_array_data()
  dst <- arch_allocate_array_data()
  arch_pointer_move(ptr, dst)
  expect_false(arch_pointer_is_valid(ptr))
  expect_identical(dst$length, 0L)

  expect_error(
    arch_pointer_move(ptr, dst),
    "`ptr_dst` is a valid struct ArrowArray"
  )

  expect_error(
    arch_pointer_move(arch_allocate_array_data(), ptr),
    "`ptr_src` is not a valid struct ArrowArray"
  )
})

test_that("arch_pointer_move() works for array_stream", {
  ptr <- arch_array_stream(schema = arch_schema("i"))
  dst <- arch_allocate_array_stream()
  arch_pointer_move(ptr, dst)
  expect_false(arch_pointer_is_valid(ptr))
  expect_identical(arch_array_stream_get_schema(dst)$format, "i")

  expect_error(
    arch_pointer_move(ptr, dst),
    "`ptr_dst` is a valid struct ArrowArrayStream"
  )

  expect_error(
    arch_pointer_move(arch_allocate_array_stream(), ptr),
    "`ptr_src` is not a valid struct ArrowArrayStream"
  )
})

test_that("arch_pointer_export() works for schema", {
  ptr <- arch_schema("i")
  dst <- arch_allocate_schema()
  arch_pointer_export(ptr, dst)
  expect_true(arch_pointer_is_valid(ptr))
  expect_identical(dst$format, "i")

  expect_error(
    arch_pointer_export(ptr, dst),
    "`ptr_dst` is a valid struct ArrowSchema"
  )

  expect_error(
    arch_pointer_export(arch_allocate_schema(), arch_allocate_schema()),
    "`ptr_src` has already been released"
  )
})

test_that("arch_pointer_export() works for array_data", {
  ptr <- arch_array_data()
  dst <- arch_allocate_array_data()
  arch_pointer_export(ptr, dst)
  expect_true(arch_pointer_is_valid(ptr))
  expect_identical(dst$length, 0L)

  expect_error(
    arch_pointer_export(ptr, dst),
    "`ptr_dst` is a valid struct ArrowArray"
  )

  expect_error(
    arch_pointer_export(arch_allocate_array_data(), arch_allocate_array_data()),
    "has already been released"
  )
})

test_that("arch_pointer_export() works for array_stream", {
  ptr <- arch_array_stream(schema = arch_schema("i"))
  dst <- arch_allocate_array_stream()
  arch_pointer_export(ptr, dst)
  expect_true(arch_pointer_is_valid(ptr))
  expect_identical(arch_array_stream_get_schema(dst)$format, "i")

  expect_error(
    arch_pointer_export(ptr, dst),
    "`ptr_dst` is a valid struct ArrowArrayStream"
  )

  expect_error(
    arch_pointer_export(arch_allocate_array_stream(), ptr),
    "has already been released"
  )
})

test_that("pointer address getters work", {
  schema <- arch_schema("i")
  expect_match(arch_pointer_addr_chr(schema), "^[0-9]+$")
  expect_identical(
    as.character(arch_pointer_addr_dbl(schema)),
    arch_pointer_addr_chr(schema)
  )
})
