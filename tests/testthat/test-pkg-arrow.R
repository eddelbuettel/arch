
test_that("array to Array works", {
  skip_if_not_installed("arrow")

  a <- from_arch_array(as_arch_array(1:5), arrow::Array)
  expect_identical(as.integer(a), as.integer(arrow::Array$create(1:5)))
  b <- from_arch_array(as_arch_array(c("one", "two")), arrow::Array)
  expect_identical(as.character(b), as.character(arrow::Array$create(c("one", "two"))))
})

test_that("array to RecordBatch works", {
  skip_if_not_installed("arrow")

  df <- data.frame(a = 1:5, b = letters[1:5])
  expect_identical(
    as.data.frame(from_arch_array(as_arch_array(df), arrow::RecordBatch)),
    as.data.frame(arrow::RecordBatch$create(a = 1:5, b = letters[1:5]))
  )
})

test_that("array to DataType/Field/Schema works", {
  skip_if_not_installed("arrow")

  a <- from_arch_array(as_arch_array(1:5), get("DataType", asNamespace("arrow")))
  expect_true(a == arrow::int32())

  a <- from_arch_array(as_arch_array(c(NA, 1:5), name = "fieldname"), arrow:::Field)
  expect_true(a == arrow::Field$create("fieldname", arrow::int32()))

  a <- from_arch_array(as_arch_array(data.frame(intcol = c(NA, 1:5))), arrow:::Schema)
  expect_true(a == arrow::schema(intcol = arrow::int32()))
})

test_that("Type to schema works", {
  skip_if_not_installed("arrow")

  s <- as_arch_schema(arrow::int32())
  expect_identical(
    arch_schema_info(s),
    arch_schema_info(arch_schema("i", name = "", flags = arch_schema_flags(nullable = TRUE)))
  )
})

test_that("Field to schema works", {
  skip_if_not_installed("arrow")

  s <- as_arch_schema(arrow::Field$create("field_name", arrow::int32()))
  expect_identical(
    arch_schema_info(s),
    arch_schema_info(
      arch_schema(
        "i", name = "field_name",
        flags = arch_schema_flags(nullable = TRUE)
      )
    )
  )
})

test_that("Schema to schema works", {
  skip_if_not_installed("arrow")

  s <- as_arch_schema(arrow::schema(field_name = arrow::int32()))
  expect_identical(
    arch_schema_info(s, recursive = TRUE),
    arch_schema_info(
      arch_schema(
        "+s", name = "", flags = 0L,
        children = list(
          arch_schema(
            "i", name = "field_name",
            flags = arch_schema_flags(nullable = TRUE)
          )
        )
      ),
      recursive = TRUE
    )
  )
})

test_that("Schema to array works", {
  skip_if_not_installed("arrow")

  v <- as_arch_array(arrow::Scalar$create(1L))
  expect_identical(from_arch_array(v, integer()), 1L)
})

test_that("Array to array works", {
  skip_if_not_installed("arrow")

  v <- as_arch_array(arrow::Array$create(1:5))
  expect_identical(from_arch_array(v, integer()), 1:5)
})

test_that("RecordBatch to array works", {
  skip_if_not_installed("arrow")

  rb <- arrow::record_batch(a = 1L, b = 2, c = "three")
  v <- as_arch_array(rb)
  expect_identical(from_arch_array(v), data.frame(a = 1L, b = 2, c = "three"))
})

test_that("streams can be exported to RecordBatchReader", {
  skip_if_not_installed("arrow")

  df <- data.frame(a = 1L, b = 2, c = "three")
  stream <- as_arch_array_stream(as_arch_array(df))
  reader <- arch_array_stream_to_arrow(stream)
  expect_identical(
    unclass(as.data.frame(reader$read_table())),
    unclass(df)
  )
})

test_that("streams can be imported from Dataset", {
  skip_if(TRUE)
  ##skip_if_not_installed("arrow")

  # some test data
  df <- data.frame(
    a = 1:26,
    b = as.double(1:26),
    c = letters,
    part = as.integer(1:26 %% 4)
  )
  tf <- tempfile()
  arrow::write_dataset(df, tf, part = "part")

  ds <- arrow::open_dataset(tf)
  stream <- as_arch_array_stream(ds)

  schema <- arch_array_stream_get_schema(stream)
  expect_identical(
    arch_schema_info(schema, recursive = TRUE),
    arch_schema_info(as_arch_schema(ds$schema), recursive = TRUE)
  )

  batches <- list()
  while (!is.null(batch <- arch_array_stream_get_next(stream))) {
    batches[[length(batches) + 1]] <- from_arch_array(batch)
  }

  df_recreated <- do.call(rbind, batches)
  df_recreated <- df_recreated[order(df_recreated$a), ]
  expect_equal(as.data.frame(df_recreated), df, ignore_attr = TRUE)

  unlink(tf, recursive = TRUE)
})

test_that("streams can be imported from Table", {
  skip_if_not_installed("arrow")

  # some test data
  df <- data.frame(
    a = 1:26,
    b = as.double(1:26),
    c = letters,
    part = as.integer(1:26 %% 4)
  )

  tbl <- arrow::Table$create(df)
  stream <- as_arch_array_stream(tbl)

  schema <- arch_array_stream_get_schema(stream)
  expect_identical(
    arch_schema_info(schema, recursive = TRUE),
    arch_schema_info(as_arch_schema(tbl$schema), recursive = TRUE)
  )

  batches <- list()
  while (!is.null(batch <- arch_array_stream_get_next(stream))) {
    batches[[length(batches) + 1]] <- from_arch_array(batch)
  }

  df_recreated <- do.call(rbind, batches)
  df_recreated <- df_recreated[order(df_recreated$a), ]
  expect_equal(as.data.frame(df_recreated), df, ignore_attr = TRUE)
})

test_that("streams can be imported from RecordBatchStreamReader", {
  skip_if_not_installed("arrow")

  # some test data
  df <- data.frame(a = 1L, b = 2, c = "three")
  batch <- arrow::record_batch(df)
  tf <- tempfile()

  # write a valid file
  file_obj <- arrow::FileOutputStream$create(tf)
  writer <- arrow::RecordBatchStreamWriter$create(file_obj, batch$schema)
  writer$write(batch)
  writer$close()
  file_obj$close()

  # create the reader
  read_file_obj <- arrow::ReadableFile$create(tf)
  reader <- arrow::RecordBatchStreamReader$create(read_file_obj)

  # export it to arch
  stream <- as_arch_array_stream(reader)

  schema <- arch_array_stream_get_schema(stream)
  expect_identical(
    arch_schema_info(schema, recursive = TRUE),
    arch_schema_info(as_arch_schema(reader$schema), recursive = TRUE)
  )

  # skip("Attempt to read batch from exported RecordBatchFileReader segfaults")
  batch <- arch_array_stream_get_next(stream)
  expect_identical(from_arch_array(batch), df)

  read_file_obj$close()
  unlink(tf)
})

test_that("Arrays can be streamed", {
  skip_if_not_installed("arrow")

  a <- arrow::Array$create(1:5)
  a_stream <- as_arch_array_stream(a)
  expect_identical(
    from_arch_array(arch_array_stream_get_next(a_stream)),
    1:5
  )
  expect_null(arch_array_stream_get_next(a_stream))
})

test_that("ChunkedArrays can be streamed", {
  skip_if_not_installed("arrow")

  a <- arrow::ChunkedArray$create(1:5, 1:3)
  a_stream <- as_arch_array_stream(a)
  expect_identical(
    from_arch_array(arch_array_stream_get_next(a_stream)),
    1:5
  )
  expect_identical(
    from_arch_array(arch_array_stream_get_next(a_stream)),
    1:3
  )
  expect_null(arch_array_stream_get_next(a_stream))
})
