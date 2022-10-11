
#' Convert to and from arrow package types
#'
#' @param x An object to convert to or from 'arrow' package types
#' @inheritParams from_arch_array
#' @param ... Passed to S3 methods
#'
#' @export
#' @rdname pkg-arrow
#'
from_arch_array.Array <- function(x, ptype, ...) {
    message("from_arch_array.Array")
 arch_arr <- from_arch_array(x, arrow::Array)

 if (arch_arr$type == ptype$type) {
   arch_arr
 } else {
   arch_arr$cast(ptype$type)
 }
}

#' @rdname pkg-arrow
#' @export
from_arch_array.R6ClassGenerator <- function(x, ptype, ...) {
    message("from_arch_array.R6ClassGenerator")
    temp_schema <- arch_allocate_schema()
  temp_array_data <- arch_allocate_array_data()

  switch(
    ptype$classname,
    RecordBatch =,
    Array = {
      message("RecordBatch + Array case")
      arch_pointer_export(x$schema, temp_schema)
      arch_pointer_export(x$array_data, temp_array_data)

      ptype$import_from_c(
        arch_pointer_addr_dbl(temp_array_data),
        arch_pointer_addr_dbl(temp_schema)
      )
    },
    DataType =,
    Field =,
    Schema = {
      arch_pointer_export(x$schema, temp_schema)

      ptype$import_from_c(
        arch_pointer_addr_dbl(temp_schema)
      )
    },
    stop(sprintf("Can't convert from arch_array to R6 type '%s'", ptype$classname))
  )
}

#' @rdname pkg-arrow
#' @export
as_arch_schema.DataType <- function(x, ...) {
  schema <- arch_allocate_schema()
  x$export_to_c(arch_pointer_addr_dbl(schema))
  schema
}

#' @rdname pkg-arrow
#' @export
as_arch_schema.Field <- function(x, ...) {
  schema <- arch_allocate_schema()
  x$export_to_c(arch_pointer_addr_dbl(schema))
  schema
}

#' @rdname pkg-arrow
#' @export
as_arch_schema.Schema <- function(x, ...) {
  schema <- arch_allocate_schema()
  x$export_to_c(arch_pointer_addr_dbl(schema))
  schema
}

#' @rdname pkg-arrow
#' @export
as_arch_array.Scalar <- function(x, ...) {
  as_arch_array(x$as_array(), ...)
}

#' @rdname pkg-arrow
#' @export
as_arch_array.Array <- function(x, ...) {
  schema <- arch_allocate_schema()
  array <- arch_allocate_array_data()
  x$export_to_c(arch_pointer_addr_dbl(array), arch_pointer_addr_dbl(schema))
  arch_array(schema, array, validate = FALSE)
}

#' @rdname pkg-arrow
#' @export
as_arch_array.RecordBatch <- function(x, ...) {
  schema <- arch_allocate_schema()
  array <- arch_allocate_array_data()
  x$export_to_c(arch_pointer_addr_dbl(array), arch_pointer_addr_dbl(schema))
  arch_array(schema, array, validate = FALSE)
}

#' @rdname pkg-arrow
#' @export
arch_array_stream_to_arrow <- function(x) {
  temp_array_stream <- arch_allocate_array_stream()
  arch_pointer_export(x, temp_array_stream)
  asNamespace("arrow")$ImportRecordBatchReader(
    arch_pointer_addr_dbl(temp_array_stream)
  )
}

#' @rdname pkg-arrow
#' @export
as_arch_array_stream.RecordBatchReader <- function(x, ...) {
  array_stream <- arch_allocate_array_stream()
  asNamespace("arrow")$ExportRecordBatchReader(x, arch_pointer_addr_dbl(array_stream))
  array_stream
}

#' @rdname pkg-arrow
#' @export
as_arch_array_stream.Scanner <- function(x, ...) {
  as_arch_array_stream.RecordBatchReader(x$ToRecordBatchReader())
}

#' @rdname pkg-arrow
#' @export
as_arch_array_stream.Dataset <- function(x, ...) {
  as_arch_array_stream.Scanner(arrow::Scanner$create(x, ...))
}

#' @rdname pkg-arrow
#' @export
as_arch_array_stream.Table <- function(x, ...) {
  as_arch_array_stream.Scanner(arrow::Scanner$create(x, ...))
}

#' @rdname pkg-arrow
#' @export
as_arch_array_stream.RecordBatchFileReader <- function(x, ...) {
  as_arch_array_stream.RecordBatchReader(x, ...)
}

#' @rdname pkg-arrow
#' @export
as_arch_array_stream.RecordBatchStreamReader <- function(x, ...) {
  as_arch_array_stream.RecordBatchReader(x, ...)
}

#' @rdname pkg-arrow
#' @export
as_arch_array_stream.Array <- function(x, ...) {
  as_arch_array_stream(as_arch_array(x), ...)
}

#' @rdname pkg-arrow
#' @export
as_arch_array_stream.ChunkedArray <- function(x, ...) {
  arrays <- vector("list", x$num_chunks)
  for (i in seq_along(arrays)) {
    arrays[[i]] <- as_arch_array.Array(x$chunk(i - 1L))
  }

  .Call(
    arch_c_arch_array_stream,
    arrays,
    as_arch_schema(x$type)
  )
}
