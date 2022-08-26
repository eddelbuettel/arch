<!--
%\VignetteIndexEntry{Why 'arch'?}
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteEncoding{UTF-8}
-->
---
title: "Why 'arch'?"
author: "Dewey Dunnington"
date: "January 2022"
css: "water.css"
---

## A developer-facing package

The [Apache Arrow](https://arrow.apache.org/) project is awesome. So is the [R package](https://arrow.apache.org/docs/r/) that provides bindings to the C++ API in a way that mirrors how it's done in [Python](https://arrow.apache.org/docs/python/) and adds a [dplyr](https://dplyr.tidyverse.org/) interface to the single-node query engine. The 'arrow' package for R provides an end-to-end solution for querying and computing on in-memory and bigger-than-memory data sets, including a particularly slick workflow for [hosting data on cloud storage and querying it locally](https://arrow.apache.org/docs/r/articles/fs.html). It's a fantastic user-facing package that makes it possible for users familiar with [dplyr](https://dplyr.tidyverse.org/) to work with larger-than-memory (and larger-than-disk!) data without learning a new skillset.

Apache Arrow also has a lot to offer the R package ecosystem as a developer-facing package. R doesn't natively support many useful types (e.g., fixed-precision decimals and 64-bit integers) and starts to get slow when dealing with really big (millions of elements) character vectors and nested lists. Arrow supports a rich set of types and can slice and rearrange tens of millions of strings without stuttering. Zero-copy transfer between R, Python, Rust, and other language bindings make it possible to take advantage of processing functions exposed in other languages without writing to disk. The parquet, feather, and IPC formats are open, cross-platform versions of Arrow types that are fast to read and write (compared to CSV or serializing R objects) and provide the same rich set of types as in-memory Arrow arrays.

For many R packages, the value offered by the arrow R package as a dependency outweighs any size or scope considerations. For packages with a more limited scope, the Arrow R package may not be a good fit as a dependency. The C API, added to the Arrow R package in [version 5.0.0](https://arrow.apache.org/docs/r/news/index.html#c-interface), provides a path to such a package: whereas the C++ structures used by the arrow R package aren't safe to expose elsewhere, the C API structures *are* safe to pass between packages and embedded interpreters and are able to do so without copying the Array data for most Arrays.

## Scope

I think the main goal of a developer-facing Apache Arrow R package should be to make it easy to use, easy to test, and safe to pass around external pointers to C API structures. If a dependency package wants to operate on an array of unsigned 64-bit integers (like an [S2 cell identifier](https://r-spatial.github.io/s2/reference/s2_cell.html#details)), all they should have to do is run the input through `as_arch_array()`. The package doing the operating doesn't have to know about how or why the input came to be a pointer to a `struct ArrowArray` and a `struct ArrowSchema`. Similarly, any package that wants to represent an object as an array of unsigned 64-bit integers just has to create the appropriate `struct ArrowArray` and `struct ArrowSchema`.

As I've prototyped it here, the idea is similar to DBI except the focus is on types rather than methods. We have four types:

- `arch_schema()`
- `arch_array_data()`
- `arch_array()` (schema + array data)
- `arch_array_stream()`

First, I'll demonstrate "easy". For this example, we need some C or C++ code:

```c++
#include <cpp11.hpp>
#include <arch.h>

using namespace cpp11;

[[cpp11::linking_to(arch)]]
[[cpp11::register]]
void print_uint64_cpp(SEXP array_data_xptr) {
  struct ArrowArray* array_data = safe[array_data_from_xptr](array_data_xptr, "array_data");
  const uint64_t* data = reinterpret_cast<const uint64_t*>(array_data->buffers[1]);
  for (int64_t i = 0; i < array_data->length; i++) {
    Rprintf("%llu\n", data[i]);
  }
}
```

And an R wrapper:

```r
print_uint64 <- function(x) {
  x <- as_arch_array(x, schema = arch_schema("L"))
  stopifnot(identical(x$schema$format, "L"))
  print_uint64_cpp(x$array_data)
}
```

Because I've implemented `as_arch_array()` for arrow `Array` objects, they can be used as input without knowing that the arrow package exists.

```r
print_uint64(arrow::Array$create(1:5)$cast(arrow::uint64()))
#> 1
#> 2
#> 3
#> 4
#> 5
```

This example doesn't require extra code to make it safe, either, because `array_data_from_xptr()` (defined in `arch.h`) checks that the `struct ArrowArray` is not `NULL` nor has a `NULL` release callback. Keen onlookers will notice that it doesn't check the validity buffer for null elements, nor does it check the number of buffers, both of which I'll come back to. The point is that iterating over Arrow arrays takes 6 lines of C++ and 5 lines of R. There's no configure script, and the arch package prototype as I've implemented it here installs in a few seconds and has no dependencies (R dependencies nor system dependencies).

Next I'll demonstrate "easy to test". This particular array only has a few buffers and is reasonable to construct by hand given some knowledge of the format spec.

```r
library(testthat)

uint_raw_test <- function(x) {
  lapply(x, function(e) as.raw(c(e, rep(0x00, 7))))
}

uint_array <- arch_array(
  arch_schema("L"),
  arch_array_data(
    list(NULL, unlist(uint_raw_test(1:3))),
    length = 5,
    null_count = 0
  )
)

expect_output(print_uint64(uint_array), "1\n2\n3\n")
```

It's reasonable to use the Arrow R package to generate test data too, since a development dependency doesn't get installed by default with `install.packages()`. If all you need are three unsigned 64-bit integers, though, I'd argue it's conceptually overkill to require contributors and CI systems to install arrow for R. It's also hard or impossible to create invalid arrays in R/arrow, which is useful to test validation code.

```r
uint_array$schema$format <- "l"
expect_error(
  print_uint64(uint_array),
  "identical.*?is not TRUE"
)

uint_array_invalid <- arch_array(
  arch_schema("L"),
  arch:::arch_allocate_array_data(),
  validate = FALSE
)

expect_error(
  print_uint64(uint_array_invalid),
  "has already been released"
)
```

The "safe" bit is related to "easy" because writing validation code can be tedious and hard to get right. The ability to write a data processing function that takes an Arrow array in 6 lines of C or C++ is closely related to the ability to pass off the responsibility for creating and validating those arrays elsewhere. Because of this, arch makes it hard to generate obviously bad objects:

```r
arch_array(arch_schema("L"), arch_array_data(list()))
#> Error in arch_array_validate(array): Expected 2 buffers for schema type 'L' but found 0 buffers in array [Invalid argument]
```

With the ability to create and validate there is a lot of extra scope...arch has to know how many buffers are expected for all the types and it *should* know how long those buffers need to be (although this isn't implemented).

## Streams?

I haven't mentioned streams to keep this concise, but they're implemented in the prototype as well, including ways to create and consume them. Like arrays, both the creating and the consuming is essential for testing.

```r
stream <- arch_array_stream(list(1:2, 3:5))
arch_array_stream_collect(stream)
#> [1] 1 2 3 4 5
```

## Read, write, slice, filter, take, convert

These are extra things that I haven't implemented that I think should also be included.

- Read and write, because feather/IPC is so much better than CSV and everybody should be using it
- Slice, filter, and take, because it's hard to implement an extension vector type otherwise
- Convert (to and from R objects), because it makes testing a lot easier and makes it possible for more users to take advantage of Arrow types.

For all of these, it's easy to fall back to the arrow R package if it's installed or to fall back to the arrow R package for the hard bits (e.g., casting). I still think all three are worth implementing for a developer-facing R package. There's no need for any of this to be fast because if speed is an issue, users can be directed to the arrow R package.

## A 'arch' C library?

Perhaps the most obvious way to do this kind of thing would be to vendor in a minimal copy of Arrow C++ library and use it to implement the bits that I've mentioned above. Another way to do it is to write and use a tiny vendorable C library. I did it using the C library approach in the prototype because I was having a good time with it (I based the style off the excellent [tensorflow C API](https://github.com/tensorflow/tensorflow/blob/master/tensorflow/c/c_api.h) and the structure off the [rlang C library](https://github.com/r-lib/rlang/tree/main/src/rlang) and [sqlite3](https://www.sqlite.org/download.html)), but that by no means requires that it be the direction a future 'arch' R package needs to go.

I think the tiny vendorable C library is the right approach because it's not trivial to vendor reorganize a C++ project into an R package. Even if *we* can do it, it would be harder for a dependency package to take the same approach if they wanted to, for example, extract metadata from the schema, read the bitpacked validity buffer, or validate arrays themselves in compiled code. I'll also point to the success of SQLite, which ships as two files (sqlite3.h and sqlite3.c), is ubiquitous as a way to read and write typed data to disk, and is the basis for many file formats (e.g., the GeoPackage format for geospatial data).

## Real-world example

I started writing arch to learn about the columnar format and continued adding bits as I started experimenting with [representing geometry as nested Arrow structures](https://fishandwhistle.net/post/2021/prototyping-an-apache-arrow-representation-of-geometry/). In the prototype package implementing this, [geoarrow](https://github.com/paleolimbot/geoarrow), I needed to create nested lists and demonstrate that reading/writing these objects was as fast or faster than some other common ways to do it that existed already. The R spatial developer crowd can be wary of dependencies, so I also wanted to demonstrate that it could be done in a lightweight package. Conceptually this also made sense...reading the C structures didn't take much code and that is a good thing: my goal was that it should take a comparable amount of code to read well-known binary, which is the current best bet for passing geometries between libraries in memory.

I used `arch_array()`, `arch_array_data()`, and `arch_schema()` to create the nested data efficiently. In Python there is `Array.from_buffers()` to do this kind of thing that isn't implemented in R yet to my knowledge. For points, it looked like this:

```r
x <- 1:5
y <- 1:5
interleaved <- rbind(as.double(x), as.double(y))

point_array <- arch_array(
  arch_schema("+w:2", children = list(arch_schema("g", name = ""))),
  arch_array_data(
    length = ncol(interleaved),
    buffers = list(NULL),
    children = list(
      arch_array_data(
        buffers = list(NULL, interleaved),
        length = length(interleaved),
        null_count = 0
      )
    ),
    null_count = 0
  )
)
```

From there I made arrow R package objects and used it to write Parquet, demonstrating that it made smaller files that could be read faster than the gold-standard shapefile (even with compression).

```r
tbl <- arrow::Table$create(points = from_arch_array(point_array, arrow::Array))
arrow::write_parquet(tbl, tempfile())
```

The conversion of C structures back to R representation of geometry is more complex, particularly to support lines, polygons, multi, and more than one point representation (e.g., float, dictionary encoded). I prototyped this, too, but this is where a tiny C library would help. The main things were that I needed to decode the metadata field to inspect the extension type and get metadata (e.g., dimension names), I needed to inspect the bitpacked validity buffer to handle nulls, and I needed to parse the format string to pick the proper point wrapper class to decode the points. Because it's a prototype I copied bits from the C library and modified them to suit, but it would be better if I didn't have to do. Importantly, it might mean that future developers of in-memory and on-disk formats (geometry or otherwise) use Arrow structures more frequently because it is the easiest way to make it happen.

### Colophon

Initially written in January 2022 for the
[narrow](https://github.com/paleolimbot/narrow/) package, this
vignette was adapted for the [arch](https://github.com/eddelbuettel/arch)
package in August 2022 by Dirk Eddelbuettel.
