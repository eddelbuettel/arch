
test_that("arrays can be constructed from R objects", {
  a_null <- arch_array_data()
  expect_identical(a_null$buffers, NULL)
  expect_identical(a_null$length, as.integer(0))
  expect_identical(a_null$null_count, as.integer(-1))
  expect_identical(a_null$offset, as.integer(0))
  expect_identical(a_null$n_children, as.integer(0))
  expect_identical(a_null$n_buffers, as.integer(0))
  expect_identical(a_null$children, NULL)
  expect_identical(a_null$dictionary, NULL)

  expect_match(format(a_null), "arch_array")
  expect_output(expect_identical(print(a_null, "arch_array"), a_null))

  a_buf <- arch_array_data(1:10, 10)
  expect_identical(a_buf$buffers[[1]], 1:10)
  expect_match(format(a_buf), "arch_array")
  expect_output(expect_identical(print(a_buf, "arch_array"), a_buf))

  a_buf2 <- arch_array_data(list(1:10, 1:5), 10)
  expect_identical(a_buf2$buffers, list(1:10, 1:5))
  expect_match(format(a_buf2), "arch_array")
  expect_output(expect_identical(print(a_buf2, "arch_array"), a_buf2))

  a_dict <- arch_array_data(dictionary = arch_array_data(1:10, 10))
  expect_identical(a_dict$dictionary$length, as.integer(10))
  expect_match(format(a_dict), "arch_array")
  expect_output(expect_identical(print(a_dict, "arch_array"), a_dict))

  a_children <- arch_array_data(children = list(arch_array_data(1:10, 10)))
  expect_identical(a_children$children[[1]]$length, as.integer(10))
  expect_match(format(a_children), "arch_array")
  expect_output(expect_identical(print(a_children, "arch_array"), a_children))
})

test_that("arch_array_data() list interface works", {
  a <- arch_array_data()
  expect_identical(a$length, as.integer(0))
  expect_identical(a[["length"]], as.integer(0))
  expect_identical(names(a), names(arch_array_data_info(a)))
  expect_identical(length(a), length(arch_array_data_info(a)))
})
