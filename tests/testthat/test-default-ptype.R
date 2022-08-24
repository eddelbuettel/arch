
test_that("arch_default_ptype() errors for non-schema arguments", {
  expect_error(arch_default_ptype(NULL), "`schema` must be a")
})
