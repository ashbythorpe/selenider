test_that("elem_expect_all() works", {
  # elem_expect_all() uses the diagnose_condition(), same as elem_expect(),
  # so we don't need to test this too thoroughly.
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  buttons <- elem_children(s(".buttons"))

  expect_no_error(elem_expect_all(buttons, is_visible))
  expect_snapshot(elem_expect_all(buttons, is_enabled, testthat = FALSE, timeout = 0), error = TRUE)
  expect_snapshot(elem_expect_all(buttons, is_enabled, is_visible, testthat = FALSE, timeout = 0), error = TRUE)
  expect_snapshot(elem_expect_all(buttons, is_enabled, testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect_all(buttons, is_disabled, testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect_all(buttons, is_visible, is_disabled, testthat = FALSE, timeout = 0.1), error = TRUE)
})

test_that("elem_wait_until_all() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  buttons <- elem_children(s(".buttons"))

  expect_true(elem_wait_until_all(buttons, is_visible))
  expect_false(elem_wait_until_all(buttons, is_enabled, timeout = 0.1))
  expect_false(elem_wait_until_all(buttons, is_disabled, timeout = 0.1))
})
