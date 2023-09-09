test_that("html_expect_all() works", {
  # html_expect_all() uses the diagnose_condition(), same as html_expect(),
  # so we don't need to test this too thoroughly.
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  buttons <- html_children(s(".buttons"))

  expect_no_error(html_expect_all(buttons, is_visible))
  expect_snapshot(html_expect_all(buttons, is_enabled, testthat = FALSE, timeout = 0), error = TRUE)
  expect_snapshot(html_expect_all(buttons, is_enabled, testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(html_expect_all(buttons, is_disabled, testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(html_expect_all(buttons, is_visible, is_disabled, testthat = FALSE, timeout = 0.1), error = TRUE)
})

test_that("html_wait_until_all() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  buttons <- html_children(s(".buttons"))

  expect_true(html_wait_until_all(buttons, is_visible))
  expect_false(html_wait_until_all(buttons, is_enabled, timeout = 0.1))
  expect_false(html_wait_until_all(buttons, is_disabled, timeout = 0.1))
})
