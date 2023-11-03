cli::test_that_cli("key printing works", {
  testthat::expect_snapshot({
    print(keys$backspace)
    print(keys$f1)
  })
})

testthat::test_that("Keys matched to Selenium keys", {
  expect_equal(
    get_rselenium_key(keys$backspace),
    RSelenium::selKeys$backspace
  )

  expect_equal(
    get_rselenium_key(keys$f1),
    RSelenium::selKeys$f1
  )

  expect_equal(
    get_rselenium_key(keys$command),
    RSelenium::selKeys$command_meta
  )
})

test_that("Each key has a selenium key definition", {
  for (key in keys) {
    rlang::inject(expect_no_error(get_rselenium_key(!!key)))
  }
})

test_that("Each key has a chromote key definition", {
  for (key in keys) {
    rlang::inject(expect_no_error(get_chromote_key(!!key)))
  }
})
