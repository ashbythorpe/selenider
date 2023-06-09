cli::test_that_cli("key printing works", {
  testthat::expect_snapshot({
    print(keys$backspace)
    print(keys$f1)
  })
})

test_that("Keys matched to Selenium keys", {
  expect_equal(
    get_selenider_key(keys$backspace),
    RSelenium::selKeys$backspace
  )

  expect_equal(
    get_selenider_key(keys$f1),
    RSelenium::selKeys$f1
  )

  expect_equal(
    get_selenider_key(keys$command),
    RSelenium::selKeys$command_meta
  )
})
