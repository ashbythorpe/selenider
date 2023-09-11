test_that("Selenium client/server checking works", {
  expect_error(check_selenium_server(list()), class = "selenider_error_invalid_server")

  expect_error(check_selenium_client(list()), class = "selenider_error_invalid_client")
})
