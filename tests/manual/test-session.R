test_that("Creating sessions using shinytest2 AppDrivers works", {
  testthat::skip_if_not_installed("shinytest2")

  shiny_app <- shiny::shinyApp(ui = shiny::fluidPage(), server = function(input, output) {})

  app <- shinytest2::AppDriver$new(shiny_app)

  session <- selenider_session(driver = app)

  expect_equal(session$session, "chromote")
  expect_s3_class(session$driver, "ChromoteSession")
})

test_that("Creating sessions using RSelenium works", {
  skip_if_selenider_unavailable("selenium")

  session <- selenider_session("selenium", browser = "firefox")

  expect_true(is_selenium_server(session$server))
  expect_true(is_selenium_client(session$driver))
  expect_equal(session$session, "selenium")

  withr::deferred_run()

  server <- create_selenium_server_internal("firefox", options = selenium_server_options(port = 4444L))

  session <- selenider_session(browser = "firefox", driver = server)

  expect_equal(find_port_from_server(server), 4444L)

  expect_true(is_selenium_server(session$server))
  expect_true(is_selenium_client(session$driver))
  expect_equal(session$session, "selenium")

  withr::deferred_run()

  server <- create_selenium_server_internal("firefox", options = selenium_server_options(port = 4445L))

  session <- selenider_session(browser = "firefox", driver = list(server = server))

  expect_true(is_selenium_server(session$server))
  expect_true(is_selenium_client(session$driver))
  expect_equal(session$session, "selenium")

  withr::deferred_run()

  server <- create_selenium_server_internal("firefox", options = selenium_server_options(port = 4446L))

  session <- selenider_session(browser = "firefox", driver = list(server))

  expect_true(is_selenium_server(session$server))
  expect_true(is_selenium_client(session$driver))
  expect_equal(session$session, "selenium")

  withr::deferred_run()

  server <- create_selenium_server_internal("firefox", options = selenium_server_options(port = 4447L))
  client <- create_selenium_client_internal("firefox", options = selenium_client_options(port = 4447L))

  session <- selenider_session(driver = list(client, server))

  expect_true(is_selenium_server(session$server))
  expect_true(is_selenium_client(session$driver))
  expect_equal(session$session, "selenium")

  withr::deferred_run()

  server <- create_selenium_server_internal("firefox", options = selenium_server_options(port = 4448L))
  client <- create_selenium_client_internal("firefox", options = selenium_client_options(port = 4448L))

  session <- selenider_session(driver = list(client = client, server = server))

  expect_true(is_selenium_server(session$server))
  expect_true(is_selenium_client(session$driver))
  expect_equal(session$session, "selenium")

  withr::deferred_run()

  server <- create_selenium_server_internal("firefox", options = selenium_server_options(port = 4449L))
  client <- create_selenium_client_internal("firefox", options = selenium_client_options(port = 4449L))

  session <- selenider_session(driver = list(client))

  expect_null(session$server)
  expect_true(is_selenium_client(session$driver))
  expect_equal(session$session, "selenium")

  withr::deferred_run()
  server$kill()
})
