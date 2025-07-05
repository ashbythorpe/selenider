test_that("chromote errors are caught correctly", {
  session <- chromote::ChromoteSession$new()

  get_node_id <- function() {
    document <- session$DOM$getDocument()
    session$DOM$querySelector(document$root$nodeId, "*")$nodeId
  }

  session$go_to("https://google.com")

  node_id <- get_node_id()
  backend_id <- chromote_backend_id(node_id, driver = session)
  object_id <- chromote_object_id(node_id, driver = session)

  session$go_to("https://google.com")

  expect_error(
    chromote_node_id(object_id = object_id, driver = session),
    class = "selenider_error_resolve_element"
  )
  expect_error(
    chromote_node_id(backend_id = backend_id, driver = session),
    class = "selenider_error_resolve_element"
  )

  expect_error(
    chromote_backend_id(node_id = node_id, driver = session),
    class = "selenider_error_resolve_element"
  )
  expect_error(
    chromote_backend_id(object_id = object_id, driver = session),
    class = "selenider_error_resolve_element"
  )

  expect_error(
    chromote_object_id(node_id = node_id, driver = session),
    class = "selenider_error_resolve_element"
  )
  expect_error(
    chromote_object_id(backend_id = backend_id, driver = session),
    class = "selenider_error_resolve_element"
  )

  expect_error(
    chromote_get_xy(node_id = node_id, driver = session),
    class = "selenider_error_resolve_element"
  )
  expect_error(
    chromote_get_xy(backend_id = backend_id, driver = session),
    class = "selenider_error_resolve_element"
  )
})
