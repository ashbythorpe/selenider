test_that("Global actions work", {
  session <- selenider_test_session()

  open_url("https://www.r-project.org/")

  expect_equal(current_url(), "https://www.r-project.org/")

  open_url("https://www.google.com/")

  expect_equal(current_url(), "https://www.google.com/")

  reload()

  expect_equal(current_url(), "https://www.google.com/")

  back()

  expect_equal(current_url(), "https://www.r-project.org/")

  forward()

  expect_equal(current_url(), "https://www.google.com/")

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elem_set_value(s(".actions-input"), "My text")

  elem_expect(s("#text-output"), has_text("My text"))

  reload()

  elem_expect(s("#text-output"), has_exact_text(""))

  file <- withr::local_tempfile(fileext = ".png")

  expect_no_error(take_screenshot(file))

  html <- get_page_source()

  expect_s3_class(html, "xml_document")
})

test_that("Scrolling works", {
  session <- selenider_test_session()

  html <- "
  <!DOCTYPE html>
  <div style = 'height:110%; width:110%; min-height:110vh; min-width:110vh;'></div>
  "

  open_url(paste0("data:text/html,", URLencode(html)))

  scroll_to(10, 20)

  expect_equal(
    execute_js_expr("return window.scrollY;"),
    10
  )

  expect_equal(
    execute_js_expr("return window.scrollX;"),
    20
  )

  scroll_by(-5, -10)

  Sys.sleep(0.1)

  expect_equal(
    execute_js_expr("return window.scrollY;"),
    5
  )

  expect_equal(
    execute_js_expr("return window.scrollX;"),
    10
  )

  scroll_to(0, 0)

  expect_equal(
    execute_js_expr("return window.scrollY;"),
    0
  )

  expect_equal(
    execute_js_expr("return window.scrollX;"),
    0
  )
})
