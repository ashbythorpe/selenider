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
