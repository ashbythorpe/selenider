test_that("read_html() works on selenider elements", {
  skip_if_not_installed("xml2")

  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_no_error(xml2::read_html(session))

  expect_equal(
    xml2::read_html(s("#toggle_div")),
    xml2::read_html('<button id="toggle_div">Toggle div</button>')
  )

  expect_equal(
    xml2::read_html(s(".toggleable"), outer = FALSE),
    xml2::read_html('<p>Hello!</p>')
  )
})
