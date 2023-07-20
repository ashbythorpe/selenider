test_that("test-site", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  html_expect(s(".toggleable"), !is_visible)
  click(s("#toggle_div"))
  html_expect(s(".toggleable"), is_visible, has_text("Hello"))
  click(s("#toggle_div"))
  html_expect(s(".toggleable"), !is_visible)

  html_expect(
    html_children(s(".buttons"))[[1]],
    is_enabled
  )

  html_expect(
    html_children(s(".buttons"))[[2]],
    is_disabled
  )
})
