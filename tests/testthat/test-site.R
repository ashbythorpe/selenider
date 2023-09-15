test_that("test-site", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elem_expect(s(".toggleable"), !is_visible)
  elem_click(s("#toggle_div"))
  elem_expect(s(".toggleable"), is_visible, has_text("Hello"))
  elem_click(s("#toggle_div"))
  elem_expect(s(".toggleable"), !is_visible)

  elem_expect(
    elem_children(s(".buttons"))[[1]],
    is_enabled
  )

  elem_expect(
    elem_children(s(".buttons"))[[2]],
    is_disabled
  )
})
