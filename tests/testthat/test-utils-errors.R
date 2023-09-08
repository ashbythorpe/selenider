test_that("Errors are thrown correctly", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_error(is_visible(s(".random-class")), class = "selenider_error_absent_element")

  expect_error(html_size(html_children(s(".random-class")), timeout = 0), class = "selenider_error_absent_parent")

  expect_error(is_covered(s(".toggleable")), class = "selenider_error_invisible_element")

  expect_error(html_expect(s(".random-class"), is_present, timeout = 0, testthat = FALSE), class = "selenider_expect_error")

  expect_error(s(), class = "selenider_error_bad_selector")

  expect_error(click(s(".random-class"), timeout = 0), class = "selenider_error_not_actionable")

  expect_error(ss(".class")[[s("a")]], class = "selenider_error_subscript_type")

  expect_error(ss(".class")[[c(1,2)]], class = "selenider_error_subscript_length")

  expect_error(ss(".class")[[NA]], class = "selenider_error_subscript_na")

  expect_error(ss(".class")[[0]], class = "selenider_error_subscript_zero")

  expect_error(
    ss(".class")[1:5][[6]],
    class = "selenider_error_subscript_max_length"
  )

  expect_warning(
    ss(".class")[1:5][6],
    class = "selenider_warning_subscript_max_length"
  )

  expect_error(html_expect(s(".random-class")), class = "selenider_error_no_conditions")

  expect_error(html_expect(s(".random-class"), exists), class = "selenider_error_base_exists")

  empty_elements <- suppressWarnings(ss(".class1")[0])

  expect_error(
    html_find(empty_elements, is_visible),
    class = "selenider_error_empty_elements_find"
  )

  element_1 <- s(".class1")
  element_2 <- s(".class2")

  element_2$driver_id <- if (element_1$driver_id == 1) 2 else 1

  expect_error(
    html_flatten(element_1, element_2),
    class = "selenider_error_incompatible_drivers"
  )

  expect_error(
    html_flatten(),
    class = "selenider_error_dots_empty"
  )

  expect_error(
    html_flatten(1),
    class = "selenider_error_flatten_dots"
  )

  expect_error(
    html_flatmap(ss(".class"), function(x) 1),
    class = "selenider_error_flatmap_result"
  )

  expect_error(
    send_keys(s(".class"), 1),
    class = "selenider_error_invalid_keys"
  )

  expect_error(
    take_screenshot(),
    class = "selenider_error_screenshot_file"
  )

  expect_warning(
    forward(),
    class = "selenider_warning_history_page"
  )
})
