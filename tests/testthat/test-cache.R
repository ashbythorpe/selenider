test_that("Cached elements can be operated on", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  button <- find_element(s(".actions-test"), "button")

  cached_button <- elem_cache(button)

  elem_expect(s("#button-output"), has_exact_text(""))

  elem_hover(cached_button)

  elem_expect(s("#button-output"), has_text("Hovered"))

  elem_click(cached_button)

  elem_expect(s("#button-output"), has_text("Left clicked"))

  elem_right_click(cached_button)

  elem_expect(s("#button-output"), has_text("Right clicked"))

  elem_double_click(cached_button)

  elem_expect(s("#button-output"), has_text("Double clicked"))
})

test_that("Cached elements can be used to make sub-selections", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  element <- find_element(s(".toggleable"), "p")

  cached_element <- find_element(elem_cache(s(".toggleable")), "p")

  expect_true(cached_element == element)

  doubly_cached_element <- elem_cache(find_element(elem_cache(s(".toggleable")), "p"))

  expect_true(doubly_cached_element == element)

  element <- find_elements(s(".buttons"), "button")[[2]]

  cached_element <- find_elements(elem_cache(s(".buttons")), "button")[[2]]

  expect_true(cached_element == element)

  doubly_cached_element <- elem_cache(find_elements(elem_cache(s(".buttons")), "button"))[[2]]

  expect_true(doubly_cached_element == element)
})

test_that("Cached element collections can be filtered", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- elem_children(s(".actions-test"))

  cached_elements <- elem_cache(elements)

  expect_true(cached_elements[[1]] == elements[[1]])

  expect_true(
    elem_find(cached_elements, has_name("button")) == elem_find(elements, has_name("button"))
  )

  subsetted_elements <- cached_elements[-3]

  expect_true(elem_cache(subsetted_elements)[[1]] == subsetted_elements[[1]])

  filtered_elements <- elem_filter(cached_elements, !has_name("p"))

  expect_true(elem_cache(filtered_elements)[[2]] == filtered_elements[[2]])

  cached_and_filtered <- elem_cache(elem_filter(cached_elements[-1], !has_text("Test button")))

  expect_true(
    elem_find(cached_and_filtered, has_value("Submit")) == find_element(s(".actions-test"), "input[type='submit']")
  )
})
