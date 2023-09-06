test_that("Cached elements can be operated on", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  button <- html_element(s(".actions-test"), "button")

  cached_button <- cache_element(button)

  html_expect(s("#button-output"), has_exact_text(""))

  hover(cached_button)

  html_expect(s("#button-output"), has_text("Hovered"))

  click(cached_button)

  html_expect(s("#button-output"), has_text("Left clicked"))

  right_click(cached_button)

  html_expect(s("#button-output"), has_text("Right clicked"))

  double_click(cached_button)

  html_expect(s("#button-output"), has_text("Double clicked"))
})

test_that("Cached elements can be used to make sub-selections", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  element <- html_element(s(".toggleable"), "p")

  cached_element <- html_element(cache_element(s(".toggleable")), "p")

  expect_true(cached_element == element)

  doubly_cached_element <- cache_element(html_element(cache_element(s(".toggleable")), "p"))

  expect_true(doubly_cached_element == element)

  element <- html_elements(s(".buttons"), "button")[[2]]

  cached_element <- html_elements(cache_element(s(".buttons")), "button")[[2]]

  expect_true(cached_element == element)

  doubly_cached_element <- cache_elements(html_elements(cache_element(s(".buttons")), "button"))[[2]]

  expect_true(doubly_cached_element == element)
})

test_that("Cached element collections can be filtered", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- html_children(s(".actions-test"))

  cached_elements <- cache_elements(elements)

  expect_true(cached_elements[[1]] == elements[[1]])

  expect_true(
    html_find(cached_elements, has_name("button")) == html_find(elements, has_name("button"))
  )

  subsetted_elements <- cached_elements[-3]

  expect_true(cache_elements(subsetted_elements)[[1]] == subsetted_elements[[1]])

  filtered_elements <- html_filter(cached_elements, !has_name("p"))

  expect_true(cache_elements(filtered_elements)[[2]] == filtered_elements[[2]])

  cached_and_filtered <- cache_elements(html_filter(cached_elements[-1], !has_text("Test button")))

  expect_true(
    html_find(cached_and_filtered, has_value("Submit")) == html_element(s(".actions-test"), "input[type='submit']")
  )
})
