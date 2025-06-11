test_that("Numeric filters work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- ss("button")

  elem_expect(
    elements[2:5],
    has_length(4)
  )

  elem_expect(
    elements[2:5][[3]],
    is_present
  )

  elem_expect(
    elements[-1],
    has_length(length(elements) - 1)
  )

  elem_expect(
    elements[-1][3],
    has_length(1)
  )

  elem_expect(
    elements[-3:-5],
    has_length(length(elements) - 3)
  )

  elem_expect(
    elements[-3:-5][1:5],
    has_length(min(5, length(elements) - 3))
  )

  elem_expect(
    elements[-1][-1],
    has_length(length(elements) - 2)
  )
})

test_that("Other filters work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- elem_children(s(".actions-test"))

  inputs <- elem_filter(elements, has_name("button") || has_name("input"))

  elem_expect(inputs, has_length(3))

  submit_button <- elem_find(inputs, has_attr("type", "submit"))

  expect_true(
    submit_button == find_element(s(".actions-form"), "input[type='submit']")
  )

  first_input <- elem_find(elements, has_name("input"))

  expect_true(
    first_input == s(".actions-input")
  )
})
