test_that("Numeric filters work", {
  session <- selenider_test_session()

  elements <- ss(".class")

  expect_equal(
    elements[2:5][[3]]$selectors[[1]]$filter[[1]],
    4
  )

  expect_equal(
    elements[-1][3]$selectors[[1]]$filter[[1]],
    4
  )

  expect_equal(
    elements[-3:-5][1:5]$selectors[[1]]$filter[[1]],
    c(1:2, 6:8)
  )

  expect_equal(
    elements[-5:-10][-2]$selectors[[1]]$filter[[1]],
    c(-5:-10, -2)
  )

  expect_equal(
    elements[-5:-10][-7]$selectors[[1]]$filter[[1]],
    -5:-10
  )

  expect_s3_class(suppressWarnings(elements[1:5][6]), "empty_selenider_elements")
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
