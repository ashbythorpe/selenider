test_that("Printing works for selenider elements", {
  session <- selenider_test_session()

  element <- s(".myclass")

  expect_snapshot(print(element))

  element <- s(".myclass", xpath = ".//p")

  expect_snapshot(print(element))

  element <- s(".myclass", xpath = ".//a", link_text = "Link")

  expect_snapshot(print(element))

  element <- html_element(
    s(".myclass", xpath = ".//a", link_text = "Link"),
    ".myclass2"
  )

  expect_snapshot(print(element))

  element <- html_element(
    s(".myclass", xpath = ".//a", link_text = "Link"),
    ".myclass2", xpath = ".//p"
  )

  expect_snapshot(print(element))

  element <- html_element(
    html_element(
      s("a"),
      "b"
    ),
    "c"
  )

  expect_snapshot(print(element))
})

test_that("Printing works for selenider element collections", {
  session <- selenider_test_session()

  elements <- ss(".myclass")

  expect_snapshot(print(elements))

  elements <- ss(".myclass", xpath = ".//a", link_text = "Link")

  expect_snapshot(print(elements))

  elements <- html_elements(
    s(".myclass", xpath = ".//a", link_text = "Link"),
    ".myclass2", xpath = ".//p"
  )

  expect_snapshot(print(elements))

  elements <- html_elements(
    html_element(
      s("a"),
      "b"
    ),
    "c"
  )

  expect_snapshot(print(elements))
})

test_that("Printing filters works", {
  session <- selenider_test_session()

  elements <- ss(".myclass")

  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[[5]]))
  expect_snapshot(print(elements[5]))
  expect_snapshot(print(elements[5:10]))
  expect_snapshot(print(elements[-4]))
  expect_snapshot(print(elements[-seq_len(6)]))

  expect_snapshot(print(html_filter(elements, is_present)))
  expect_snapshot(print(html_find(elements, is_present)))

  expect_snapshot(print(html_filter(elements, is_present, is_enabled)))
  expect_snapshot(print(html_find(elements, is_present, is_enabled)))

  expect_snapshot(print(html_filter(elements, is_present)[[4]]))
  expect_snapshot(print(html_filter(elements, is_present)[2:6]))
  expect_snapshot(print(html_filter(elements, is_present)[-3]))
})

test_that("Printing DOM-relative selectors works", {
  session <- selenider_test_session()

  element <- s(".class")

  expect_snapshot(print(html_ancestors(element)))
  expect_snapshot(print(html_ancestors(element)[[1]]))
  expect_snapshot(print(html_ancestors(element)[-1]))

  expect_snapshot(print(html_parent(element)))

  expect_snapshot(print(html_siblings(element)))
  expect_snapshot(print(html_siblings(element)[[1]]))
  expect_snapshot(print(html_siblings(element)[-1]))

  expect_snapshot(print(html_children(element)))
  expect_snapshot(print(html_children(element)[[1]]))
  expect_snapshot(print(html_children(element)[-1]))

  expect_snapshot(print(html_descendants(element)))
  expect_snapshot(print(html_descendants(element)[[1]]))
  expect_snapshot(print(html_descendants(element)[-1]))
})

test_that("Printing flattened objects works", {
  session <- selenider_test_session()

  # The contents don't matter
  elements <- html_flatten(s(".random-class"))

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[1:5]))
  expect_snapshot(print(elements[-3]))

  expect_snapshot(print(html_filter(elements, is_present)))
  expect_snapshot(print(html_find(elements, is_present)))
  expect_snapshot(print(html_filter(elements, is_present, is_enabled)))
})

test_that("Printing results of html_flatmap() works", {
  session <- selenider_test_session()

  elements <- html_flatmap(
    ss(".class"),
    function(x) html_element(x, "p")
  )

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-1]))
  expect_snapshot(print(html_filter(elements, is_visible)))

  elements <- html_flatmap(ss(".class"), html_children)

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-seq_len(10)]))
  expect_snapshot(print(html_find(elements, is_visible)))

  elements <- html_flatmap(
    ss(".class"),
    function(x) html_children(x)[[1]]
  )

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-1]))
  expect_snapshot(print(html_filter(elements, is_visible)))

  elements <- html_flatmap(
    html_flatmap(ss(".class"), function(x) html_elements(x, "#id")),
    html_parent
  )

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-1]))
  expect_snapshot(print(html_filter(elements, is_visible)))

  elements <- html_flatmap(
    ss(".class"),
    function(x)  html_flatmap(html_children(x), function(y) html_element(y, "p"))
  )

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-seq_len(10)]))
  expect_snapshot(print(html_find(elements, is_visible)))
})
