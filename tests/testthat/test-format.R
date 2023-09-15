test_that("Printing works for selenider elements", {
  session <- selenider_test_session()

  element <- s(".myclass")

  expect_snapshot(print(element))

  element <- s(".myclass", xpath = ".//p")

  expect_snapshot(print(element))

  element <- s(".myclass", xpath = ".//a", link_text = "Link")

  expect_snapshot(print(element))

  element <- find_element(
    s(".myclass", xpath = ".//a", link_text = "Link"),
    ".myclass2"
  )

  expect_snapshot(print(element))

  element <- find_element(
    s(".myclass", xpath = ".//a", link_text = "Link"),
    ".myclass2", xpath = ".//p"
  )

  expect_snapshot(print(element))

  element <- find_element(
    find_element(
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

  elements <- find_elements(
    s(".myclass", xpath = ".//a", link_text = "Link"),
    ".myclass2", xpath = ".//p"
  )

  expect_snapshot(print(elements))

  elements <- find_elements(
    find_element(
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

  expect_snapshot(print(elem_filter(elements, is_present)))
  expect_snapshot(print(elem_find(elements, is_present)))

  expect_snapshot(print(elem_filter(elements, is_present, is_enabled)))
  expect_snapshot(print(elem_find(elements, is_present, is_enabled)))

  expect_snapshot(print(elem_filter(elements, is_present)[[4]]))
  expect_snapshot(print(elem_filter(elements, is_present)[2:6]))
  expect_snapshot(print(elem_filter(elements, is_present)[-3]))
})

test_that("Printing DOM-relative selectors works", {
  session <- selenider_test_session()

  element <- s(".class")

  expect_snapshot(print(elem_ancestors(element)))
  expect_snapshot(print(elem_ancestors(element)[[1]]))
  expect_snapshot(print(elem_ancestors(element)[-1]))

  expect_snapshot(print(elem_parent(element)))

  expect_snapshot(print(elem_siblings(element)))
  expect_snapshot(print(elem_siblings(element)[[1]]))
  expect_snapshot(print(elem_siblings(element)[-1]))

  expect_snapshot(print(elem_children(element)))
  expect_snapshot(print(elem_children(element)[[1]]))
  expect_snapshot(print(elem_children(element)[-1]))

  expect_snapshot(print(elem_descendants(element)))
  expect_snapshot(print(elem_descendants(element)[[1]]))
  expect_snapshot(print(elem_descendants(element)[-1]))
})

test_that("Printing flattened objects works", {
  session <- selenider_test_session()

  # The contents don't matter
  elements <- elem_flatten(s(".random-class"))

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[1:5]))
  expect_snapshot(print(elements[-3]))

  expect_snapshot(print(elem_filter(elements, is_present)))
  expect_snapshot(print(elem_find(elements, is_present)))
  expect_snapshot(print(elem_filter(elements, is_present, is_enabled)))
})

test_that("Printing results of elem_flatmap() works", {
  session <- selenider_test_session()

  elements <- elem_flatmap(
    ss(".class"),
    function(x) find_element(x, "p")
  )

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-1]))
  expect_snapshot(print(elem_filter(elements, is_visible)))

  elements <- elem_flatmap(ss(".class"), elem_children)

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-seq_len(10)]))
  expect_snapshot(print(elem_find(elements, is_visible)))

  elements <- elem_flatmap(
    ss(".class"),
    function(x) elem_children(x)[[1]]
  )

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-1]))
  expect_snapshot(print(elem_filter(elements, is_visible)))

  elements <- elem_flatmap(
    elem_flatmap(ss(".class"), function(x) find_elements(x, "#id")),
    elem_parent
  )

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-1]))
  expect_snapshot(print(elem_filter(elements, is_visible)))

  elements <- elem_flatmap(
    ss(".class"),
    function(x)  elem_flatmap(elem_children(x), function(y) find_element(y, "p"))
  )

  expect_snapshot(print(elements))
  expect_snapshot(print(elements[[1]]))
  expect_snapshot(print(elements[-seq_len(10)]))
  expect_snapshot(print(elem_find(elements, is_visible)))
})

test_that("Printing works for results of JS expressions.", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  js_element <- execute_js_fn("x => x", s(".toggleable"))
  
  expect_snapshot(print(js_element))
  expect_snapshot(print(find_element(js_element, "p")))

  js_elements <- execute_js_fn("x => [x, x]", s(".toggleable"))

  expect_snapshot(print(js_elements))
  expect_snapshot(print(js_elements[1:2]))
  expect_snapshot(print(js_elements[[1]]))
})
