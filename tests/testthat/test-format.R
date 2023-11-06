test_that("Lazy printing works for selenider elements", {
  session <- selenider_test_session()

  element <- s(".myclass")

  expect_snapshot(print_lazy(element))

  element <- s(".myclass", xpath = ".//p")

  expect_snapshot(print_lazy(element))

  element <- s(".myclass", xpath = ".//a", name = "name")

  expect_snapshot(print_lazy(element))

  element <- find_element(
    s(".myclass", xpath = ".//a", name = "name"),
    ".myclass2"
  )

  expect_snapshot(print_lazy(element))

  element <- find_element(
    s(".myclass", xpath = ".//a", name = "name"),
    ".myclass2",
    xpath = ".//p"
  )

  expect_snapshot(print_lazy(element))

  element <- find_element(
    find_element(
      s("a"),
      "b"
    ),
    "c"
  )

  expect_snapshot(print_lazy(element))
})

test_that("Lazy printing works for selenider element collections", {
  session <- selenider_test_session()

  elements <- ss(".myclass")

  expect_snapshot(print_lazy(elements))

  elements <- ss(".myclass", xpath = ".//a", name = "name")

  expect_snapshot(print_lazy(elements))

  elements <- find_elements(
    s(".myclass", xpath = ".//a", name = "name"),
    ".myclass2",
    xpath = ".//p"
  )

  expect_snapshot(print_lazy(elements))

  elements <- find_elements(
    find_element(
      s("a"),
      "b"
    ),
    "c"
  )

  expect_snapshot(print_lazy(elements))
})

test_that("Lazy printing filters works", {
  session <- selenider_test_session()

  elements <- ss(".myclass")

  expect_snapshot(print_lazy(elements[[1]]))
  expect_snapshot(print_lazy(elements[[5]]))
  expect_snapshot(print_lazy(elements[5]))
  expect_snapshot(print_lazy(elements[5:10]))
  expect_snapshot(print_lazy(elements[-4]))
  expect_snapshot(print_lazy(elements[-seq_len(6)]))

  expect_snapshot(print_lazy(elem_filter(elements, is_present)))
  expect_snapshot(print_lazy(elem_find(elements, is_present)))

  expect_snapshot(print_lazy(elem_filter(elements, is_present, is_enabled)))
  expect_snapshot(print_lazy(elem_find(elements, is_present, is_enabled)))

  expect_snapshot(print_lazy(elem_filter(elements, is_present)[[4]]))
  expect_snapshot(print_lazy(elem_filter(elements, is_present)[2:6]))
  expect_snapshot(print_lazy(elem_filter(elements, is_present)[-3]))
})

test_that("Lazy printing DOM-relative selectors works", {
  session <- selenider_test_session()

  element <- s(".class")

  expect_snapshot(print_lazy(elem_ancestors(element)))
  expect_snapshot(print_lazy(elem_ancestors(element)[[1]]))
  expect_snapshot(print_lazy(elem_ancestors(element)[-1]))

  expect_snapshot(print_lazy(elem_parent(element)))

  expect_snapshot(print_lazy(elem_siblings(element)))
  expect_snapshot(print_lazy(elem_siblings(element)[[1]]))
  expect_snapshot(print_lazy(elem_siblings(element)[-1]))

  expect_snapshot(print_lazy(elem_children(element)))
  expect_snapshot(print_lazy(elem_children(element)[[1]]))
  expect_snapshot(print_lazy(elem_children(element)[-1]))

  expect_snapshot(print_lazy(elem_descendants(element)))
  expect_snapshot(print_lazy(elem_descendants(element)[[1]]))
  expect_snapshot(print_lazy(elem_descendants(element)[-1]))
})

test_that("Lazy printing flattened objects works", {
  session <- selenider_test_session()

  # The contents don't matter
  elements <- elem_flatten(s(".random-class"))

  expect_snapshot(print_lazy(elements))
  expect_snapshot(print_lazy(elements[[1]]))
  expect_snapshot(print_lazy(elements[1:5]))
  expect_snapshot(print_lazy(elements[-3]))

  expect_snapshot(print_lazy(elem_filter(elements, is_present)))
  expect_snapshot(print_lazy(elem_find(elements, is_present)))
  expect_snapshot(print_lazy(elem_filter(elements, is_present, is_enabled)))
})

test_that("Lazy printing results of elem_flatmap() works", {
  session <- selenider_test_session()

  elements <- elem_flatmap(
    ss(".class"),
    function(x) find_element(x, "p")
  )

  expect_snapshot(print_lazy(elements))
  expect_snapshot(print_lazy(elements[[1]]))
  expect_snapshot(print_lazy(elements[-1]))
  expect_snapshot(print_lazy(elem_filter(elements, is_visible)))

  elements <- elem_flatmap(ss(".class"), elem_children)

  expect_snapshot(print_lazy(elements))
  expect_snapshot(print_lazy(elements[[1]]))
  expect_snapshot(print_lazy(elements[-seq_len(10)]))
  expect_snapshot(print_lazy(elem_find(elements, is_visible)))

  elements <- elem_flatmap(
    ss(".class"),
    function(x) elem_children(x)[[1]]
  )

  expect_snapshot(print_lazy(elements))
  expect_snapshot(print_lazy(elements[[1]]))
  expect_snapshot(print_lazy(elements[-1]))
  expect_snapshot(print_lazy(elem_filter(elements, is_visible)))

  elements <- elem_flatmap(
    elem_flatmap(ss(".class"), function(x) find_elements(x, "#id")),
    elem_parent
  )

  expect_snapshot(print_lazy(elements))
  expect_snapshot(print_lazy(elements[[1]]))
  expect_snapshot(print_lazy(elements[-1]))
  expect_snapshot(print_lazy(elem_filter(elements, is_visible)))

  elements <- elem_flatmap(
    ss(".class"),
    function(x) elem_flatmap(elem_children(x), function(y) find_element(y, "p"))
  )

  expect_snapshot(print_lazy(elements))
  expect_snapshot(print_lazy(elements[[1]]))
  expect_snapshot(print_lazy(elements[-seq_len(10)]))
  expect_snapshot(print_lazy(elem_find(elements, is_visible)))
})

test_that("Lazy printing works for results of JS expressions.", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  js_element <- execute_js_fn("x => x", s(".toggleable"))

  expect_snapshot(print_lazy(js_element))
  expect_snapshot(print_lazy(find_element(js_element, "p")))

  js_elements <- execute_js_fn("x => [x, x]", s(".toggleable"))

  expect_snapshot(print_lazy(js_elements))
  expect_snapshot(print_lazy(js_elements[1:2]))
  expect_snapshot(print_lazy(js_elements[[1]]))
})
