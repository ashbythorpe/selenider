test_that("multiplication works", {
  session <- selenider_test_session()

  expect_equal(
    execute_js_fn("() => 2 * 2"),
    4
  )

  expect_equal(
    execute_js_expr("return 2 * 2;"),
    4
  )

  expect_equal(
    execute_js_fn("(x, y) => x * y", 2, 2),
    4
  )

  expect_equal(
    execute_js_expr("return arguments[0] * arguments[1];", 2, 2),
    4
  )
})

test_that("execute_js_() work with no arguments", {
  session <- selenider_test_session()

  expect_true(execute_js_fn("x => x === undefined"))

  expect_equal(execute_js_expr("arguments"), NULL)
})

test_that("execute_js_() work with a variable number of arguments", {
  session <- selenider_test_session()

  expect_equal(
    execute_js_fn("x => x[0]", list(1)),
    1
  )

  expect_equal(
    execute_js_expr("return arguments[0][0];", list(1)),
    1
  )

  expect_equal(
    execute_js_fn("(x, y) => [y, x]", 2, 1),
    list(1, 2)
  )

  expect_equal(
    execute_js_expr("return [arguments[1], arguments[0]];", 1, 2),
    list(2, 1)
  )

  expect_equal(
    execute_js_fn("(x, y) => x", 1, 1),
    1
  )
})

test_that("execute_js_() maintain selenider elements when given an identity function", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  element <- s("#toggle_div")

  result <- execute_js_fn("x => x", element)

  expect_true(result == element)

  expr_result <- execute_js_expr("return arguments[0];", element)

  expect_true(expr_result == element)
})

test_that("execute_js_() work with selenider elements", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- find_elements(s("main"), "div")

  expect_true(execute_js_fn("x => x[0]", elements) == elements[[1]])

  expect_true(execute_js_expr("return arguments[0][0];", elements) == elements[[1]])

  element_1 <- s(".toggleable")
  element_2 <- s("#toggle_div")
  combined <- c(element_1, element_2)

  result <- execute_js_fn("(x, y) => [x, y]", element_1, element_2)
  expect_true(result[[1]] == combined[[1]])
  expect_true(result[[2]] == combined[[2]])

  result <- execute_js_expr("return [arguments[0], arguments[1]];", element_1, element_2)
  expect_true(result[[1]] == combined[[1]])
  expect_true(result[[2]] == combined[[2]])

  expect_true(
    execute_js_fn("(x, y, z) => y", elements, element_1, element_2) == element_1
  )

  elem_expect(s(".toggleable"), is_invisible)

  execute_js_fn("x => x.click()", element_2)

  elem_expect(s(".toggleable"), is_visible)
})
