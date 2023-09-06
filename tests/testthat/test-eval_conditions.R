test_that("eval_conditions() works", {
  true_condition <- function(...) TRUE
  false_condition <- function(...) FALSE

  x <- structure(list(), class = "selenider_element")

  conditions_1 <- list(rlang::quo(true_condition), rlang::quo(false_condition))

  result <- eval_conditions(rlang::quo(x), conditions_1, timeout = 0.1)
  expect_equal(result$timeout, 0.1)
  expect_equal(result$calls, list(rlang::quo(true_condition(element)), rlang::quo(false_condition(element))))
  expect_equal(result$exprs, conditions_1)
  expect_equal(result$res, list(n = 2, val = FALSE))
  expect_equal(result$x_res, x)

  conditions_2 <- list(rlang::quo(false_condition()), rlang::quo(true_condition()), rlang::quo(true_condition()))

  result <- eval_conditions(conditions_2[[1]], conditions_2[-1], timeout = 0.1)
  expect_equal(result$timeout, 0.1)
  expect_equal(result$calls, rlang::quos(false_condition(), true_condition(), true_condition()))
  expect_equal(result$exprs, result$calls)
  expect_equal(result$res, list(n = 1, val = FALSE))
  expect_equal(result$x_res, FALSE)

  conditions_3 <- list(rlang::quo(true_condition()), rlang::quo(true_condition()), rlang::quo(false_condition()))

  result <- eval_conditions(conditions_3[[1]], conditions_3[-1], timeout = 0.1)
  expect_equal(result$timeout, 0.1)
  expect_equal(result$calls, rlang::quos(true_condition(), true_condition(), false_condition()))
  expect_equal(result$exprs, result$calls)
  expect_equal(result$res, list(n = 3, val = FALSE))
  expect_equal(result$x_res, TRUE)

  conditions_4 <- list(rlang::quo(true_condition()), rlang::quo(true_condition()), rlang::quo(true_condition()))

  result <- eval_conditions(conditions_4[[1]], conditions_4[-1], timeout = 0.1)
  expect_equal(result$timeout, 0.1)
  expect_equal(result$calls, rlang::quos(true_condition(), true_condition(), true_condition()))
  expect_equal(result$exprs, result$calls)
  expect_equal(result$res, TRUE)
  expect_equal(result$x_res, TRUE)
})

test_that("parse_conditions() works", {
  expr <- rlang::expr({ myfunction() })

  expect_equal(parse_condition_expr(expr, "x"), expr)

  expr <- rlang::expr(!is_present)

  expect_equal(parse_condition_expr(expr, "x"), rlang::expr(!is_present(x)))

  expr <- rlang::expr((is_present))

  expect_equal(parse_condition_expr(expr, "x"), rlang::expr((is_present(x))))

  expr <- rlang::expr(is_present || is_enabled)

  expect_equal(parse_condition_expr(expr, "x"), rlang::expr(is_present(x) || is_enabled(x)))

  expr <- rlang::expr(all(is_present, is_enabled, na.rm = TRUE))

  expect_equal(
    parse_condition_expr(expr, "x"),
    rlang::expr(all(is_present(x), is_enabled(x), na.rm = TRUE))
  )

  expr <- rlang::expr(is_present(x = 1))

  expect_equal(parse_condition_expr(expr, "x"), expr)

  expr <- rlang::expr(myfunction(1, y = 2))

  expect_equal(parse_condition_expr(expr, "x"), rlang::expr(myfunction(x, 1, y = 2)))

  q <- rlang::quo(exists)

  expect_error(parse_condition(q, "x"), class = "selenider_error_base_exists")
})

test_that("make_elem_name() works", {
  x <- rlang::quo(x + x_ + x___)

  expect_equal(make_elem_name(x), "element")
  expect_equal(make_elem_name(x, name = "x"), "x__")
})

test_that("get_expr_string() works", {
  q <- rlang::quo(element_1 + element_2)

  expect_equal(get_expr_string(q), "element_1 + element_2")
})
