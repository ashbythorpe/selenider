test_that("call_insert works", {
  call <- rlang::call2("c", 1, 2, 3)
  expect_equal(
    call_insert(call, "element", quo = FALSE),
    rlang::expr(c(element, 1, 2, 3))
  )

  complex_call <- rlang::expr(x$a(1)(2, 3))
  expect_equal(
    call_insert(complex_call, "element", quo = FALSE),
    rlang::expr(x$a(1)(element, 2, 3))
  )

  env <- rlang::new_environment()
  call <- rlang::expr(fun(a, b))
  quo <- rlang::new_quosure(call, env)
  expect_equal(
    call_insert(quo, "element"),
    rlang::new_quosure(
      rlang::expr(fun(element, a, b)),
      env
    )
  )
})
