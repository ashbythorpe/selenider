test_that("selenider environment works", {
  set_in_env(x = 1)
  expect_equal(get_from_env("x"), 1)
})

test_that("withr functions work", {
  mock_session_1 <- 1
  class(mock_session_1) <- "selenider_session"
  mock_session_2 <- 2
  class(mock_session_2) <- "selenider_session"

  local_session(mock_session_1, close = FALSE)
  expect_equal(get_session(), mock_session_1)

  with_session(mock_session_2,
    {
      expect_equal(get_session(), mock_session_2)
    },
    close = FALSE
  )

  expect_equal(get_session(), mock_session_1)
})
