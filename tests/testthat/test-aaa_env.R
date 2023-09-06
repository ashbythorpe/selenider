test_that("selenider environment works", {
  set_in_env(x = 1)
  expect_equal(get_from_env("x"), 1)
})

test_that("local session functions work", {
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

test_that("local timeout functions work", {
  expect_equal(get_timeout(NULL, NULL), 4)

  local_session(
    structure(
      list(timeout = 10),
      class = "selenider_session"
    ),
    close = FALSE
  )

  expect_equal(get_timeout(NULL, NULL), 10)

  expect_equal(get_timeout(NULL, 9), 9)

  with_timeout(8, {
    expect_equal(get_timeout(NULL, 9), 8)

    expect_equal(get_timeout(7, 9), 7)
  })
})
