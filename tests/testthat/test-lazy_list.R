test_that("lazy lists work", {
  generator <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
    coro::yield(3)
  })

  x <- lazy_list(generator)

  expect_equal(next_value(x), 1)
  expect_equal(next_value(x), 2)
  expect_equal(next_value(x), 3)

  expect_equal(next_value_start(x), 1)
  expect_equal(next_value(x), 2)
  expect_equal(next_value(x), 3)
  expect_equal(next_value(x), coro::exhausted())
})

test_that("eager lists work", {
  data <- list(1, 2, 3, 4)

  x <- eager_list(data)

  expect_equal(next_value(x), 1)
  expect_equal(next_value(x), 2)
  expect_equal(next_value(x), 3)

  expect_equal(next_value_start(x), 1)
  expect_equal(next_value(x), 2)
  expect_equal(next_value(x), 3)
  expect_equal(next_value(x), 4)
  expect_equal(next_value(x), coro::exhausted())

  expect_equal(x[[1]], 1)
})

test_that("Subsetting lazy lists works", {
  generator <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
    coro::yield(3)
  })

  x <- lazy_list(generator)

  expect_equal(x[[2]], 2)
  expect_equal(x[[1]], 1)
  expect_equal(length(x), 3)
  expect_equal(x[[4]], NULL)
  expect_equal(x[[3]], 3)
  expect_equal(length(x), 3)

  expect_s3_class(x[1], "lazy_list")
  expect_equal(as.list(x[1]), list(1))
  expect_equal(as.list(x[-2]), list(1, 3))
  expect_equal(as.list(x[1:4]), list(1, 2, 3))
  expect_equal(as.list(x[-2:-3]), list(1))
  expect_equal(as.list(x[-4]), list(1, 2, 3))

  reset_iterator(x)

  expect_equal(as.list(x[2:3]), list(2, 3))

  reset_iterator(x)

  expect_equal(as.list(x[-1]), list(2, 3))
})

test_that("Filtering lazy lists works", {
  generator <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
    coro::yield(3)
  })

  x <- lazy_list(generator)

  expect_s3_class(
    lazy_filter(x, function(x) x %% 2 == 0),
    "lazy_list"
  )

  expect_equal(
    as.list(lazy_filter(x, function(x) x %% 2 == 0)),
    list(2)
  )

  expect_equal(
    as.list(lazy_filter(x, function(x) x %% 2 != 0)),
    list(1, 3)
  )
})

test_that("lazy_map() works", {
  generator <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
    coro::yield(3)
  })

  x <- lazy_list(generator)

  expect_s3_class(
    lazy_map(x, function(x) x + 1),
    "lazy_list"
  )

  expect_equal(
    as.list(lazy_map(x, function(x) x + 1)),
    list(2, 3, 4)
  )
})

test_that("lazy_flatten() works", {
  generator <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
    coro::yield(3)
  })

  x_1 <- lazy_list(generator)

  generator_2 <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
  })

  x_2 <- lazy_list(generator_2)

  final_generator <- coro::generator(function() {
    coro::yield(x_1)
    coro::yield(x_2)
    coro::yield(x_1)
  })

  final_list <- lazy_list(final_generator)

  expect_s3_class(lazy_flatten(final_list), "lazy_list")

  expect_equal(
    as.list(lazy_flatten(final_list)),
    list(1, 2, 3, 1, 2, 1, 2, 3)
  )
})

test_that("combine_lazy_lists() works", {
  generator <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
    coro::yield(3)
  })

  x_1 <- lazy_list(generator)

  generator_2 <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
  })

  x_2 <- lazy_list(generator_2)

  final_list <- list(x_1, x_2, x_1)

  expect_s3_class(combine_lazy_lists(final_list), "lazy_list")

  expect_equal(
    as.list(combine_lazy_lists(final_list)),
    list(1, 2, 3, 1, 2, 1, 2, 3)
  )
})

test_that("lazy_intersect_by() works", {
  generator <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
    coro::yield(3)
    coro::yield(5)
  })

  x_1 <- lazy_list(generator)

  generator_2 <- coro::generator(function() {
    coro::yield(2)
    coro::yield(4)
    coro::yield(5)
  })

  x_2 <- lazy_list(generator_2)

  generator_3 <- coro::generator(function() {
    coro::yield(2)
    coro::yield(5)
  })

  x_3 <- lazy_list(generator_3)

  final_list <- list(x_1, x_2, x_3)

  expect_s3_class(lazy_intersect_by(final_list, `==`), "lazy_list")

  expect_equal(
    as.list(lazy_intersect_by(final_list, `==`)),
    list(2, 5)
  )
})

test_that("lazy_unique() works", {
  generator <- coro::generator(function() {
    coro::yield(1)
    coro::yield(2)
    coro::yield(3)
    coro::yield(5)
  })

  x_1 <- lazy_list(generator)

  generator_2 <- coro::generator(function() {
    coro::yield(2)
    coro::yield(4)
    coro::yield(5)
  })

  x_2 <- lazy_list(generator_2)

  generator_3 <- coro::generator(function() {
    coro::yield(2)
    coro::yield(5)
  })

  x_3 <- lazy_list(generator_3)

  final_list <- list(x_1, x_2, x_3)

  expect_s3_class(lazy_unique(final_list, `==`), "lazy_list")

  expect_equal(
    as.list(lazy_unique(final_list, `==`)),
    list(1, 2, 3, 5, 4)
  )
})
