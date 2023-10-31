test_that("Printing session works", {
  session <- selenider_test_session()

  # So the snapshot always works
  session$session <- "chromote"

  expect_snapshot(print(session, .time = as.difftime(10, units = "secs")))
})
