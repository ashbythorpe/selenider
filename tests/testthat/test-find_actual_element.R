test_that("selector_to_css() works", {
  expect_equal(selector_to_css("css", "selector"), "selector")
  expect_equal(selector_to_css("id", "id"), "#id")
  expect_equal(selector_to_css("class_name", "class"), ".class")
  expect_equal(selector_to_css("name", "name"), "[name = 'name']")
})
