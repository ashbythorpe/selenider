test_that("actions work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/dev/articles/test-site.html")

  # Button tests

  button <- find_element(s(".actions-test"), "button")

  elem_expect(s("#button-output"), has_exact_text(""))

  elem_hover(button)

  elem_expect(s("#button-output"), has_text("Hovered"))

  elem_click(button)

  elem_expect(s("#button-output"), has_text("Left clicked"))

  elem_right_click(button)

  elem_expect(s("#button-output"), has_text("Right clicked"))

  elem_double_click(button)

  elem_expect(s("#button-output"), has_text("Double clicked"))

  # Input tests

  input <- find_element(s(".actions-test"), "input[type='text']")

  elem_expect(s("#text-output"), has_exact_text(""))

  elem_set_value(input, "my text")

  elem_expect(s("#text-output"), has_text("my text"))

  elem_clear_value(input)

  elem_expect(s("#text-output"), has_exact_text(""))

  elem_set_value(input, "ABA")

  elem_expect(s("#text-output"), has_text("ABA"))

  elem_send_keys(input, "ax", modifiers = "ctrl")

  elem_expect(s("#text-output"), has_text(""))

  elem_send_keys(input, "v", modifiers = "ctrl")

  elem_expect(s("#text-output"), has_text("ABA"))

  # Form tests

  elem_expect(s("#form-output"), has_exact_text(""))

  elem_submit(button)

  elem_expect(s("#form-output"), has_text("Form submitted"))

  output <- s("#selection-output")
  selection <- s("#selection")

  elem_expect(output, has_exact_text(""))

  elem_select(selection, 2)

  elem_expect(output, has_text("2"))

  elem_select(selection, 1)

  elem_expect(output, has_text("1"))

  elem_select(selection, text = "Three")

  elem_expect(output, has_text("3"))

  elem_select(selection, index = 2)

  elem_expect(output, has_text("2"))

  elem_select(find_element(selection, "option"))

  elem_expect(output, has_text("1"))

  elem_select(selection)

  elem_expect(output, has_exact_text(""))

  output <- s("#multiple-selection-output")
  selection <- s("#multiple-selection")

  elem_expect(output, has_exact_text(""))

  elem_select(selection, 2)

  elem_expect(output, has_text("2"))

  elem_select(selection, 1, reset_other = FALSE)

  elem_expect(output, has_text("3"))

  elem_select(selection, c(2, 3))

  elem_expect(output, has_text("5"))

  elem_select(selection, c(1, 2), reset_other = FALSE)

  elem_expect(output, has_text("6"))

  elem_select(selection, text = c("One", "Three"))

  elem_expect(output, has_text("4"))

  elem_select(selection, text = "Two", reset_other = FALSE)

  elem_expect(output, has_text("6"))

  elem_select(selection, 1)

  elem_select(selection, text = c("Two", "Three"), reset_other = FALSE)

  elem_expect(output, has_text("6"))

  elem_select(selection, index = c(1, 2))

  elem_expect(output, has_text("3"))

  elem_select(selection, index = 3, reset_other = FALSE)

  elem_expect(output, has_text("6"))

  elem_select(selection, 1)

  elem_select(selection, index = c(1, 2), reset_other = FALSE)

  elem_expect(output, has_text("3"))

  elem_select(find_elements(selection, "option")[-1])

  elem_expect(output, has_text("5"))

  elem_select(find_element(selection, "option"), reset_other = FALSE)

  elem_expect(output, has_text("6"))

  elem_select(selection, 1)

  elem_select(find_elements(selection, "option")[-1], reset_other = FALSE)

  elem_expect(output, has_text("6"))

  elem_select(selection)

  elem_expect(output, has_text("0"))
})

test_that("get_element_for_selection() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/dev/articles/test-site.html")

  selection <- s("#selection")
  option <- find_element(selection, "option")

  expect_snapshot(
    elem_select(selection, reset_other = FALSE, timeout = 0.1),
    error = TRUE
  )

  expect_snapshot(
    elem_select(option, 1, timeout = 0.1),
    error = TRUE
  )

  expect_snapshot(
    elem_select(selection, 4, timeout = 0.1),
    error = TRUE
  )

  expect_snapshot(
    elem_select(selection, c(1, 2), timeout = 0.1),
    error = TRUE
  )

  expect_snapshot(
    elem_select(selection, text = "Four", timeout = 0.1),
    error = TRUE
  )

  expect_snapshot(
    elem_select(selection, text = c("One", "Three"), timeout = 0.1),
    error = TRUE
  )

  expect_snapshot(
    elem_select(selection, index = 4, timeout = 0.1),
    error = TRUE
  )

  expect_snapshot(
    elem_select(selection, index = c(1, 2), timeout = 0.1),
    error = TRUE
  )
})

test_that("JavaScript actions work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  # Button tests

  button <- find_element(s(".actions-test"), "button")

  elem_expect(s("#button-output"), has_exact_text(""))

  elem_hover(button, js = TRUE)

  elem_expect(s("#button-output"), has_text("Hovered"))

  elem_click(button, js = TRUE)

  elem_expect(s("#button-output"), has_text("Left clicked"))

  elem_right_click(button, js = TRUE)

  elem_expect(s("#button-output"), has_text("Right clicked"))

  elem_double_click(button, js = TRUE)

  elem_expect(s("#button-output"), has_text("Double clicked"))

  elem_expect(s("#form-output"), has_exact_text(""))

  elem_submit(button, js = TRUE)

  if (!isTRUE(as.logical(Sys.getenv("SELENIDER_DOCKER")))) {
    elem_expect(s("#form-output"), has_text("Form submitted"))
  }
})

test_that("elem_scroll_to() works", {
  if (on_ci() && Sys.getenv("SELENIDER_SESSION") == "selenium") {
    skip("Selenium scroll tests don't work on Github Actions")
  }

  session <- selenider_test_session()

  html <- "
  <!DOCTYPE html>
  <div style = 'height:100%; min-height:100vh'></div>
  <button onclick='checkScrolled()'>Click me to check if scrolled</button>
  <p>Scroll down to find me!</p>
  <script>
  function checkScrolled() {
    let element = document.getElementsByTagName('p').item(0);
    let rect = element.getBoundingClientRect();
    // If paragraph is in view
    if (rect.top <= (window.innerHeight || document.documentElement.clientHeight)) {
      element.innerText = 'You found me!';
    }
  }
  </script>
  "

  open_url(paste0("data:text/html,", URLencode(html)))

  elem_scroll_to(s("p"))

  elem_click(s("button"))

  elem_expect(s("p"), has_text("You found me!"))

  file <- withr::local_tempfile(fileext = ".html")
  writeLines(html, file(file))

  open_url(paste0("file://", file))

  elem_scroll_to(s("p"), js = TRUE)

  elem_click(s("button"))

  elem_expect(s("p"), has_text("You found me!"))
})
