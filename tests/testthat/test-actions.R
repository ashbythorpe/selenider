test_that("actions work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

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

  file <- withr::local_tempfile(fileext = ".html")
  writeLines(html, file(file))
  open_url(paste0("file://", file))

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
