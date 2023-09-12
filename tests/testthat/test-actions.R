test_that("actions work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  # Button tests

  button <- html_element(s(".actions-test"), "button")

  html_expect(s("#button-output"), has_exact_text(""))

  hover(button)

  html_expect(s("#button-output"), has_text("Hovered"))

  click(button)

  html_expect(s("#button-output"), has_text("Left clicked"))

  right_click(button)

  html_expect(s("#button-output"), has_text("Right clicked"))

  double_click(button)

  html_expect(s("#button-output"), has_text("Double clicked"))

  # Input tests

  input <- html_element(s(".actions-test"), "input[type='text']")

  html_expect(s("#text-output"), has_exact_text(""))

  set_value(input, "my text")

  html_expect(s("#text-output"), has_text("my text"))

  clear_value(input)

  html_expect(s("#text-output"), has_exact_text(""))

  set_value(input, "ABA")

  html_expect(s("#text-output"), has_text("ABA"))

  send_keys(input, "ax", modifiers = "ctrl")

  html_expect(s("#text-output"), has_text(""))

  send_keys(input, "v", modifiers = "ctrl")

  html_expect(s("#text-output"), has_text("ABA"))

  # Form tests

  html_expect(s("#form-output"), has_exact_text(""))

  submit(button)

  html_expect(s("#form-output"), has_text("Form submitted"))
})

test_that("JavaScript actions work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  # Button tests

  button <- html_element(s(".actions-test"), "button")

  html_expect(s("#button-output"), has_exact_text(""))

  hover(button, js = TRUE)

  html_expect(s("#button-output"), has_text("Hovered"))

  click(button, js = TRUE)

  html_expect(s("#button-output"), has_text("Left clicked"))

  right_click(button, js = TRUE)

  html_expect(s("#button-output"), has_text("Right clicked"))

  double_click(button, js = TRUE)

  html_expect(s("#button-output"), has_text("Double clicked"))

  html_expect(s("#form-output"), has_exact_text(""))

  submit(button, js = TRUE)

  html_expect(s("#form-output"), has_text("Form submitted"))
})

test_that("scroll_to() works", {
  session <- selenider_test_session()

  html <- "
  <!DOCTYPE html>
  <div style = 'height:100%; min-height:100vh'></div>
  <button onclick='checkScrolled()'>Click to check if scrolled</button>
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

  scroll_to(s("p"))

  click(s("button"))

  html_expect(s("p"), has_text("You found me!"))

  file <- withr::local_tempfile(fileext = ".html")
  writeLines(html, file(file))

  open_url(paste0("file://", file))

  scroll_to(s("p"), js = TRUE)

  click(s("button"))

  html_expect(s("p"), has_text("You found me!"))
})
