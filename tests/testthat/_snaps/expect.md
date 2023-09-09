# html_expect() works

    Code
      html_expect(is_present, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_present`
      i The condition returned a function instead of TRUE.
      i Did you forget to supply `x`?
      x Instead of `html_expect(is_present)`
      v Use: `html_expect(element, is_present)`

---

    Code
      html_expect(s(".toggleable"))
    Condition
      Error in `eval_conditions()`:
      ! No conditions were specified.
      i Try specifying a condition.
      x Instead of: `html_expect(element)`
      v Try: `html_expect(element, is_present)`

---

    Code
      html_expect(s(".random-class"), is_present, testthat = FALSE, timeout = 0)
    Condition
      Error in `html_expect()`:
      ! Condition failed:
      `is_present`
      i `x` is not present.

---

    Code
      html_expect(s(".random-class"), is_present, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_present`
      i `x` is not present.

---

    Code
      html_expect(s(".toggleable"), !is_in_dom, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `!is_in_dom`
      i `x` is in the DOM.

---

    Code
      html_expect(s(".random-class"), !is_absent, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `!is_absent`
      i `x` is absent.

---

    Code
      html_expect(s(".toggleable"), !(!(is_absent)), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `!(!(is_absent))`
      i `x` is in the DOM.

---

    Code
      html_expect(s(".random-class"), !(!(!(is_absent))), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `!(!(!(is_absent)))`
      i `x` is absent.

---

    Code
      html_expect(s(".random-class"), is_visible, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_visible`
      i `x` is not visible.
      Caused by error in `is_visible()`:
      ! `x` does not exist in the DOM.

---

    Code
      html_expect(s(".toggleable"), is_visible, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_visible`
      i `x` is not visible.

---

    Code
      html_expect(s(".buttons"), is_hidden, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_hidden`
      i `x` is displayed.

---

    Code
      html_expect(enabled_button, is_disabled, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_disabled`
      i `x` is enabled.

---

    Code
      html_expect(disabled_button, is_enabled, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_enabled`
      i `x` is not enabled.

---

    Code
      html_expect(s(".random-class"), has_name("p"), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_name("p")`
      i `x` does not have tag name "p".
      Caused by error in `has_name()`:
      ! `x` does not exist in the DOM.

---

    Code
      html_expect(s(".toggleable"), has_name("biv"), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_name("biv")`
      i `x` does not have tag name "biv".
      i Actual tag name: "div".

---

    Code
      html_expect(element, has_text("Goodbye!"), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_text("Goodbye!")`
      i `x` does not have text "Goodbye!".
      i Actual text: "Hello!".

---

    Code
      html_expect(element, has_exact_text("ell"), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_exact_text("ell")`
      i `x` does not have exact text "ell".
      i Actual text: "Hello!".

---

    Code
      html_expect(buttons[[1]], has_attr("disabled", ""), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_attr("disabled", "")`
      i `x`'s "disabled" attribute is not "".
      i Actual value: `NA`.

---

    Code
      html_expect(s(".toggleable"), attr_contains("style", "color"), testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `attr_contains("style", "color")`
      i `x`'s "style" attribute does not contain "color".
      i Actual value: "display: none;".

---

    Code
      html_expect(submit_button, has_value("Don't submit"), testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_value("Don't submit")`
      i `x` does not have value "Don't submit".
      i Actual value: "Submit".

---

    Code
      html_expect(s(".toggleable"), has_css_property("display", "block"), testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_css_property("display", "block")`
      i `x`'s "display" CSS property is not "block".
      i Actual value: "none".

---

    Code
      html_expect(s(".random-class"), function(x) html_name(x) == "biv", testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) html_name(x) == "biv"`
      i `x` does not exist, which may have caused the condition to fail.
      Caused by error in `html_name()`:
      ! To get the tag name of `x`, it must exist.
      i `x` was not present.

---

    Code
      html_expect(s(".toggleable"), function(x) html_name(x) == "biv", testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) html_name(x) == "biv"`
      i `x` exists, but the condition still failed.

---

    Code
      html_expect(html_children(s(".random-class")), function(x) html_name(x[[1]]) ==
        "biv", testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) html_name(x[[1]]) == "biv"`
      i `x`'s parent element does not exist, which may have caused the condition to fail.
      Caused by error in `html_name()`:
      ! To get the tag name of `x`, it must exist.
      i `x` was not present.

---

    Code
      html_expect(ss(".random-class"), function(x) html_name(x[[1]]) == "biv",
      testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) html_name(x[[1]]) == "biv"`
      i `x` contains no elements, which may have caused the condition to fail.
      Caused by error in `html_name()`:
      ! To get the tag name of `x`, it must exist.
      i `x` was not present.

---

    Code
      html_expect(ss(".toggleable"), function(x) html_name(x[[1]]) == "biv",
      testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) html_name(x[[1]]) == "biv"`
      i `x` contains 1 element, but the condition still failed.

---

    Code
      html_expect(ss(".random-class"), has_at_least(1), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_at_least(1)`
      i `x` contains less than 1 element.
      i Actual number of elements: "0".

---

    Code
      html_expect(ss(".toggleable"), has_length(2), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_length(2)`
      i `x` does not contain 2 elements.
      i Actual number of elements: "1".

# html_expect() test failures work

    Code
      show_failure(html_expect(s(".random-class"), is_present, timeout = 0.1))
    Output
      Failed expectation:
      Condition failed after waiting for 0.1 seconds:
      `is_present`
      i `x` is not present.
      
      Where `x` is:
      A selenider element selecting:
      The first element with css selector ".random-class".
      
      

---

    Code
      show_failure(html_expect(s(".random-class"), is_visible, timeout = 0.1))
    Output
      Failed expectation:
      Condition failed after waiting for 0.1 seconds:
      `is_visible`
      i `x` is not visible.
      Caused by error in `is_visible()`:
      ! `x` does not exist in the DOM.
      
      Where `x` is:
      A selenider element selecting:
      The first element with css selector ".random-class".
      
      

