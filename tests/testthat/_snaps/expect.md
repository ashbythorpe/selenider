# elem_expect() works

    Code
      elem_expect(is_present, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_present`
      i The condition returned a function instead of TRUE.
      i Did you forget to supply `x`?
      x Instead of `elem_expect(is_present)`
      v Use: `elem_expect(element, is_present)`

---

    Code
      elem_expect(s(".toggleable"))
    Condition
      Error in `eval_conditions()`:
      ! No conditions were specified.
      i Try specifying a condition.
      x Instead of: `elem_expect(element)`
      v Try: `elem_expect(element, is_present)`

---

    Code
      elem_expect(s(".random-class"), is_present, testthat = FALSE, timeout = 0)
    Condition
      Error in `elem_expect()`:
      ! Condition failed:
      `is_present`
      i `x` is not present.

---

    Code
      elem_expect(s(".random-class"), is_present, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_present`
      i `x` is not present.

---

    Code
      elem_expect(s(".toggleable"), !is_in_dom, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `!is_in_dom`
      i `x` is in the DOM.

---

    Code
      elem_expect(s(".random-class"), !is_absent, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `!is_absent`
      i `x` is absent.

---

    Code
      elem_expect(s(".toggleable"), !(!(is_absent)), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `!(!(is_absent))`
      i `x` is in the DOM.

---

    Code
      elem_expect(s(".random-class"), !(!(!(is_absent))), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `!(!(!(is_absent)))`
      i `x` is absent.

---

    Code
      elem_expect(s(".random-class"), is_visible, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_visible`
      i `x` is not visible.
      Caused by error in `is_visible()`:
      ! `x` does not exist in the DOM.

---

    Code
      elem_expect(s(".toggleable"), is_visible, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_visible`
      i `x` is not visible.

---

    Code
      elem_expect(s(".buttons"), is_hidden, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_hidden`
      i `x` is displayed.

---

    Code
      elem_expect(enabled_button, is_disabled, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_disabled`
      i `x` is enabled.

---

    Code
      elem_expect(disabled_button, is_enabled, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_enabled`
      i `x` is not enabled.

---

    Code
      elem_expect(s(".random-class"), has_name("p"), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_name("p")`
      i `x` does not have tag name "p".
      Caused by error in `has_name()`:
      ! `x` does not exist in the DOM.

---

    Code
      elem_expect(s(".toggleable"), has_name("biv"), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_name("biv")`
      i `x` does not have tag name "biv".
      i Actual tag name: "div".

---

    Code
      elem_expect(element, has_text("Goodbye!"), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_text("Goodbye!")`
      i `x` does not have text "Goodbye!".
      i Actual text: "Hello!".

---

    Code
      elem_expect(element, has_exact_text("ell"), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_exact_text("ell")`
      i `x` does not have exact text "ell".
      i Actual text: "Hello!".

---

    Code
      elem_expect(buttons[[1]], has_attr("disabled", ""), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_attr("disabled", "")`
      i `x`'s "disabled" attribute is not "".
      i Actual value: `NULL`.

---

    Code
      elem_expect(s(".toggleable"), attr_contains("style", "color"), testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `attr_contains("style", "color")`
      i `x`'s "style" attribute does not contain "color".
      i Actual value: "display: none;".

---

    Code
      elem_expect(submit_button, has_value("Don't submit"), testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_value("Don't submit")`
      i `x` does not have value "Don't submit".
      i Actual value: "Submit".

---

    Code
      elem_expect(s(".toggleable"), has_css_property("display", "block"), testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_css_property("display", "block")`
      i `x`'s "display" CSS property is not "block".
      i Actual value: "none".

---

    Code
      elem_expect(s(".random-class"), function(x) elem_name(x) == "biv", testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) elem_name(x) == "biv"`
      i `x` does not exist, which may have caused the condition to fail.
      Caused by error in `elem_name()`:
      ! To get the tag name of `x`, it must exist.
      i `x` was not present.

---

    Code
      elem_expect(s(".toggleable"), function(x) elem_name(x) == "biv", testthat = FALSE,
      timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) elem_name(x) == "biv"`
      i `x` exists, but the condition still failed.

---

    Code
      elem_expect(elem_children(s(".random-class")), function(x) elem_name(x[[1]]) ==
        "biv", testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) elem_name(x[[1]]) == "biv"`
      i `x`'s parent element does not exist, which may have caused the condition to fail.
      Caused by error in `elem_name()`:
      ! To get the tag name of `x`, it must exist.
      i `x` was not present.

---

    Code
      elem_expect(ss(".random-class"), function(x) elem_name(x[[1]]) == "biv",
      testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) elem_name(x[[1]]) == "biv"`
      i `x` contains no elements, which may have caused the condition to fail.
      Caused by error in `elem_name()`:
      ! To get the tag name of `x`, it must exist.
      i `x` was not present.

---

    Code
      elem_expect(ss(".toggleable"), function(x) elem_name(x[[1]]) == "biv",
      testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `function(x) elem_name(x[[1]]) == "biv"`
      i `x` contains 1 element, but the condition still failed.

---

    Code
      elem_expect(ss(".random-class"), has_at_least(1), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_at_least(1)`
      i `x` contains less than 1 element.
      i Actual number of elements: 0.

---

    Code
      elem_expect(ss(".toggleable"), has_length(2), testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect()`:
      ! Condition failed after waiting for 0.1 seconds:
      `has_length(2)`
      i `x` does not contain 2 elements.
      i Actual number of elements: 1.

# elem_expect() test failures work

    Code
      show_failure(elem_expect(s(".random-class"), is_present, timeout = 0.1))
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
      show_failure(elem_expect(s(".random-class"), is_visible, timeout = 0.1))
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
      
      

