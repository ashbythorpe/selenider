# get_element_for_selection() works

    Code
      elem_select(selection, reset_other = FALSE, timeout = 0.1)
    Condition
      Error in `get_select_element()`:
      ! To select `x`, it must be an `<option>` element, not a `<select>` element (as all other arguments are `NULL` and `reset_other` is `FALSE`).
      i After 0.1 seconds, `x` did not have the correct tag name.

---

    Code
      elem_select(option, 1, timeout = 0.1)
    Condition
      Error in `get_select_element()`:
      ! To select `x`, it must be a `<select>` element, not an `<option>` element (as `value` is not `NULL`).
      i After 0.1 seconds, `x` did not have the correct tag name.

---

    Code
      elem_select(selection, 4, timeout = 0.1)
    Condition
      Error in `get_select_element()`:
      ! To select `x`, it must have an `<option>` element with value "4".
      i After 0.1 seconds, `x` did not have contain the required option.

---

    Code
      elem_select(selection, c(1, 2), timeout = 0.1)
    Condition
      Error in `get_select_element()`:
      ! To select `x`, it must support multiple selections (as `value` contains multiple items).
      i After 0.1 seconds, `x` did not support multiple selections.

---

    Code
      elem_select(selection, text = "Four", timeout = 0.1)
    Condition
      Error in `get_select_element()`:
      ! To select `x`, it must have an `<option>` element with text "Four".
      i After 0.1 seconds, `x` did not have contain the required option.

---

    Code
      elem_select(selection, text = c("One", "Three"), timeout = 0.1)
    Condition
      Error in `get_select_element()`:
      ! To select `x`, it must support multiple selections (as `text` contains multiple items).
      i After 0.1 seconds, `x` did not support multiple selections.

---

    Code
      elem_select(selection, index = 4, timeout = 0.1)
    Condition
      Error in `get_select_element()`:
      ! To select `x`, it must have at least 4 options.
      i After 0.1 seconds, `x` had 3 options.

---

    Code
      elem_select(selection, index = c(1, 2), timeout = 0.1)
    Condition
      Error in `get_select_element()`:
      ! To select `x`, it must support multiple selections (as `index` contains multiple items).
      i After 0.1 seconds, `x` did not support multiple selections.

