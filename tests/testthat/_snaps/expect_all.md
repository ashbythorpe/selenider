# elem_expect_all() works

    Code
      elem_expect_all(buttons, is_enabled, testthat = FALSE, timeout = 0)
    Condition
      Error in `elem_expect_all()`:
      ! Condition failed:
      `is_enabled`
      i `x[[2]]` is not enabled.

---

    Code
      elem_expect_all(buttons, is_enabled, is_visible, testthat = FALSE, timeout = 0)
    Condition
      Error in `elem_expect_all()`:
      ! Condition failed:
      `is_enabled`
      i `x[[2]]` is not enabled.

---

    Code
      elem_expect_all(buttons, is_enabled, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect_all()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_enabled`
      i `x[[2]]` is not enabled.

---

    Code
      elem_expect_all(buttons, is_disabled, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect_all()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_disabled`
      i `x[[1]]` is enabled.

---

    Code
      elem_expect_all(buttons, is_visible, is_disabled, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `elem_expect_all()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_visible`
      i `x[[1]]` is not visible.

