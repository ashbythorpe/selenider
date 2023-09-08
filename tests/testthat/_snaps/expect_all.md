# html_expect_all() works

    Code
      html_expect_all(buttons, is_enabled, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect_all()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_enabled`
      i `x[[2]]` is not enabled.

---

    Code
      html_expect_all(buttons, is_disabled, testthat = FALSE, timeout = 0.1)
    Condition
      Error in `html_expect_all()`:
      ! Condition failed after waiting for 0.1 seconds:
      `is_disabled`
      i `x[[1]]` is enabled.

