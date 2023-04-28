
session <- selenider_session("firefox")
session

set_session(session)

s("#g") |>
  html_element("#h") |>
  html_elements("#i")

rlang::as_data_pronoun(list(x = 1))

rlang::eval_tidy(rlang::quo(x + 1), list(x = 1))

