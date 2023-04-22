
session <- selenider_session("firefox")

set_session(session)
s("#g") |>
  html_element("#h") |>
  html_elements("#i")
