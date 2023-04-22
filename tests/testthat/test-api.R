
session <- selenider_session("firefox")

set_session(session)
s("#g") |>
  html_element("#y")

open_url("https://google.com")
