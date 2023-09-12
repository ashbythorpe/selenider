## code to prepare `keys` dataset goes here
key <- function(x) {
  class(x) <- "selenider_key"
  x
}

keys <- list(
  backspace = key("BACKSPACE"),
  tab = key("TAB"),
  return = key("RETURN"),
  enter = key("ENTER"),
  shift = key("SHIFT"),
  control = key("CTRL"),
  alt = key("ALT"),
  escape = key("ESC"),
  space = key("SPACE"),
  up = key("UP"),
  down = key("DOWN"),
  left = key("LEFT"),
  right = key("RIGHT"),
  insert = key("INSERT"),
  f1 = key("F1"),
  f2 = key("F2"),
  f3 = key("F3"),
  f4 = key("F4"),
  f5 = key("F5"),
  f6 = key("F6"),
  f7 = key("F7"),
  f8 = key("F8"),
  f9 = key("F9"),
  f10 = key("F10"),
  f11 = key("F11"),
  f12 = key("F12"),
  command = key("COMMAND"),
  meta = key("META")
)

usethis::use_data(keys, overwrite = TRUE)
