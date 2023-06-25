key <- function(x) {
  class(x) <- "selenider_key"
  x
}

#' @export
print.selenider_key <- function(x, ...) {
  cli::cli_text("{.key {x}}")
  invisible(x)
}

#' Special keys
#' 
#' List of special keys, for use with [send_keys()].
#'
#' @examples
#' keys$backspace
#'
#' @export
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
  delete = key("DELETE"),
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

get_selenider_key <- function(x) {
  switch(
    x,
    BACKSPACE = RSelenium::selKeys$backspace,
    TAB = RSelenium::selKeys$tab,
    RETURN = RSelenium::selKeys$return,
    ENTER = RSelenium::selKeys$enter,
    SHIFT = RSelenium::selKeys$shift,
    CTRL = RSelenium::selKeys$control,
    ALT = RSelenium::selKeys$alt,
    ESC = RSelenium::selKeys$esc,
    SPACE = RSelenium::selKeys$space,
    UP = RSelenium::selKeys$up,
    DOWN = RSelenium::selKeys$down,
    LEFT = RSelenium::selKeys$left,
    RIGHT = RSelenium::selKeys$right,
    INSERT = RSelenium::selKeys$insert,
    F1 = RSelenium::selKeys$f1,
    F2 = RSelenium::selKeys$f2,
    F3 = RSelenium::selKeys$f3,
    F4 = RSelenium::selKeys$f4,
    F5 = RSelenium::selKeys$f5,
    F6 = RSelenium::selKeys$f6,
    F7 = RSelenium::selKeys$f7,
    F8 = RSelenium::selKeys$f8,
    F9 = RSelenium::selKeys$f9,
    F10 = RSelenium::selKeys$f10,
    F11 = RSelenium::selKeys$f11,
    F12 = RSelenium::selKeys$f12,
    COMMAND = RSelenium::selKeys$command_meta,
    META = RSelenium::selKeys$command_meta
  )
}
