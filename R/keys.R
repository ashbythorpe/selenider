#' @export
print.selenider_key <- function(x, ...) {
  cli::cli_text("{.key {x}}")
  invisible(x)
}

#' Special keys
#' 
#' List of special keys, for use with [elem_send_keys()].
#'
#' @format
#' A list containing `selenider_key` objects.
#'
#' @examples
#' keys$backspace
"keys"

get_selenider_key <- function(x) {
  rlang::check_installed("RSelenium")
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
    META = RSelenium::selKeys$command_meta,
    stop("Key not found")
  )
}

# Given a selenider_key, get the description of the key.
get_chromote_key <- function(x) {
  switch(
    x,
    BACKSPACE = list(windowsVirtualKeyCode = 8, code = "Backspace", key = "Backspace"),
    TAB = list(windowsVirtualKeyCode = 9, code = "Tab", key = "Tab"),
    RETURN = list(windowsVirtualKeyCode = 13, code = "Enter", key = "Enter", text = "\r"),
    ENTER = list(windowsVirtualKeyCode = 13, code = "Enter", key = "Enter", text = "\r"),
    SHIFT = list(windowsVirtualKeyCode = 16, code = "ShiftLeft", key = "Shift", location = 1),
    CTRL = list(windowsVirtualKeyCode = 17, code = "ControlLeft", key = "Control", location = 1),
    ALT = list(windowsVirtualKeyCode = 18, code = "AltLeft", key = "Alt", location = 1),
    ESC = list(windowsVirtualKeyCode = 27, code = "Escape", key = "Escape"),
    SPACE = list(windowsVirtualKeyCode = 32, code = "Space", key = " "),
    UP = list(windowsVirtualKeyCode = 38, code = "ArrowUp", key = "ArrowUp"),
    DOWN = list(windowsVirtualKeyCode = 40, code = "ArrowDown", key = "ArrowDown"),
    LEFT = list(windowsVirtualKeyCode = 37, code = "ArrowLeft", key = "ArrowLeft"),
    RIGHT = list(windowsVirtualKeyCode = 39, code = "ArrowRight", key = "ArrowRight"),
    INSERT = list(windowsVirtualKeyCode = 45, code = "Insert", key = "Insert"),
    F1 = list(windowsVirtualKeyCode = 112, code = "F1", key = "F1"),
    F2 = list(windowsVirtualKeyCode = 113, code = "F2", key = "F2"),
    F3 = list(windowsVirtualKeyCode = 114, code = "F3", key = "F3"),
    F4 = list(windowsVirtualKeyCode = 115, code = "F4", key = "F4"),
    F5 = list(windowsVirtualKeyCode = 116, code = "F5", key = "F5"),
    F6 = list(windowsVirtualKeyCode = 117, code = "F6", key = "F6"),
    F7 = list(windowsVirtualKeyCode = 118, code = "F7", key = "F7"),
    F8 = list(windowsVirtualKeyCode = 119, code = "F8", key = "F8"),
    F9 = list(windowsVirtualKeyCode = 120, code = "F9", key = "F9"),
    F10 = list(windowsVirtualKeyCode = 121, code = "F10", key = "F10"),
    F11 = list(windowsVirtualKeyCode = 122, code = "F11", key = "F11"),
    F12 = list(windowsVirtualKeyCode = 123, code = "F12", key = "F12"),
    COMMAND = list(windowsVirtualKeyCode = 91, code = "MetaLeft", key = "Meta", location = 1),
    META = list(windowsVirtualKeyCode = 91, code = "MetaLeft", key = "Meta", location = 1),
    stop("Key not found")
  )
}

# Given a character, identify the description of the key it represents.
identify_chromote_key <- function(x) {
  chromote_key_definitions[[x]]
}
