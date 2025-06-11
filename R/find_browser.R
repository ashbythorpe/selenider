# Tries to find a browser that we can use
find_browser <- function() { # nolint: cyclocomp_linter
  if (is_mac()) {
    path <- "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"

    if (file.exists(path)) {
      return("chrome")
    } else {
      find_browser_linux()
    }
  } else if (is_windows()) {
    find_browser_windows()
  } else if (is_linux()) {
    find_browser_linux()
  }
}

find_browser_linux <- function() {
  possible_names <- c(
    "google-chrome",
    "google-chrome-stable",
    "chromium-browser",
    "chromium",
    "google-chrome-beta",
    "google-chrome-unstable",
    "chrome"
  )

  for (path in possible_names) {
    path <- Sys.which(path)
    if (nzchar(path)) {
      return("chrome")
    }
  }

  path <- Sys.which("firefox")

  if (nzchar(path)) {
    "firefox"
  } else {
    NULL
  }
}

find_browser_windows <- function() {
  path <- tryCatch(
    {
      path <- utils::readRegistry(
        "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\"
      )
      path[["(Default)"]]
    },
    error = function(e) {
      NULL
    }
  )

  if (!is.null(path)) {
    return("chrome")
  }

  path <- tryCatch(
    {
      path <- utils::readRegistry(
        "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\firefox.exe\\"
      )
      path[["(Default)"]]
    },
    error = function(e) {
      NULL
    }
  )

  if (!is.null(path)) {
    return("firefox")
  }

  batch_file <-
    "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\iexplore.exe\\"

  path <- tryCatch(
    {
      path <- utils::readRegistry(
        batch_file
      )
      path[["(Default)"]]
    },
    error = function(e) {
      NULL
    }
  )

  if (!is.null(path)) {
    return("internet explorer")
  } else {
    NULL
  }
}
