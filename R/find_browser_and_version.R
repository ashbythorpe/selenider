# No idea if this works
find_browser_and_version <- function() { # nolint: cyclocomp_linter
  browser <- NULL
  version <- NULL

  if (is_mac()) {
    path <- "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"

    if (file.exists(path)) {
      version <- tryCatch(
        processx::run(path, "--product-version", timeout = 10)$stdout,
        error = function(e) NULL
      )

      # The version of chromedriver depends on the chrome version
      return(list(
        browser = "chrome",
        version = selenium_version(version, "chrome")
      ))
    }

    path <- Sys.which("firefox")

    if (nchar(path) != 0) {
      return(list(
        browser = "firefox",
        version = "latest"
      ))
    }
  } else if (is_windows()) {
    path <- tryCatch({
      path <- utils::readRegistry(
        "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\"
      )
      path[["(Default)"]]
    }, error = function(e) {
      NULL
    })

    if (!is.null(path)) {
      batch_file <- system.file(
        "scripts/get_chrome_version.bat",
        package = "selenider"
      )
      version <- tryCatch(shell.exec(batch_file), error = function(e) NULL)

      return(list(
        browser = "chrome",
        version = selenium_version(version, "chrome")
      ))
    }

    path <- tryCatch({
      path <- utils::readRegistry(
        "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\firefox.exe\\"
      )
      path[["(Default)"]]
    }, error = function(e) {
      NULL
    })

    if (!is.null(path)) {
      return(list(
        browser = "firefox",
        version = "latest"
      ))
    }

    batch_file <-
      "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\iexplore.exe\\"

    path <- tryCatch({
      path <- utils::readRegistry(
        batch_file
      )
      path[["(Default)"]]
    }, error = function(e) {
      NULL
    })

    if (!is.null(path)) {
      return(list(
        browser = "internet explorer",
        version = "latest"
      ))
    }
  } else if (is_linux()) {
    possible_names <- c(
      "google-chrome",
      "google-chrome-stable",
      "chromium-browser",
      "chromium",
      "google-chrome-beta",
      "google-chrome-unstable"
    )

    for (path in possible_names) {
      path <- Sys.which(path)
      if (nzchar(path)) {
        version <- tryCatch(
          processx::run(path, "--product-version")$stdout,
          error = function(e) NULL
        )

        return(list(
          browser = "chrome",
          version = selenium_version(version, "chrome")
        ))
      }
    }

    path <- Sys.which("firefox")

    if (nzchar(path)) {
      return(list(
        browser = "firefox",
        version = "latest"
      ))
    }
  }

  NULL
}

get_browser_version <- function(x) {
  if (x != "chrome") {
    return("latest")
  }

  if (is_mac()) {
    path <- "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"

    if (file.exists(path)) {
      version <- tryCatch(
        processx::run(path, "--product-version", timeout = 10)$stdout,
        error = function(e) NULL
      )

      return(selenium_version(version, "chrome"))
    }
  } else if (is_windows()) {
    path <- tryCatch({
      path <- utils::readRegistry(
        "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\"
      )
      path[["(Default)"]]
    }, error = function(e) {
      NULL
    })

    if (!is.null(path)) {
      batch_file <- system.file(
        "scripts/get_chrome_version.bat",
        package = "selenider"
      )
      version <- tryCatch(
        shell.exec(batch_file),
        error = function(e) NULL
      )

      return(selenium_version(version, "chrome"))
    }
  } else if (is_linux()) {
    possible_names <- c(
      "google-chrome",
      "google-chrome-stable",
      "chromium-browser",
      "chromium",
      "google-chrome-beta",
      "google-chrome-unstable"
    )

    for (path in possible_names) {
      path <- Sys.which(path)
      if (nzchar(path)) {
        version <- tryCatch(
          processx::run(path, "--product-version")$stdout,
          error = function(e) NULL
        )
        return(selenium_version(version, "chrome"))
      }
    }
  }

  "latest"
}

selenium_version <- function(x, browser) {
  if (is.null(x)) {
    return("latest")
  }

  # Very simple version matching for now: will only match numeric versions
  version <- regmatches(x, regexpr("[0-9]+(?:\\.[0-9]+)+", x))[[1]][1]

  if (length(version) == 0) {
    return("latest")
  }

  version
}
