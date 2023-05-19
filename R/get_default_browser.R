# No idea if this works
find_browser_and_version <- function() {
  browser <- NULL
  version <- NULL

  if (is_mac()) {
    path <- "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"

    if (file.exists(path)) {
      version <- tryCatch(
        processx::run(path, "--product-version", timeout = 10)$stdout,
        error = function(e) NULL
      )

      return(list(
        browser = "chrome",
        version = selenium_version(version, "chrome")
      ))
    }

    path <- Sys.which("firefox")

    if (nchar(path) != 0) {
      version <- tryCatch(
        processx::run(path, "--version")$stdout,
        error = function(e) {
          tryCatch(
            processx::run(path, "-v")$stdout,
            error = function(e) NULL
          )
        }
      )

      return(list(
        browser = "firefox",
        version = selenium_version(version, "firefox")
      ))
    }
  } else if (is_windows()) {
    path <- tryCatch({
      path <- utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\")
      path[["(Default)"]]
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(path)) {
      batch_file <- system.file("scripts/get_chrome_version.bat", package = "selenider")
      version <- tryCatch(shell.exec(batch_file), error = function(e) NULL)
      
      return(list(
        browser = "chrome",
        version = selenium_version(version, "chrome")
      ))
    }

    path <- tryCatch({
      path <- utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\firefox.exe\\")
      path[["(Default)"]]
    }, error = function(e) {
      NULL
    })

    if (!is.null(path)) {
      version <- tryCatch(
        processx::run(path, "--version", timeout = 10)$stdout,
        error = function(e) NULL
      )

      return(list(
        browser = "firefox",
        version = selenium_version(version, "firefox")
      ))
    }

    path <- tryCatch({
      path <- utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\iexplore.exe\\")
      path[["(Default)"]]
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(path)) {
      batch_file <- system.file("scripts/get_ie_version.bat", package = "selenider")
      version <- tryCatch(shell.exec(batch_file), error = function(e) NULL)
      
      return(list(
        browser = "internet explorer",
        version = selenium_version(version, "internet explorer")
      ))
    }
  } else if (is_linux()) {
    path <- Sys.which("google-chrome")
    if (nchar(path) == 0) {
      path <- Sys.which("chromium-browser")
    }
    if (nchar(path) == 0) {
      path <- Sys.which("chromium")
    }

    if (nchar(path) != 0) {
      version <- tryCatch(
        processx::run(path, "--product-version")$stdout,
        error = function(e) NULL
      )

      return(list(
        browser = "chrome",
        version = selenium_version(version, "chrome")
      ))
    }

    path <- Sys.which("firefox")

    if (nchar(path) != 0) {
      version <- tryCatch(
        processx::run(path, "--version")$stdout,
        error = function(e) {
          tryCatch(
            processx::run(path, "-v")$stdout,
            error = function(e) NULL
          )
        }
      )

      return(list(
        browser = "firefox",
        version = selenium_version(version, "firefox")
      ))
    }
  }
  
  NULL
}

get_browser_version <- function(x) {
  if (is_mac()) {
    if (x == "chrome") {
      path <- "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"

      if (file.exists(path)) {
        version <- tryCatch(
          processx::run(path, "--product-version", timeout = 10)$stdout,
          error = function(e) NULL
        )

        return(selenium_version(version, "chrome"))
      }
    } else if (x == "firefox") {
      path <- Sys.which("firefox")

      if (nchar(path) != 0) {
        version <- tryCatch(
          processx::run(path, "--version")$stdout,
          error = function(e) {
            tryCatch(
              processx::run(path, "-v")$stdout,
              error = function(e) NULL
            )
          }
        )

        return(selenium_version(version, "firefox"))
      }
    }
  } else if (is_windows()) {
    if (x == "chrome") {
      path <- tryCatch({
        path <- utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\")
        path[["(Default)"]]
      }, error = function(e) {
        NULL
      })
    
      if (!is.null(path)) {
        batch_file <- system.file("scripts/get_chrome_version.bat", package = "selenider")
        version <- tryCatch(shell.exec(batch_file), error = function(e) NULL)
        
        return(selenium_version(version, "chrome"))
      }
    } else if (x == "firefox") {
      path <- tryCatch({
        path <- utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\firefox.exe\\")
        path[["(Default)"]]
      }, error = function(e) {
        NULL
      })

      if (!is.null(path)) {
        version <- tryCatch(
          processx::run(path, "--version", timeout = 10)$stdout,
          error = function(e) NULL
        )

        return(selenium_version(version, "firefox"))
      }
    } else {
      path <- tryCatch({
        path <- utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\iexplore.exe\\")
        path[["(Default)"]]
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(path)) {
        batch_file <- system.file("scripts/get_ie_version.bat", package = "selenider")
        version <- tryCatch(shell.exec(batch_file), error = function(e) NULL)
        
        return(selenium_version(version, "internet explorer"))
      }
    }
  } else if (is_linux()) {
    if (x == "chrome") {
      path <- Sys.which("google-chrome")
      if (nchar(path) == 0) {
        path <- Sys.which("chromium-browser")
      }
      if (nchar(path) == 0) {
        path <- Sys.which("chromium")
      }

      if (nchar(path) != 0) {
        version <- tryCatch(
          processx::run(path, "--product-version")$stdout,
          error = function(e) NULL
        )

        return(selenium_version(version, "chrome"))
      }
    } else if (x == "firefox") {
      path <- Sys.which("firefox")

      if (nchar(path) != 0) {
        version <- tryCatch(
          processx::run(path, "--version")$stdout,
          error = function(e) {
            tryCatch(
              processx::run(path, "-v")$stdout,
              error = function(e) NULL
            )
          }
        )

        return(selenium_version(version, "firefox"))
      }
    }
  }
  
  "latest"
}

selenium_version <- function(x, browser) {
  if (is.null(x)) {
    return("latest")
  }

  version <- regmatches(x, regexpr("[0-9]+(?:\\.[0-9]+)+", x))[[1]][1]

  if (length(version) == 0) {
    return("latest")
  }

  # TODO: Use rlang::check_installed() if we move RSelenium to Suggests
  driver_name <- switch(browser,
    chrome = "chromedriver",
    firefox = "geckodriver",
    "internet explorer" = "iedriverserver"
  )

  versions <- unlist(binman::list_versions(driver_name))

  if (version %in% versions) {
    version 
  } else {
    cli::cli_warn(c(
      "Version of {tools::toTitleCase(browser)} is not supported.",
      "i" = "Supported versions: {versions}.",
      "x" = "Actual version: {version}",
      "i" = "Using version \"latest\" instead."
    ))

    "latest"
  }
}
