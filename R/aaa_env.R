selenider <- rlang::new_environment()

get_selenider_env <- function() {
  utils::getFromNamespace("selenider", ns = "selenider")
}

get_from_env <- function(items) {
  sel_env <- get_selenider_env()
  env_get(sel_env, items, default = NULL)
}

set_in_env <- function(...) {
  sel_env <- get_selenider_env()
  env_bind(sel_env, ...)
}

set_session <- function(session) {
  old_session <- get_session(create = FALSE)

  set_in_env(session = session)

  old_session
}

reset_session <- function(session, old_session, close) {
  set_in_env(session = old_session)

  if (close) {
    close_session(session)
  }
}

#' Get or set the local selenider session
#'
#' @description
#' Change the locally defined [selenider_session()] object, allowing it to be
#' used in functions like [s()] without explicitly providing it.
#'
#' `get_session()` retrieves the current local session. If none have been
#' created, a session is created automatically.
#'
#' `local_session()` sets the local session. The function uses [withr::defer()]
#' to make sure the session is closed and the local session is set to its
#' previous value when it is no longer needed.
#'
#' `with_session()` runs some code with a temporary local session. The session
#' is closed and the local session is set to its previous value when the code
#' finishes executing.
#'
#' @param session The [selenider_session()] object to use.
#' @param code The code to run with the local session set.
#' @param .local_envir The environment where the session is being used. When
#'   the function associated with this environment finishes execution, the
#'   session will be reset.
#' @param close Should we close `session` when the local session is reset? Set
#'   this to `FALSE` if you want to use the session even if it is no longer the
#'   local session. If you want to close the session manually, use
#'   [close_session()].
#' @param create If a session is not found, should we create a new one? If this
#'   is `FALSE` and a session is not found, `NULL` is returned.
#' @param .env If `get_session()` creates a session, the environment where this
#'   session is being used.
#'
#' @details
#' Use [withr::deferred_run()] to reset any local sessions set using
#' `local_session()`.
#'
#' @returns
#' `get_session()` returns the local [selenider_session()] object (or a newly
#' created session).
#'
#' `local_session()` returns the *previous* local session object (or `NULL`).
#' This is the same as running `get_session()` before this function.
#'
#' `with_session()` returns the result of `code`.
#'
#' @seealso
#' [selenider_session()], which calls `local_session()` unless otherwise
#' specified.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' # Don't set the local session, since we want to do it manually.
#' session <- selenider_session(local = FALSE)
#'
#' get_session(create = FALSE) # NULL
#'
#' local_session(session, close = FALSE)
#'
#' get_session(create = FALSE)
#'
#' withr::deferred_run()
#'
#' get_session(create = FALSE) # NULL
#'
#' # By default, the local session is only set inside the function that it is
#' # called.
#' # If we want to set the local session outside the scope of a function, we
#' # need to use the `.local_envir` argument.
#' set_my_session <- function(env = rlang::caller_env()) {
#'   # caller_env() is the environment where the function is called.
#'   local_session(session, .local_envir = env, close = FALSE)
#' }
#'
#' set_my_session()
#'
#' with_session(
#'   session,
#'   {
#'     get_session(create = FALSE)
#'   },
#'   close = FALSE
#' )
#'
#' get_session(create = FALSE)
#'
#' @export
get_session <- function(create = TRUE, .env = rlang::caller_env()) {
  check_bool(create)
  check_environment(.env)
  session <- get_from_env("session")

  if (is.null(session) && create) {
    cli::cli_inform(c(
      "Can't find an existing selenider session.",
      "i" = "Creating a new session."
    ))

    selenider_session(.env = .env)
  } else {
    session
  }
}

#' @rdname get_session
#'
#' @export
local_session <- function(session,
                          .local_envir = rlang::caller_env(),
                          close = TRUE) {
  check_class(session, "selenider_session")
  check_environment(.local_envir)
  check_bool(close)

  old <- get_session(create = FALSE)
  withr::with_options(list(withr.hook_source = TRUE), {
    withr::defer(reset_session(session, old, close), envir = .local_envir)
  })
  set_session(session = session)

  invisible(old)
}

#' @rdname get_session
#'
#' @export
with_session <- function(session, code, close = TRUE) {
  check_class(session, "selenider_session")
  check_bool(close)

  old <- get_session(create = FALSE)
  set_session(session = session)
  on.exit(reset_session(session, old, close))

  force(code)
}

set_timeout <- function(timeout) {
  set_in_env(timeout = timeout)
}

get_local_timeout <- function(...) {
  get_from_env("timeout")
}

reset_timeout <- function(old_timeout) {
  set_in_env(timeout = old_timeout)
}

with_timeout <- function(new, code) {
  old <- get_local_timeout(timeout = new)
  on.exit(reset_timeout(old))
  set_timeout(timeout = new)
  force(code)
}

get_timeout <- function(user_timeout, element_timeout) {
  if (!is.null(user_timeout)) {
    return(user_timeout)
  }

  local_timeout <- get_local_timeout()

  if (!is.null(local_timeout)) {
    local_timeout
  } else if (!is.null(element_timeout)) {
    element_timeout
  } else {
    timeout <- get_session(create = FALSE)$timeout

    if (!is.null(timeout)) timeout else 4
  }
}
