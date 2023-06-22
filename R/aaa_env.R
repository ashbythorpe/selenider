selenider <- rlang::new_environment()

get_selenider_env <- function() {
  utils::getFromNamespace("selenider", ns = "selenider")
}

get_from_env <- function(items) {
  sel_env <- get_selenider_env()
  rlang::env_get(sel_env, items, default = NULL)
}

set_in_env <- function(...) {
  sel_env <- get_selenider_env()
  rlang::env_bind(sel_env, ...)
}

set_session <- function(session) {
  old_session <- get_session()
  
  set_in_env(session = session)
  
  old_session
}

reset_session <- function(old_session, close) {
  if (close) {
    rlang::try_fetch(
      close_session(get_session()),
      error = function(e) {
        set_in_env(session = old_session)
        rlang::zap() # Throw error but reset session object first
      }
    )
  }

  set_in_env(session = old_session)
}

#' Get or set the local selenider session
#' 
#' @description 
#' Change the locally defined [selenider_session()] object, allowing it to be
#' used in functions like [s()] without explicitly providing it.
#' 
#' `get_session()` retrieves the current local session, or `NULL` if none have
#' been created.
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
#' @param ... Not used
#' 
#' @details 
#' Use [withr::deferred_run()] to reset any local sessions set using
#' `local_session()`.
#' 
#' @returns 
#' `get_session()` returns the local [selenider_session()] object, or `NULL` if
#' none have been set.
#' 
#' `local_session()` returns the *previous* local session object (or `NULL`). 
#' This is the same as running `get_session()` before this function.
#' 
#' `with_session()` returns the result of `code`.
#' 
#' @seealso 
#' `local_session()` is called by [selenider_session()] unless otherwise
#' specified.
#' 
#' @examples 
#' # Don't set the local session, since we want to do it manually.
#' session_1 <- mock_selenider_session(local = FALSE)
#' session_2 <- mock_selenider_session(local = FALSE)
#' 
#' get_session() # NULL
#' 
#' local_session(session_1)
#' 
#' get_session()
#' 
#' withr::deferred_run()
#' 
#' get_session() # NULL
#' 
#' # By default, the local session is only set inside the function that it is
#' # called.
#' # If we want to set the local session outside the scope of a function, we
#' # need to use the `.local_envir` argument.
#' set_my_session <- function(env = rlang::caller_env()) {
#'   # caller_env() is the environment where the function is called.
#'   local_session(session_1, .local_envir = env)
#' }
#' 
#' set_my_session()
#' 
#' with_session(
#'   session_2,
#'   {get_session()}
#' ) # session_2
#' 
#' get_session() # session_1
#' 
#' @export
get_session <- function(...) {
  get_from_env("session")
}

#' @rdname get_session
#' 
#' @export
local_session <- function(session,
                          .local_envir = rlang::caller_env(),
                          close = TRUE) {
  old <- get_session(session = session)
  withr::defer(reset_session(old, close), envir = .local_envir)
  set_session(session = session)
  invisible(old)
}

#' @rdname get_session
#' 
#' @export
with_session <- function(session, code, close = TRUE) {
  old <- get_session(session = session)
  on.exit(reset_session(old))
  set_session(session = session)
  
  if (close) {
    on.exit(close_session(session))
  }
  
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
    get_session()$timeout
  }
}
