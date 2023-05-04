selenider <- rlang::new_environment()

get_selenider_env <- function() {
  current <- utils::getFromNamespace("selenider", ns = "selenider")
  current
}

get_from_env <- function(items) {
  sel_env <- get_selenider_env()
  rlang::env_get(sel_env, items, default = NULL)
}

set_in_env <- function(...) {
  sel_env <- get_selenider_env()
  rlang::env_bind(sel_env, ...)
}

#' Set or get the global selenider session
#' 
#' `get_session()` gets the selenider session that is currently being used.
#' `set_session()` changes this session.
#' 
#' @param session The session to set: a [selenider_session()] object.
#' 
#' @returns 
#' `get_session()` returns the current global `selenider_session` object, or
#'   `NULL` if none have been created.
#' `set_session()` returns the previous global session.
#' 
#' @examples 
#' session <- mock_session(set = FALSE)
#' 
#' get_session() # NULL
#' 
#' set_session(session)
#' 
#' get_session()
#' 
#' @export
get_session <- function() {
  get_from_env("session")
}

#' @rdname get_session
#' 
#' @export
set_session <- function(session) {
  old_session <- get_session()
  
  set_in_env(session = session)
  
  invisible(old_session)
}
