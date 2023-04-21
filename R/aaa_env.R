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

set_session <- function(session) {
  set_in_env(session = session)
  
  session
}

get_session <- function() {
  get_from_env("session")
}
