s <- function(css = NULL,
              xpath = NULL,
              id = NULL,
              class_name = NULL,
              name = NULL,
              link_text = NULL) {
  session <- get_session()
  
  html_element(session, css, xpath, id, class_name, name, link_text)
}

ss <- function(css = NULL,
               xpath = NULL,
               id = NULL,
               class_name = NULL,
               name = NULL,
               link_text = NULL) {
  session <- get_session()
  
  html_elements(session, css, xpath, id, class_name, name, link_text)
}

