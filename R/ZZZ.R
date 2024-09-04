md_file_to_html <- function(...) {
  
  file <- app_sys(...)
  
  if (!file.exists(file)) {
    return(NULL)
  }
  
  shiny::HTML(markdown::mark_html(file, output = FALSE, template = FALSE))
  
}