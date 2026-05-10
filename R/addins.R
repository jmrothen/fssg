#' Simple add-in which lets you keyboard map pipe + newline
quick_pipe <- function() {
  if('magrittr' %in% loadedNamespaces()){
    rstudioapi::insertText(" %>% \n")
    rstudioapi::executeCommand("reindent")
  }else{
    rstudioapi::insertText(" |> \n")
    rstudioapi::executeCommand("reindent")
  }
}
