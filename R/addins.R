#' Simple add-in which lets you keyboard map the writing of pipe + newline. Intended to make functional programming pipelines a little easier on the hands.
quick_pipe <- function() {
  if('magrittr' %in% loadedNamespaces()){
    rstudioapi::insertText(" %>% \n")
    rstudioapi::executeCommand("reindent")
  }else{
    rstudioapi::insertText(" |> \n")
    rstudioapi::executeCommand("reindent")
  }
}
