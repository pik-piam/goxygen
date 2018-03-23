#' check_pandoc
#' 
#' Support function which checks pandoc availability and stops with an error in case
#' that pandoc cannot be found
#' @author Jan Philipp Dietrich

check_pandoc <- function() {
  test <-try(system("pandoc --help",intern = TRUE, ignore.stderr = TRUE),silent = TRUE)
  if("try-error" %in% class(test)) stop("pandoc not found. Please install pandoc and pandoc-citeproc first (https://pandoc.org/)!")
}