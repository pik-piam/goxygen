#' check_pandoc
#' 
#' Support function which checks pandoc availability and stops with an error in case
#' that pandoc cannot be found
#' @param error boolean indicating whether function should throw an error in case of missing pandoc or return a boolean FALSE.
#' @return boolean indicating whether pandoc is available or not.
#' @author Jan Philipp Dietrich
#' @export

check_pandoc <- function(error=FALSE) {
  test <-try(system("pandoc --help",intern = TRUE, ignore.stderr = TRUE),silent = TRUE)
  if("try-error" %in% class(test)) {
    if(error) stop("pandoc not found. Please install pandoc and pandoc-citeproc first (https://pandoc.org/)!")
    return(FALSE)
  } 
  return(TRUE)
}