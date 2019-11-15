#' buildMarkdown
#' 
#' Creates a folder filled with markdown files from a list object with markdown code
#' 
#' @param x a named list of markdown codes which should be written as markdown files. 
#' The name of each entry will become the file name.
#' @param folder folder the markdown files should be written to
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{buildHTML}}
#' @export

buildMarkdown <- function(x, folder="markdown") {
  if(!dir.exists(folder)) dir.create(folder)
  returnReferences(names(x),paste0(names(x),".md"),paste0(folder,"/md.ref"),level=2)
  for(n in names(x)) {
    stri_write_lines(x[[n]],paste0(folder,"/",n,".md"))
  }
}