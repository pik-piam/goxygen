#' createSimplePage
#' 
#' Creates markdown code from a supplied data list 
#' 
#' @param data a list of data entries for the resulting markdown page. Following 
#' entries can be provided:
#' \describe{
#'   \item{title}{Page title}
#'   \item{description}{General description}
#'   \item{limitations}{Limitations the implementation comes with}
#'   \item{authors}{Module authors}
#' }
#' @return a character vector containing the produced markdown text
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}
#' @export


createSimplePage <- function(data) {
  
  out <- NULL
  zz <- textConnection("out",open = "w", local=TRUE)
  
  .header(zz,paste0(data$title," (",data$name,")"),1, id=gsub("/","",data$name))
  
  .header(zz,"Description",2)
  .write(zz,data$description)
  
  .limitations(zz,data$limitations, emptyIfNULL=TRUE)
  
  if(!is.null(data$authors)) {
    .header(zz,"Authors",2)
    .write(zz,data$authors)
  }
  
  close(zz)
  
  out <- .updateImagePaths(out)
  
  return(out)
}