#' createIndexPage
#' 
#' Creates markdown code from a supplied data list 
#' 
#' @param data a list of data entries for the resulting markdown page. Following 
#' entries can be provided:
#' \describe{
#'   \item{title}{Page title}
#'   \item{description}{General description}
#'   \item{citation}{A read in citation in Citation File Format (CFF)}
#' }
#' @return a character vector containing the produced markdown text
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}
#' @export

createIndexPage <- function(data) {
  
  out <- NULL
  zz <- textConnection("out",open = "w", local=TRUE)
  
  if(!is.null(data$title)) .header(zz,data$title,1)
  .write(zz,data$description)
  
  if(!is.null(data$citation)) {
    citbib <- cff2bibentry(data$citation)
    
    authors <- citbib$author
    if(!is.null(authors)) {
      .header(zz,"Authors",2)
      .write(zz,paste(as.character(authors),collapse=", \n"))
    }
    
    .header(zz, "How to cite",2)
    .write(zz,capture.output(citbib))
    .header(zz, "Bibtex format",3)
    .write(zz,c("```",toBibtex(citbib),"```"))
    .header(zz, "Citation File Format",3)
    .write(zz,c("```",as.yaml(data$citation),"```"))
  }
  
  close(zz)
  out <- .updateImagePaths(out)
  return(out)
}