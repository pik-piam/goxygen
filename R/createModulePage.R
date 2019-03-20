#' createModulePage
#' 
#' Creates markdown code from a supplied data list 
#' 
#' @param data a list of data entries for the resulting markdown page. Following 
#' entries can be provided:
#' \describe{
#'   \item{name}{Name of the module}
#'   \item{title}{Page title}
#'   \item{description}{General description}
#'   \item{input}{Table containing inputs to the module}
#'   \item{output}{Table containing outputs from the module}
#'   \item{realizations}{A list of realizations with entries
#'   "description" and "limitations" for each of them}
#'   \item{declarations}{Table of declarations for internal objects}
#'   \item{stes}{Table containing sets used in the module}
#'   \item{authors}{Module authors}
#'   \item{seealso}{A vector with names of relevant other documentation pages.}
#' }
#' @return a character vector containing the produced markdown text
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}
#' @export


createModulePage <- function(data) {
  
  out <- NULL
  zz <- textConnection("out",open = "w", local=TRUE)
  
  .header(zz,paste0(data$title," (",data$name,")"),1, id=data$name)
  .header(zz,"Description",2)
  .write(zz,data$description)
  
  .header(zz,"Interfaces",2)
  
  .interfaceplot(zz,data$name)
  
  .header(zz,"Input",3)
  .write(zz,data$input)
  
  .header(zz,"Output",3)
  .write(zz,data$output)
  
  .header(zz,"Realizations",2)
  
  rdata <- data$realizations
  i <- 1
  for(r in names(rdata)) {
    title <- paste0("(",LETTERS[i],") ",r)
    .header(zz,title,3)
    .write(zz,rdata[[r]]$description)
    .limitations(zz,rdata[[r]]$limitations)
    i <- i+1
  }
  
  .limitations(zz,data$limitations, emptyIfNULL=TRUE)
  
  .header(zz,"Definitions",2)
  .header(zz,"Objects",3)
  .write(zz,data$declarations)
  .header(zz,"Sets",3)
  .write(zz,data$sets)
  
  .header(zz,"Authors",2)
  .write(zz,data$authors)
  
  .header(zz,"See Also",2)
  .write(zz,paste0("[",sort(data$seealso),"]",collapse=", "))
  
  close(zz)
  
  out <- .updateImagePaths(out)
  
  return(out)
}