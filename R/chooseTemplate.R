#' chooseTemplate
#' 
#' Support function helping to choose the selected template
#' 
#' @param style visualization style to be used for the creation. 
#' @param templatefolder Folder in which goxygen will search for template files in addition to the pre-installed ones.
#' @param ftype template file type / file ending
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{buildTEX}}

chooseTemplate <- function(style, templatefolder, ftype) {
  templatefile <- c(paste0(templatefolder,"/",style,".",ftype),
                    paste0(system.file("templates",package="goxygen"),"/",style,".",ftype))
  if(file.exists(templatefile[1])) {
    templatefile <- templatefile[1]
  } else if(file.exists(templatefile[2])) {
    templatefile <- templatefile[2]
  } else {
    stop("No template found for style \"",style,"\" (",ftype,")")
  }
  return(templatefile)
}