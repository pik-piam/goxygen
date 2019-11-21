#' .empty
#' 
#' helper function which adds an empty line in a markdown document
#' 
#' @param zz a connection object of class "textConnection" containing the markdown document
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{createModulePage}}
#' @export
#' 
.empty <- function(zz) {
  writeLines("",zz)
}

#' .write
#' 
#' helper function which writes a character vector line by line in a markdown document
#' 
#' @param zz a connection object of class "textConnection" containing the markdown document
#' @param data a character vector to be written to the markdown document
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{createModulePage}}
#' 
.write <- function(zz,data) {
  if(!is.null(data)) writeLines(data,zz)
  .empty(zz)
}

#' .header
#' 
#' helper function which writes a title for a markdown section
#' 
#' @param zz a connection object of class "textConnection" containing the markdown document
#' @param title the title to be used (character vector of length 1)
#' @param level level of the heading (1 means main header, higher numbers reflect lower levels)
#' @param id ID given to the title (relevant for anchors)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{createModulePage}}
#' 
.header <- function(zz,title,level,id=NULL) {
  if(length(title)>1) {
    warning("Multiline entry to title detected. Only first line will be used!")
    title <- title[1]
  }
  if(!is.null(id)) id <- paste0(" {#id-",id,"}")
  if(level<3) {
    writeLines(paste0(title,id),zz)
    symbol <- ifelse(level==1,"=","-")
    writeLines(paste(rep(symbol,nchar(title)), collapse=""),zz)
  } else {
    start <- paste(rep("#",level),collapse="")
    writeLines(paste(start,title),zz)
  }
  .empty(zz)
}

#' .interfaceplot
#' 
#' helper function which includes interface plot figures in a markdown document, if available.
#' The figures need to be created beforehand.
#' 
#' @param zz a connection object of class "textConnection" containing the markdown document
#' @param name Name of the module for which the interfaceplot should be shown
#' @param docfolder folder the documentation should be written to relative to model folder
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{createModulePage}}
#' 
.interfaceplot <- function(zz,name,docfolder) {
  file <- paste0("images/interfaces_",sub("^[^_]*_","",name),".png")
  if(file.exists(paste0(docfolder,"/",file))) {
    .write(zz,paste0("![Interfaces to other modules](",file,"){ height=50% width=100% }"))
  } else {
    .write(zz,"**Interface plot missing!**") 
  }
}

#' .limitations
#' 
#' helper function which adds a "limitations" section.
#' 
#' @param zz a connection object of class "textConnection" containing the markdown document
#' @param limitations A character vector containing the given limitations
#' @param emptyIfNULL switch which decides whether limitations section should be ignored, if
#' limitations input is NULL or if it should state that there are no known limitations.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{createModulePage}}
#' 
.limitations <- function(zz,limitations, emptyIfNULL=FALSE) {
  if(is.null(limitations)) {
    if(emptyIfNULL) return()
    limitations <- "There are no known limitations."
  }
  limitations <- c("**Limitations**",limitations)
  limitations <- paste(">",limitations)
  .write(zz,limitations)
}


#' .updateImagePaths
#' 
#' helper function which updates relative image paths so that they refer to a subfolder
#' images instead of refering to the current folder.
#' 
#' @param x A character vector containing image paths.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{createModulePage}}
#' 
.updateImagePaths <- function(x){
  return(gsub("\\(([^/]*\\.(png|jpg))\\)","(images/\\1)",x))
}


