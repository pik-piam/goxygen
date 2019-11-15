#' createListSimpleCode
#' 
#' support function to create documentation of non-modular GAMS code.
#' 
#' @param path path to the model to be documented
#' @param citation citation data read from a CFF file
#' @param mainfile main file of the model
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{codeCheck}}

createListSimpleCode <- function(path=".", citation=NULL, mainfile="main.gms") {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(path)
  
  # write doc files
  full <- list()
  data <- extractDocumentation(mainfile)
  data$citation <- citation
  full[["index"]] <- createIndexPage(data)
  
  files <- setdiff(list.files(pattern="\\.gms$",recursive = TRUE),mainfile)
  for(f in files) {
    fname <- sub("\\.gms$","",gsub("/","_",f,fixed = TRUE))
    data <- extractDocumentation(f)
    if(is.null(data$title)) data$title <- fname
    data$name <- f
    full[[fname]] <- createSimplePage(data)
  }
  return(full)
}

