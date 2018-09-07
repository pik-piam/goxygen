#' buildTEX
#' 
#' Converts a folder with markdown files and a corresponding literature library 
#' (if available) to a tex file 
#' 
#' Pandoc (https://pandoc.org/) together with pandoc-citeproc need to be installed 
#' on the system.
#' 
#' @param file name of the tex file to be written
#' @param mdfolder path to the markdown folder to be used as source
#' @param literature path to a bibliography, if available (will be ignored
#' if file does not exist)
#' @param supplementary a vector of files and/or folders required for the conversion
#' (e.g. an images subdirectory with figures to be shown in the documents)
#' @author Jan Philipp Dietrich, Kristine Karstens
#' @seealso \code{\link{goxygen}}, \code{\link{buildPDF}}
#' @export

buildTEX <- function(file="documentation.tex", mdfolder="markdown", literature="literature.bib", supplementary=NULL) {
  message("Start TEX creation...")
  check_pandoc()
  for(elem in supplementary) file.copy(elem,".",recursive = TRUE, overwrite = TRUE)
  sep <- tempfile()
  writeLines("\\pagebreak",sep)
  ref <- tempfile()
  files <- list.files(mdfolder,pattern="*.md",full.names = TRUE)
  #bring index to the front
  files <- files[order(!grepl("index.md",files))]
  moduleNames <- sub("\\.[^.]*$","",basename(files))
  returnReferences(moduleNames,paste0("#id-",moduleNames),ref,level=1)
  files <- paste(paste(files,collapse=paste0(" ",sep," ")),ref)
  if(is.null(literature)) bib <- ""
  else bib <- ifelse(file.exists(literature),paste0(" --metadata link-citations --bibliography=",literature),"")
  system(paste0("pandoc ",files," -s -o ",file," --template ",
                system.file("templates","template.latex",package="goxygen"),
                " -V colorlinks --listings",bib))
  message("...finished TEX creation!")
}
