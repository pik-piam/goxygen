#' buildHTML
#' 
#' Converts a folder with markdown files and a corresponding literature library 
#' (if available) to HTML files and creates cross-links between them.
#' 
#' Pandoc (https://pandoc.org/) together with pandoc-citeproc need to be installed 
#' on the system.
#' 
#' @param folder location the HTML files should be written to
#' @param mdfolder path to the markdown folder to be used as source
#' @param literature path to a bibliography, if available (will be ignored
#' if file does not exist)
#' @param supplementary a vector of files and/or folders required for the conversion
#' (e.g. an images subdirectory with figures to be shown in the documents)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{buildPDF}}
#' @export

buildHTML <- function(folder="html", mdfolder="markdown", literature="literature.bib", supplementary=NULL) {
  message("Start HTML creation...")
  check_pandoc()
  files <- list.files(mdfolder,pattern="*.md",full.names = TRUE)
  moduleNames <- sub("\\.[^.]*$","",basename(files))
  if(!dir.exists(folder)) dir.create(folder)
  file.copy(system.file("templates","template.css",package="goxygen"),paste0(folder,"/template.css"))
  ref <- tempfile()
  returnReferences(moduleNames,paste0(moduleNames,".htm"),ref, level=2)
  bib <- ifelse(file.exists(literature),paste0("--bibliography=",literature),"")
  for(m in moduleNames) {
    system(paste0("pandoc ",mdfolder,"/",m,".md ",ref," -o ",folder,"/",m,
                  ".htm --css template.css ",bib," --metadata link-citations=true --mathjax"))
  }
  unlink(ref)
  for(elem in supplementary) file.copy(elem,folder,recursive = TRUE, overwrite = TRUE)
  message("...finished HTML creation!")
}
