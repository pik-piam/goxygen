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
#' @param citation Citation information in citation file format (optional)
#' @param supplementary a vector of files and/or folders required for the conversion
#' (e.g. an images subdirectory with figures to be shown in the documents)
#' @param pdf boolean which specifies whether pdf file should be generated from tex
#' @author Jan Philipp Dietrich, Kristine Karstens
#' @seealso \code{\link{goxygen}}, \code{\link{buildHTML}}
#' @export

buildTEX <- function(file="documentation.tex", mdfolder="markdown", literature="literature.bib", citation="../CITATION.cff", supplementary=NULL, pdf=TRUE) {
  message("Start TEX creation...")
  check_pandoc()
  if(is.character(citation) && file.exists(citation)) citation <- read_cff(citation)
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
  
  .tmp <- function(x) return(paste0(x[["given-names"]]," \\textbf{",x[["family-names"]],"}"))
  version <- ifelse(is.null(citation$version), "", paste0('--variable version="',paste0("Model Version ",citation$version),'"'))
  title <- ifelse(is.null(citation$title), "Model Documentation", citation$title)
  authors <- ifelse(is.null(citation$authors), "",  paste0('--variable author="',paste(sapply(citation$authors,.tmp),collapse=" | "),'"'))
  logo <- ifelse(file.exists("images/logo.png"), 
                 '\\raisebox{-.2\\height}{\\includegraphics[width=2cm]{images/logo}}  \\hskip 0.5em ', NULL)
  
  additional_settings <- paste(paste0('--variable title="',title,'"'),
                               '--variable titlepage',
                               '--variable toc',
                               authors,
                               paste0('--variable logo="',logo,'"'),
                               paste0('--variable date="created with \\href{https://github.com/pik-piam/goxygen}{goxygen} on ',
                                      format(Sys.time(), "%b %d %Y"),'"'),
                               paste0('--variable goxygen="created with \\href{https://github.com/pik-piam/goxygen}{goxygen} ',
                                      packageVersion("goxygen"),'"'),
                               version)
  
  if(is.null(literature)) bib <- ""
  else bib <- ifelse(file.exists(literature),paste0(" --metadata link-citations --bibliography=",literature),"")
  system(paste0("pandoc ",files," -s -o ",file," --template ",
                system.file("templates","template.latex",package="goxygen"),
                " -V colorlinks ",additional_settings," --listings",bib))
  tex <- readLines(file)
  tex <- gsub("{multline*}","{dmath*}",tex, fixed=TRUE)
  writeLines(tex,file)
  message("...finished TEX creation!")
  if(pdf) {
    message("Start PDF creation...")
    for(i in 1:3) tmp <- system(paste("pdflatex -interaction=nonstopmode",file),intern=TRUE)
    if(grepl("error",tail(tmp,2)[1])) writeLines(tmp,con ="goxygen_pdflatex.log")
    message("...finished running pdflatex")
  }
}
