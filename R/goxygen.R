#' goxygen
#' 
#' Documentation function which extracts a full model documentation from a 
#' modularized gams model. The function extracts comments used as documentation,
#' extracts code and can extract and convert GAMS equations as latex code. Output
#' is returned in Markdown, HTML and PDF format.
#' 
#' @note Documentation lines in the code must start with *' to be detected as documentation.
#' Identifier at the beginning of each block describe what kind of documentation is given. 
#' All identifiers start with @ followed by the name of the identifier. Currently, following
#' identifiers are available
#' \itemize{
#'  \item @title Title 
#'  \item @authors List of authors
#'  \item @description Model description (only the documentation text will be interpreted)
#'  \item @equations Equation description (documentation text will be extracted and gams equations
#'  will be converted to latex code)
#'  \item @code Code description (documentation text and code will be extracted)
#'  \item @limitations details about limitations of an implementation
#'  \item @stop everything following will be ignored until the next identifier is mentioned again. Useful
#'  to stop a section
#' }
#' 
#' In addition you can store a model logo (100px height, 100px weight) as \code{logo.png} in the main
#' folder of the model which then will be used in the HTML version of the documentation.
#' If you want to add citations to your documentation you can do so by adding a bibtex file with 
#' the name literature.bib in the main folder of the model. To link these references in the text
#' you can use the syntax \code{@<id>} in which "<id>" stands for the identifier given to the 
#' corresponding bibtex entry.
#' 
#' @param path path to the model to be documented
#' @param docfolder folder the documentation should be written to relative to model folder
#' @param cache Boolean to allow read data from existing cache file
#' @param output List of output to be written, available (and also default) are "html","pdf" and "tex"
#' @param cff path to a citation file in citation-file-format (ignored if not existing)
#' @param modularCode Boolean deciding whether code should be interpreted as modular GAMS code (only av)
#' @param unitPattern pattern that is usedto identify the unit in the description, default =c("\\(","\\)")
#' @param includeCore Boolean whether core should be included or not, default=FALSE
#' @param use_advanced_interfacePlot_function Logical, to switch between interface plot functions, default=FALSE
#' @param mainfile main file of the model
#'
#' @author Jan Philipp Dietrich
#' @importFrom stringi stri_extract_all_regex stri_replace_all_regex stri_write_lines
#' @importFrom lucode codeCheck modules_interfaceplot is.modularGAMS
#' @importFrom pander pandoc.table.return
#' @importFrom citation read_cff cff2bibentry
#' @importFrom yaml as.yaml
#' @importFrom utils tail toBibtex capture.output
#' @seealso \code{\link{codeCheck}}
#' @export
goxygen <- function(path=".", 
                    docfolder="doc", 
                    cache=FALSE, 
                    output=c("html","tex","pdf"), 
                    cff="CITATION.cff", 
                    modularCode=is.modularGAMS(), 
                    unitPattern=c("\\(","\\)"), 
                    includeCore=FALSE, 
                    mainfile="main.gms",
                    use_advanced_interfacePlot_function=FALSE) {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(path)

  if(file.exists(cff)) {
    citation <- read_cff(cff)
  } else {
    citation <- NULL
  }
  
  if(!dir.exists(docfolder)) dir.create(docfolder, recursive = TRUE)
  if(file.exists("literature.bib")) file.copy("literature.bib",paste0(docfolder,"/literature.bib"), overwrite = TRUE)

  copyimages <- function(docfolder,paths) {
    imagefolder <- paste0(docfolder,"/images")
    if(!dir.exists(imagefolder)) dir.create(imagefolder, recursive=TRUE)
    file.copy(Sys.glob(paths),imagefolder,overwrite = TRUE)
  }
    
  if(modularCode) {
    copyimages(docfolder, paths = c("*.png",
                                    "*.jpg",
                                    "modules/*/*.png",
                                    "modules/*/*/*.png",
                                    "modules/*/*.jpg",
                                    "modules/*/*/*.jpg"))
    cachefile <- paste0(docfolder,"/doc.rds")
    if(cache & file.exists(cachefile)) {
      cache <- readRDS(cachefile)
      cc <- cache$cc
      interfaces <- cache$interfaces
    } else {
      cc <- codeCheck(details=TRUE)
      interfaces <- modules_interfaceplot(cc$interfaceInfo, 
                                          targetfolder= paste0(docfolder,"/images"), 
                                          writetable=FALSE,
                                          includeCore=includeCore,
                                          use_advanced_interfacePlot_function=use_advanced_interfacePlot_function)
      saveRDS(list(cc=cc,interfaces=interfaces),cachefile)
    }  
    full <- createListModularCode(cc=cc, interfaces=interfaces, path=".", citation=citation, unitPattern=unitPattern, includeCore=includeCore, mainfile=mainfile, docfolder=docfolder)
  } else {
    copyimages(docfolder, paths=list.files(pattern="\\.(jpg|png)$",recursive = TRUE))
    full <- createListSimpleCode(path=".", citation=citation, mainfile=mainfile)
  }
  
  setwd(docfolder)

  if(any(nomatch <- !(output %in% c("html","pdf","tex")))){
    warning(paste0("No output format '",output[nomatch],"' available. It will be ignored."))
  }
  buildMarkdown(full)
  if("html"%in% output) buildHTML(supplementary="images", citation=citation)
  if("tex" %in% output | "pdf" %in% output) buildTEX(pdf=("pdf" %in% output))
}
  