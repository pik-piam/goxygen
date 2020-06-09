#' buildHTML
#' 
#' Converts a folder with markdown files and a corresponding literature library 
#' (if available) to HTML files and creates cross-links between them.
#' 
#' Pandoc (https://pandoc.org/) together with pandoc-citeproc need to be installed 
#' on the system.
#' 
#' @param style visualization style to be used for the creation. Currently available styles are 
#' "classic" and "ming"
#' @param folder location the HTML files should be written to
#' @param mdfolder path to the markdown folder to be used as source
#' @param literature path to a bibliography, if available (will be ignored
#' if file does not exist)
#' @param citation Citation information in citation file format (optional)
#' @param supplementary a vector of files and/or folders required for the conversion
#' (e.g. an images subdirectory with figures to be shown in the documents)
#' @param debug logical which switches on/off a debug mode which will return additional 
#' status updates and keep build files
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{buildTEX}}
#' @importFrom utils packageVersion
#' @export

buildHTML <- function(style="classic", folder="html", mdfolder="markdown", literature="literature.bib", citation="../CITATION.cff", supplementary="images", debug=FALSE) {
  
  # check style
  if(style=="classic") return(oldBuildHTML(folder=folder, mdfolder=mdfolder, literature=literature, citation=citation, supplementary=supplementary))
  if(style!="ming") stop("Unknown style ", style,"!")
  
  message("Start HTML creation...")
  check_pandoc()
  
  # check available md files
  files <- list.files(mdfolder,pattern="*.md",full.names = TRUE)
  moduleNames <- sub("\\.[^.]*$","",basename(files))
  
  # prepare folder
  if(!dir.exists(folder)) dir.create(folder)
  file.copy(system.file("templates",paste0(style,".css"),package="goxygen"),paste0(folder,"/style.css"))
  for(elem in supplementary) file.copy(elem,folder,recursive = TRUE, overwrite = TRUE)
  
  # prepare reference file
  ref <- ifelse(debug, "ref.md", tempfile())
  returnReferences(moduleNames,paste0(moduleNames,".htm"),ref, level=2)
  
  # prepare main navigation
  returnHTMLref <- function(files) {
    out <- data.frame(name=NULL, title=NULL, stringsAsFactors = FALSE)
    for(f in files) {
      tmp <- readLines(f,n=2)
      # use title if title is detected
      out <- rbind(out, 
                   data.frame(title = ifelse(grepl("^=*$",tmp[2]), sub(" *\\(.*$","",tmp[1]), ""),
                              name = sub(".md","",basename(f),fixed=TRUE), stringsAsFactors = FALSE))
    }
    return(out)
  }
  mainNav <- function(mdfolder){
    cwd <- getwd()
    setwd(mdfolder)
    on.exit(setwd(cwd))
    ref <- returnHTMLref(dir(".", pattern = "*.\\.md$"))
    
    if("index"%in%ref$name) {
      #bring index page to the front
      i <- which(ref$name=="index")
      ref <- ref[c(i,setdiff(1:nrow(ref),i)),]
      #rename index to overview
      ref$title[1] <- "Overview"
    }
    
    has_number <- grepl("^[0-9]{1,2}_",ref$name)
    if(sum(has_number)>=(nrow(ref)-1)) {
      # if all names (expect of one, which might be the index page) begin
      # with a number, use that number in front of the title
      number <- as.integer(sub("_.*$","",ref$name[has_number]))
      ref$title[has_number] <- paste0(format(number,width=2),". ",ref$title[has_number])
    }
    
    out <- paste0('<a href="',ref$name,'.htm">',ref$title,'</a>',collapse="\n")
    return(out)
  }
  
  if(is.character(citation) && file.exists(citation)) {
    citation <- read_cff(citation)
  } else if(!is.list(citation)) {
    citation <- list(title="Model Documentation")
  }
  repo <- ifelse(!is.null(citation$`repository-code`),paste0(" -V repo=",citation$`repository-code`),"")
  bib <- ifelse(file.exists(literature),paste0("--bibliography=",literature),"")
  logo <- ifelse(file.exists(paste0(folder,"/images/logo.png")), " -V logo", "")
  authors <- ""
  if(!is.null(citation$authors)) {
    .tmp <- function(x) return(paste(x$`given-names`,x$`family-names`))
    authors <- paste0(" -V author-meta=\"", sapply(citation$authors,.tmp),"\"")
    authors <- paste(rev(authors),collapse="")
  }
  
  for(m in moduleNames) {
    ofile <- paste0(folder,"/",m,".htm")
    pandoc_call <-paste0("pandoc ",mdfolder,"/",m,".md ",ref," -o ",ofile,
                         " --css template.css ",
                         bib,
                         authors,
                         logo,
                         repo,
                         " --toc --mathjax --standalone --metadata link-citations=true",
                         " --template=",system.file("templates",paste0(style,".html5"),package="goxygen"),
                         " --metadata title=",m,
                         " -V modeltitle=\"",citation$title,"\"",
                         " -V goxygenversion=",packageVersion("goxygen"),
                         " -V modelversion=",citation$version,
                         " -V mainnav='",mainNav(mdfolder),"'")
    if(debug) cat(pandoc_call,"\n\n")
    system(pandoc_call)
  }
  if(!debug) unlink(ref)
  message("...finished HTML creation!")
}
