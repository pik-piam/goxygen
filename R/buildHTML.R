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
#' @param goxygen_conf path to a goxygen configuration file (will be ignored if file does not exist). The file
#' can be used to supply additional information about the model such as name or version number. 
#' Allowed settings are: Name (name of the model) and Version (model version).
#' Entries are supplied in the format "Name: content", each entry must be a single line. 
#' @param supplementary a vector of files and/or folders required for the conversion
#' (e.g. an images subdirectory with figures to be shown in the documents)
#' @param addHTML character vector with HTML code which should be added to the body of each HTML file.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{buildPDF}}
#' @importFrom utils packageVersion
#' @export

buildHTML <- function(folder="html", mdfolder="markdown", literature="literature.bib", goxygen_conf="goxygen.conf", supplementary=NULL, addHTML=NULL) {
  message("Start HTML creation...")
  check_pandoc()
  files <- list.files(mdfolder,pattern="*.md",full.names = TRUE)
  moduleNames <- sub("\\.[^.]*$","",basename(files))
  if(!dir.exists(folder)) dir.create(folder)
  file.copy(system.file("templates","template.css",package="goxygen"),paste0(folder,"/template.css"))
  ref <- tempfile()
  returnReferences(moduleNames,paste0(moduleNames,".htm"),ref, level=2)
  
  returnHTMLNav <- function(names, targets, id="TOCL") {
    
    if("index"%in%names) {
      #bring index page to the front
      targets <- targets[order(names!="index")]
      names   <- names[order(names!="index")]
      #rename index to overview
      names[names=="index"] <- "Overview"
    }
    
    out <- paste0('<div id="',id,'">','<ul>')
    for(i in 1:length(names)) {
      out <- c(out,paste0('<li><a href="',targets[i],'">',names[i],'</a></li>'))
    }
    out <- c(out,'</ul>','</div>')
    return(out)
  }
  
  addMainHeaderHTML <- function(goxygen_conf) {
    cfg <- readConf(goxygen_conf)
  
    if(file.exists("images/logo.png")) {
      logo <- '  <a href="index.htm"><img id="logo" src="images/logo.png" height="100" alt="model logo" /></a>'
    } else {
      logo <- NULL
    }
    
    if(!is.null(cfg$Version)) {
      version <- paste0(" | Version ",cfg$Version)
    } else {
      version <- NULL
    }
    
    out <- c('<div id="mainheaderbox"></div>',
             '<div id="mainheader">',
             logo,
             paste0('  <div id=mainheadertext><h1 id="mainheadertitle">Model Documentation</h1><h3 id="mainheaderversion">',version,'</h3></div>'),
             paste0('  <small>created with <a href="https://github.com/pik-piam/goxygen">goxygen</a> ',packageVersion("goxygen"),'</small>'),
             '</div>')
    return(out)
  }
  
  addHTML <- c(addHTML,addMainHeaderHTML(goxygen_conf),returnHTMLNav(moduleNames, paste0(moduleNames,".htm")))
  
  bib <- ifelse(file.exists(literature),paste0("--bibliography=",literature),"")
  for(m in moduleNames) {
    ofile <- paste0(folder,"/",m,".htm")
    system(paste0("pandoc ",mdfolder,"/",m,".md ",ref," -o ",ofile,
                  " --css template.css ",bib," --toc --mathjax --standalone --metadata link-citations=true --metadata title=",m))
    # Add additional code to html file
    if(!is.null(addHTML)) {
      html <- readLines(ofile)
      cut <- which(html=="<body>")
      html <- c(html[1:cut],addHTML,html[(cut+1):length(html)])
      writeLines(html,ofile)
    }
  }
  unlink(ref)
  for(elem in supplementary) file.copy(elem,folder,recursive = TRUE, overwrite = TRUE)
  message("...finished HTML creation!")
}
