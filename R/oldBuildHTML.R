#' oldBuildHTML
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
#' @param citation Citation information in citation file format (optional)
#' @param supplementary a vector of files and/or folders required for the conversion
#' (e.g. an images subdirectory with figures to be shown in the documents)
#' @param addHTML character vector with HTML code which should be added to the body of each HTML file.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{buildTEX}}
#' @importFrom utils packageVersion

oldBuildHTML <- function(folder="html", mdfolder="markdown", literature="literature.bib", citation="../CITATION.cff", supplementary=NULL, addHTML=NULL) {
  message("Start HTML creation...")
  check_pandoc(error=TRUE)
  files <- list.files(mdfolder,pattern=".*\\.md$",full.names = TRUE)
  moduleNames <- sub("\\.[^.]*$","",basename(files))
  if(!dir.exists(folder)) dir.create(folder)
  file.copy(system.file("templates","classic.css",package="goxygen"),paste0(folder,"/template.css"))
  ref <- tempfile()
  returnReferences(moduleNames,paste0(moduleNames,".htm"),ref, level=2)
  
  if(is.character(citation) && file.exists(citation)) citation <- read_cff(citation)
  
  returnHTMLNav <- function(names, targets, id="TOCL") {
    
    if("index"%in%names) {
      #bring index page to the front
      targets <- targets[order(names!="index")]
      names   <- names[order(names!="index")]
      #rename index to overview
      names[names=="index"] <- "Overview"
    }
    
    out <- paste0('<nav id="',id,'">','<ul>')
    for(i in 1:length(names)) {
      out <- c(out,paste0('<li><a href="',targets[i],'">',names[i],'</a></li>'))
    }
    out <- c(out,'</ul>','</nav>')
    return(out)
  }
  
  addMainHeaderHTML <- function(citation) {
  
    if(file.exists("images/logo.png")) {
      logo <- '  <a href="index.htm"><img id="logo" src="images/logo.png" height="100" alt="model logo" /></a>'
    } else {
      logo <- NULL
    }
    
    if(!is.null(citation$version)) {
      version <- paste0(" | Version ",citation$version)
    } else {
      version <- NULL
    }
    
    out <- c('<div id="mainheaderbox">',
             '<div id="mainheader">',
             logo,
             paste0('  <div id=mainheadertext><h1 id="mainheadertitle">Model Documentation</h1><h3 id="mainheaderversion">',version,'</h3></div>'),
             paste0('  <small>created with <a href="https://github.com/pik-piam/goxygen">goxygen</a> ',packageVersion("goxygen"),'</small>'),
             '</div></div>')
    return(out)
  }
  
  addHTML <- c(addHTML,addMainHeaderHTML(citation),returnHTMLNav(moduleNames, paste0(moduleNames,".htm")))
  
  bib <- ifelse(file.exists(literature),paste0("--bibliography=",literature),"")
  
  addText <- function(html,key, content, before=FALSE, occurrence=1) {
    cut <- grep(key,html,fixed=TRUE)[occurrence]
    if(is.na(cut)) {
      warning("Pattern ",key," not found!")
      return(html)
    }
    if(before) cut <- cut-1
    return(c(html[1:cut],content,html[(cut+1):length(html)])) 
  }
  
  for(m in moduleNames) {
    ofile <- paste0(folder,"/",m,".htm")
    system(paste0("pandoc ",mdfolder,"/",m,".md ",ref," -o ",ofile,
                  " --css template.css ",bib," --toc --mathjax --standalone --metadata link-citations=true --metadata title=",m))
    # Add additional code to html file
      html <- readLines(ofile)
      html <- addText(html, "<h1 ","<div id=\"everything\">",before=TRUE, occurrence=2)
      html <- addText(html, "</body>","</div>", before=TRUE)
      html <- sub("(<title>)(.*)(</title>)",paste0("\\1",citation$title," | \\2\\3"),html)
      html <- addText(html, "<body>", addHTML)
      #add mathjax config
      addMJConfig <- '<script type="text/x-mathjax-config">
                    MathJax.Hub.Config({
                      CommonHTML: { linebreaks: { automatic: true, width: "32em" } },
                      "HTML-CSS": { linebreaks: { automatic: true, width: "32em" } },
                             SVG: { linebreaks: { automatic: true, width: "32em" } }
                    }); 
                  </script>'
      html <- addText(html,"<head>", addMJConfig)
      writeLines(html,ofile)
  }
  unlink(ref)
  for(elem in supplementary) file.copy(elem,folder,recursive = TRUE, overwrite = TRUE)
  message("...finished HTML creation!")
}
