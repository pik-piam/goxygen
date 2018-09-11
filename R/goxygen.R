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
#' You can supply additional information via a file named goxygen.conf stored in the doc folder of your model.
#' Currently it can contain model namd and model version. The file should be formatted
#' as follows:
#' 
#' \code{Model: MyModel} \cr 
#' \code{Version: 1.2.3-beta}
#' 
#' In addition you can store a model logo (100px height) as \code{doc/images/logo.png} which then will be used in the
#' HTML version of the documentation.
#' 
#' @param path path to the model to be documented
#' @param docfolder folder the documentation should be written to relative to model folder
#' @param cache Boolean to allow read data from existing cache file
#' @param output List of output to be written, available (and also default) are "html","pdf" and "tex"
#' @author Jan Philipp Dietrich
#' @importFrom stringi stri_extract_all_regex stri_replace_all_regex
#' @importFrom lucode codeCheck modules_interfaceplot
#' @importFrom pander pandoc.table.return
#' @importFrom utils tail
#' @seealso \code{\link{codeCheck}}
#' @export


goxygen <- function(path=".", docfolder="doc", cache=FALSE, output=c("html","pdf","tex")) {
  cwd <- getwd()
  on.exit(setwd(cwd))
  
  if(any(nomatch <- !(output %in% c("html","pdf","tex")))){
      warning(paste0("No output format '",output[nomatch],"' available. It will be ignored."))
    }
  
  setwd(path)
  if(!dir.exists(docfolder)) dir.create(docfolder, recursive = TRUE)
  cachefile <- paste0(docfolder,"/doc.rds")
  if(cache & file.exists(cachefile)) {
    cache <- readRDS(cachefile)
    cc <- cache$cc
    interfaces <- cache$interfaces
  } else {
    cc <- codeCheck(debug=TRUE)
    interfaces <- modules_interfaceplot(cc$interfaceInfo, targetfolder= paste0(docfolder,"/images"), writetable=FALSE)
    saveRDS(list(cc=cc,interfaces=interfaces),cachefile)
  }
  
  copyimages <- function(docfolder) {
    paths <- c("modules/*/*.png",
               "modules/*/*/*.png",
               "modules/*/*.jpg",
               "modules/*/*/*.jpg")
    imagefolder <- paste0(docfolder,"/images")
    if(!dir.exists(imagefolder)) dir.create(imagefolder, recursive=TRUE)
    file.copy(Sys.glob(paths),imagefolder,overwrite = TRUE)
  }
  
  copyimages(docfolder)
  setwd(docfolder)
  
  collectTables <- function(cc) {
    .merge <- function(dec) {
      .cleanunit <- function(unit) {
        if(length(unit)==0) return(unit)
        unit[!grepl(pattern,dec[,"description"])] <- ""
        unit <- sub("[Mm]i(lli|)on?\\.?","10^6",unit)
        unit <- gsub("\\\\","/",unit)
        unit <- gsub("$","\\$",unit,fixed=TRUE)
        unit <- gsub("%","\\%",unit,fixed=TRUE)
        unit <- gsub(" per ", "/", unit)
        unit <- gsub("USD([0-9]*(PPP|MER))","USD_{\\1}", unit)
        unit <- paste0("$",unit,"$")
        unit[unit=="$$"] <- ""
        return(unit)
      }
      # merge information
      pattern <- "^(.*) \\((.*)\\) *(|\\/.*\\/ *)$"
      description <- sub(pattern,"\\1",dec[,"description"])
      unit <- sub(pattern,"\\2",dec[,"description"])
      return(data.frame(name=dec[,"names"],sets=dec[,"sets"], description=description, unit=.cleanunit(unit), stringsAsFactors = FALSE))
    }
    .format <- function(out,aps,ifs=NULL) {
      if(nrow(out)==0) {
        if(is.null(ifs)) {
          return(NULL)
        } else {
          return(list(input=NULL,output=NULL))
        }
      }
      # format information
      fout <- data.frame(Name=paste0(out$name,sub("()","",paste0(" (",out$sets,")"),fixed=TRUE)), 
                         Description=out$description, 
                         Unit=out$unit)
      aps[,] <- ifelse(aps==1,"x","")
      fout <- cbind(fout,aps)
      if(is.null(ifs)) return(fout)
      return(list(input=fout[ifs[names(ifs) == "in"],],output=fout[ifs[names(ifs) == "out"],1:3]))
    }
    .clean <- function(x,caption, keep_names=1:3) {
      if(is.null(x)) return(NULL)
      if(nrow(x)==0) return(NULL)
      sub <- NULL
      j <- 1
      for(i in setdiff(1:length(x),keep_names)) {
        sub <- c(sub,paste0(toupper(letters[j]),": ",names(x)[i]))
        names(x)[i] <- toupper(letters[j])
        j <- j+1
      }
      if(!is.null(sub)) caption <- paste0(caption, " (",paste(sub,collapse=" | "),")")
      rownames(x) <- make.unique(as.character(x[[1]]))
      rownames(x) <- gsub("\\,([^ ])",", \\1",rownames(x))
      rownames(x) <- sub("(","\\ \n(",rownames(x), fixed=TRUE)
      x <- x[sort(rownames(x)),]
      x <- x[!grepl("^o[qv]",rownames(x)),]
      split.cells <- c(15,30,rep(1,length(x)-2))
      return(pandoc.table.return(x[-1], "pandoc", caption=caption, split.tables=100, split.cells=split.cells, 
                                 emphasize.rownames=FALSE,  keep.line.breaks = TRUE))
    }
    interfaceTables <- function(cc, module) {
      # collect information about module interfaces
      ifs <- cc$interfaceInfo[[module]]
      ifs <- sort(ifs)
      dec <- cc$gams$declarations[cc$gams$declarations[,"names"] %in% ifs,, drop=FALSE]
      dec <- dec[!duplicated(dec[,"names"]),,drop=FALSE]
      aps <- cc$ap$appearance[ifs,grepl(paste0("^",module,"\\."),colnames(cc$ap$appearance)),drop=FALSE]
      colnames(aps) <- sub("^.*\\.","",colnames(aps))
      aps <- aps[dec[,"names"],,drop=FALSE]
      aps <- aps[!duplicated(rownames(aps)),,drop=FALSE]
    
      out  <- .merge(dec)
      out <- .format(out,aps,ifs)
        
      out$input  <- .clean(out$input,"module inputs")
      out$output <- .clean(out$output,"module outputs")
      return(out)
    }
    moduleTables <- function(cc, module) {
      # collect information about module interfaces
      dec <- cc$gams$declarations[grepl(paste0("^",module,"\\."),rownames(cc$gams$declarations)),, drop=FALSE]
      dec <- dec[order(dec[,"names"]),,drop=FALSE]
      if(nrow(dec)==0) return(NULL)
      dec <- dec[!(dec[,"names"] %in% cc$interfaceInfo[[module]]),, drop=FALSE]
      dec <- dec[!duplicated(dec[,"names"]),,drop=FALSE]
      
      aps <- cc$ap$appearance[dec[,"names"],grepl(paste0("^",module,"\\."),colnames(cc$ap$appearance)),drop=FALSE]
      colnames(aps) <- sub("^.*\\.","",colnames(aps))
      aps <- aps[!duplicated(rownames(aps)),,drop=FALSE]
      
      out <- .merge(dec)
      out <- .format(out,aps)
      return(.clean(out,"module-internal objects"))
    }

    modInfo <- rbind(cc$modulesInfo,core=c(name="core",number="",folder="core",realizations=""))
    out <- list()
    for(m in names(cc$interfaceInfo)) {
      out[[modInfo[m,"folder"]]] <- interfaceTables(cc,m)
      out[[modInfo[m,"folder"]]]$declarations <- moduleTables(cc,m)
    }
    return(out)
  }
  
  collectRealizations <- function(m, cc,modules="../modules/") {
    m <- sub("[0-9]*_","",m)
    if(m=="core") return(NULL)
    rea <- strsplit(cc$modulesInfo[m,"realizations"],",")[[1]]
    folder <- cc$modulesInfo[m,"folder"]
    out <- list()
    for(r in rea) {
      rmain <- paste0(modules,folder,"/",r,".gms")
      files <- sub(".*/([^.]*)\\.gms.*$","\\1.gms",grep(".gms",readLines(rmain), value=TRUE, fixed=TRUE))
      paths <- c(rmain,paste0(modules,folder,"/",r,"/",files))
      out[[r]] <- extractDocumentation(paths, start_type="equations")
    }
    module_description <- extractDocumentation(paste0(modules,folder,"/",folder,".gms"))
    return(list(rdata=out,doc=module_description))
  }
  
  collectSeealso <- function(interfaces,module,modulesInfo) {
    module <- sub("^.*_","",module)
    seealso <- setdiff(unique(c(interfaces$to,interfaces$from)),module)
    modulesInfo <- rbind(modulesInfo,core=c("core","","core",""))
    seealso <- modulesInfo[seealso,"folder"]
    return(seealso)
  }
  
 buildModulePage <- function(name,data,module,seealso) {
    
    out <- NULL
    zz <- textConnection("out",open = "w", local=TRUE)
    
    .header(zz,paste0(module$doc$title," (",name,")"),1, id=name)
    .header(zz,"Description",2)
    .write(zz,module$doc$description)
    
    .header(zz,"Interfaces",2)
    
    .interfaceplot(zz,name)
    
    .header(zz,"Input",3)
    .write(zz,data$input)
    
    .header(zz,"Output",3)
    .write(zz,data$output)
    
    .header(zz,"Realizations",2)
    
    rdata <- module$rdata
    i <- 1
    for(r in names(rdata)) {
      title <- paste0("(",toupper(letters[i]),") ",r)
      .header(zz,title,3)
      .write(zz,rdata[[r]]$description)
      .limitations(zz,rdata[[r]]$limitations)
      i <- i+1
    }
    
    .header(zz,"Definitions",2)
    .write(zz,data$declarations)
    
    .header(zz,"Authors",2)
    .write(zz,module$doc$authors)
    
    .header(zz,"See Also",2)
    .write(zz,paste0("[",sort(seealso),"]",collapse=", "))
    
    close(zz)
    
    out <- .updateImagePaths(out)
    
    return(out)
  }

 buildIndexPage <- function(path="../main.gms",authorsfile="../AUTHORS") {
   index <- extractDocumentation(path)
   authors <- .modelAuthors(authorsfile)
   out <- NULL
   zz <- textConnection("out",open = "w", local=TRUE)
   
   if(!is.null(index$title)) .header(zz,index$title,1)
   .write(zz,index$description)
   
   if(!is.null(authors)) {
     .header(zz,"Authors (alphabetically)",2)
     .write(zz,paste(sort(authors),collapse=", "))
   }
   
   close(zz)
   out <- .updateImagePaths(out)
   return(out)
 }
    
  out <- collectTables(cc)
  moduleNames <- cc$modulesInfo[,"folder"]
  
  # write doc files
  full <- list()
  full[["index"]] <- buildIndexPage("../main.gms")
  
  for(m in setdiff(sort(names(out)),"core")) {
    mr <- collectRealizations(m,cc)
    seealso <- collectSeealso(interfaces[[m]],m,cc$modulesInfo)
    full[[m]] <- buildModulePage(name=m,data=out[[m]],module=mr,seealso=seealso)
  }
  
  returnMarkdown <- function(x, folder="markdown") {
    if(!dir.exists(folder)) dir.create(folder)
    returnReferences(names(x),paste0(names(x),".md"),paste0(folder,"/md.ref"),level=2)
    for(n in names(x)) {
      writeLines(x[[n]],paste0(folder,"/",n,".md"))
    }
  }
  
  returnMarkdown(full)
  
  if("html"%in%output) buildHTML(supplementary="images")
  if("pdf" %in%output) buildPDF()
  if("tex" %in%output) buildTEX()
}
  