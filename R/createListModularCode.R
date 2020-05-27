#' createListModularCode
#' 
#' support function to create documentation of modular GAMS code.
#' 
#' @param cc codeCheck information
#' @param interfaces interface information
#' @param path path to the model to be documented
#' @param citation citation data read from a CFF file
#' @param unitPattern pattern that is usedto identify the unit in the description, default =c("\\(","\\)")
#' @param includeCore Boolean whether core should be included or not, default=FALSE
#' @param mainfile main file of the model
#' @param docfolder folder the documentation should be written to relative to model folder
#' @author Jan Philipp Dietrich
#' @importFrom stringi stri_extract_all_regex stri_replace_all_regex stri_write_lines
#' @importFrom gms codeCheck modules_interfaceplot is.modularGAMS
#' @importFrom pander pandoc.table.return
#' @importFrom citation read_cff cff2bibentry
#' @importFrom yaml as.yaml
#' @importFrom utils tail toBibtex capture.output
#' @seealso \code{\link{codeCheck}}

createListModularCode <- function(cc, interfaces, path=".", citation=NULL, unitPattern=c("\\(","\\)"), includeCore=FALSE, mainfile="main.gms", docfolder="doc") {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(path)
  
  collectTables <- function(cc, unitPattern) {
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
      # create pattern to identify the unit in the describtion
      pattern <- paste0("^(.*) ",unitPattern[1],"(.*)",unitPattern[2]," *(|\\/.*\\/ *)$")
      description <- sub(pattern,"\\1",dec[,"description"])
      unit <- sub(pattern,"\\2",dec[,"description"])
      return(data.frame(name=dec[,"names"],sets=dec[,"sets"], description=description, unit=.cleanunit(unit), stringsAsFactors = FALSE))
    }
    .format <- function(out,aps,ifs=NULL) {
      if(nrow(out)==0 &&  is.null(ifs)) return(NULL)
      if(nrow(out)==0 && !is.null(ifs)) return(list(input=NULL,output=NULL))
      
      # format information
      fout <- data.frame(Name=paste0(out$name,sub("()","",paste0(" (",out$sets,")"),fixed=TRUE)), 
                         Description=out$description, 
                         Unit=out$unit)
      aps[,] <- ifelse(aps==1,"x","")
      fout <- cbind(fout,aps)
      if(is.null(ifs)) return(fout)
      return(list(input=fout[ifs[names(ifs) == "in"],],output=fout[ifs[names(ifs) == "out"],1:3]))
    }
    .clean <- function(x,caption, keep_names=1:3, braket.break=TRUE) {
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
      if(braket.break) rownames(x) <- sub("(","\\ \n(",rownames(x), fixed=TRUE)
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
      dec <- cc$declarations[cc$declarations$names %in% ifs,, drop=FALSE]
      dec <- dec[!duplicated(dec$names),,drop=FALSE]
      aps <- cc$appearance[ifs,grepl(paste0("^",module,"\\."),colnames(cc$appearance)),drop=FALSE]
      colnames(aps) <- sub("^.*\\.","",colnames(aps))
      aps <- aps[dec$names,,drop=FALSE]
      aps <- aps[!duplicated(rownames(aps)),,drop=FALSE]
    
      out  <- .merge(dec)
      out <- .format(out,aps,ifs)
        
      out$input  <- .clean(out$input,"module inputs")
      out$output <- .clean(out$output,"module outputs")
      return(out)
    }
    moduleTables <- function(cc, module) {
      # collect information about module interfaces
      dec <- cc$declarations[grepl(paste0("^",module,"\\."),cc$declarations$origin),, drop=FALSE]
      dec <- dec[dec$typ!="set",]
      dec <- dec[order(dec$names),,drop=FALSE]
      if(nrow(dec)==0) return(NULL)
      dec <- dec[!(dec[,"names"] %in% cc$interfaceInfo[[module]]),, drop=FALSE]
      dec <- dec[!duplicated(dec[,"names"]),,drop=FALSE]
      
      aps <- cc$appearance[dec$names,grepl(paste0("^",module,"\\."),colnames(cc$appearance)),drop=FALSE]
      colnames(aps) <- sub("^.*\\.","",colnames(aps))
      aps <- aps[!duplicated(rownames(aps)),,drop=FALSE]
      
      out <- .merge(dec)
      out <- .format(out,aps)
      return(.clean(out,"module-internal objects"))
    }
    
    setTables <- function(cc, module) {
      tmp <- cc$appearance[,grep(paste0(module,"."),colnames(cc$appearance),fixed=TRUE),drop=FALSE]
      elems <- rownames(tmp)[rowSums(tmp)>0]
      elems <- grep("^o[qv]",elems,invert=TRUE,value=TRUE) #remove output objects
      sets <- cc$declarations$sets[cc$declarations$names %in% elems]
      sets <- unique(unlist(strsplit(sets,",")))
      
      if(!is.null(cc$setappearance)) {
        #alternative method to find sets
        aps <- cc$setappearance[,grepl(paste0("^",module,"\\."),colnames(cc$setappearance)),drop=FALSE]
        aps <- aps[rowSums(aps)>0,,drop=FALSE]
        sets2 <- rownames(aps)
        #if(length(setdiff(sets,sets2))>0) warning("More sets found after old method in module ",module," (",paste(setdiff(sets,sets2),collapse=", "),")")
        sets <- union(sets,sets2)
      }
      
      dec <- cc$declarations[cc$declarations$names %in% sets,]
      dec <- dec[order(dec$names),,drop=FALSE]
      if(nrow(dec)==0) return(NULL)
      dec <- dec[!duplicated(dec[,"names"]),,drop=FALSE]
      dec$names <- sub("\\(\\)$","",paste0(dec$names,"(",dec$sets,")"))

      return(.clean(dec[c("names","description")],"sets in use", braket.break = FALSE))
    }

    modInfo <- rbind(cc$modulesInfo,core=c(name="core",number="",folder="core",realizations=""))
    out <- list()
    for(m in names(cc$interfaceInfo)) {
      out[[modInfo[m,"folder"]]] <- interfaceTables(cc,m)
      out[[modInfo[m,"folder"]]]$declarations <- moduleTables(cc,m)
      out[[modInfo[m,"folder"]]]$sets <- setTables(cc,m)
    }
    return(out)
  }
  
  collectRealizations <- function(m,cc,modules="modules/") {
    m <- sub("[0-9]*_","",m)
    if(m=="core") {
      outSub <- list()
      outSub$realizations <- list()
      files <- list.files(path="core",pattern="\\.gms")
      paths <- paste0("core/",files)
      outSub$realizations[["core"]] <- extractDocumentation(paths, start_type="equations")
    } else {  
      rea <- strsplit(cc$modulesInfo[m,"realizations"],",")[[1]]
      folder <- cc$modulesInfo[m,"folder"]
      modulegms <- paste0(modules,folder,"/module.gms")
      if(!file.exists(modulegms)) modulegms <- paste0(modules,folder,"/",folder,".gms")
      outSub <- extractDocumentation(modulegms)
      outSub$realizations <- list()
      for(r in rea) {
        rmain <- paste0(modules,folder,"/",r,"/realization.gms")
        if(!file.exists(rmain)) rmain <- paste0(modules,folder,"/",r,".gms")
        files <- sub(".*/([^.]*)\\.gms.*$","\\1.gms",grep(".gms",readLines(rmain), value=TRUE, fixed=TRUE))
        paths <- c(rmain,paste0(modules,folder,"/",r,"/",files))
        outSub$realizations[[r]] <- extractDocumentation(paths, start_type="equations")
      }
    }  
    return(outSub)
  }
  
  collectSeealso <- function(interfaces,module,modulesInfo) {
    module <- sub("^.*_","",module)
    seealso <- setdiff(unique(c(interfaces$to,interfaces$from)),module)
    modulesInfo <- rbind(modulesInfo,core=c("core","","core",""))
    seealso <- modulesInfo[seealso,"folder"]
    return(seealso)
  }
 
  out <- collectTables(cc, unitPattern)
  moduleNames <- cc$modulesInfo[,"folder"]
  
  # write doc files
  full <- list()
  data <- extractDocumentation(mainfile)
  data$citation <- citation
  full[["index"]] <- createIndexPage(data)
  
  # take only all modules into account or also core
  if(includeCore)  {
    m_loop <- sort(names(out))
  } else {
    m_loop <- setdiff(sort(names(out)),"core")
  }
  for(m in m_loop) {
    data <- append(out[[m]],collectRealizations(m,cc))
    data$name <- m
    data$seealso <- collectSeealso(interfaces[[m]],m,cc$modulesInfo)
    full[[m]] <- createModulePage(data,docfolder=docfolder)
  }
  return(full)
}

 