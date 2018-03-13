#' goxygen
#' 
#' Documentation function which extracts a full model documentation from a 
#' modularized gams model. (incomplete --- work in progress!) 
#' 
#' @param path path to the model to be documented
#' @param docfolder folder the documentation should be written to relative to model folder
#' @param cache Boolean to allow read data from existing cache file
#' @author Jan Philipp Dietrich
#' @importFrom stringi stri_extract_all_regex stri_replace_all_regex
#' @importFrom lucode codeCheck modules_interfaceplot
#' @importFrom pander pandoc.table.return
#' @importFrom utils tail
#' @seealso \code{\link{codeCheck}}
#' @export


goxygen <- function(path=".", docfolder="doc", cache=FALSE) {
  cwd <- getwd()
  on.exit(setwd(cwd))
  
  setwd(path)
  if(!dir.exists(docfolder)) dir.create(docfolder, recursive = TRUE)
  cachefile <- paste0(docfolder,"/doc.rds")
  if(cache & file.exists(cachefile)) {
    cache <- readRDS(cachefile)
    cc <- cache$cc
    interfaces <- cache$interfaces
  } else {
    cc <- codeCheck(debug=TRUE)
    interfaces <- modules_interfaceplot(cc$interfaceInfo)
    saveRDS(list(cc=cc,interfaces=interfaces),cachefile)
  }
  
  copyimages <- function(docfolder) {
    paths <- c("modules/*/*.png",
               "modules/*/*/*.png",
               "modules/*/*.jpg",
               "modules/*/*/*.jpg")
  file.copy(Sys.glob(paths),docfolder,overwrite = TRUE)
  }
  
  copyimages(docfolder)
  setwd(docfolder)
  
  collectTables <- function(cc) {
    .merge <- function(dec) {
      # merge information
      pattern <- "^(.*) \\((.*)\\) *(|\\/.*\\/ *)$"
      description <- sub(pattern,"\\1",dec[,"description"])
      unit <- sub(pattern,"\\2",dec[,"description"])
      unit[!grepl(pattern,dec[,"description"])] <- ""
      unit <- sub("mio.","10^6",unit)
      unit <- gsub("\\\\","/",unit)
      return(data.frame(name=dec[,"names"],sets=dec[,"sets"], description=description, unit=unit, stringsAsFactors = FALSE))
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
    .clean <- function(x,caption) {
      if(is.null(x)) return(NULL)
      if(nrow(x)==0) return(NULL)
      rownames(x) <- NULL
      return(pandoc.table.return(x, "pandoc", caption=caption, split.tables=160))
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
      path <- paste0(modules,folder,"/",r,"/equations.gms")
      if(file.exists(path)) out[[r]] <- extractDocumentation(path, start_type="realization")
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
  
  writeModulePage <- function(name,data,module,seealso) {
    
    zz <- file(paste0(name,".md"), "w")
    
    .empty <- function(zz) {
      writeLines("",zz)
    }
    
    .write <- function(data,zz) {
      if(!is.null(data)) writeLines(data,zz)
      .empty(zz)
    }
    
    .header <- function(title,level,zz) {
      if(level<3) {
        writeLines(title,zz)
        symbol <- ifelse(level==1,"=","-")
        writeLines(paste(rep(symbol,nchar(title)), collapse=""),zz)
      } else {
        start <- paste(rep("#",level),collapse="")
        writeLines(paste(start,title),zz)
      }
      .empty(zz)
    }
    
    .interfaceplot <- function(name,zz) {
      file <- paste0("interfaces_",sub("^.*_","",name),".png")
      if(file.exists(file)) {
       .write(paste0("![Interfaces to other modules](",file,"){ height=50% width=100% }"),zz)
      } else {
       .write("**Interface plot missing!**",zz) 
      }
    }
    
    .limitations <- function(limitations,zz) {
      if(is.null(limitations)) limitations <- "There are no known limitations."
      limitations <- c("**Limitations**",limitations)
      limitations <- paste(">",limitations)
      .write(limitations,zz)
    }
    
    .header(paste0(module$doc$title," (",name,")"),1,zz)
    .header("Description",2,zz)
    .write(module$doc$description,zz)
    
    .header("Interfaces",2,zz)
    
    .interfaceplot(name,zz)
    
    .header("Input",3,zz)
    .write(data$input, zz)
    
    .header("Output",3,zz)
    .write(data$output, zz)
    
    .header("Realizations",2,zz)
    
    rdata <- module$rdata
    for(r in names(rdata)) {
      .header(r,3,zz)
      .write(rdata[[r]]$realization,zz)
      .limitations(rdata[[r]]$limitations,zz)
    }
    
    .header("Definitions",2,zz)
    .write(data$declarations, zz)
    
    .header("Authors",2,zz)
    .write(module$doc$authors,zz)
    
    .header("See Also",2,zz)
    .write(paste0("[",sort(seealso),"]",collapse=", "),zz)
    
    .header("References",2,zz)
    
    close(zz)
  }
   
  out <- collectTables(cc)

  # write doc files
  for(m in setdiff(names(out),"core")) {
    mr <- collectRealizations(m,cc)
    seealso <- collectSeealso(interfaces[[m]],m,cc$modulesInfo)
    writeModulePage(m,out[[m]],mr,seealso)
  }
  }
  