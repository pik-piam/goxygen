.empty <- function(zz) {
  writeLines("",zz)
}

.write <- function(zz,data) {
  if(!is.null(data)) writeLines(data,zz)
  .empty(zz)
}

.header <- function(zz,title,level,id=NULL) {
  if(length(title)>1) {
    warning("Multiline entry to title detected. Only first line will be used!")
    title <- title[1]
  }
  if(!is.null(id)) id <- paste0(" {#id-",id,"}")
  if(level<3) {
    writeLines(paste0(title,id),zz)
    symbol <- ifelse(level==1,"=","-")
    writeLines(paste(rep(symbol,nchar(title)), collapse=""),zz)
  } else {
    start <- paste(rep("#",level),collapse="")
    writeLines(paste(start,title),zz)
  }
  .empty(zz)
}

.interfaceplot <- function(zz,name) {
  file <- paste0("images/interfaces_",sub("^.*_","",name),".png")
  if(file.exists(file)) {
    .write(zz,paste0("![Interfaces to other modules](",file,"){ height=50% width=100% }"))
  } else {
    .write(zz,"**Interface plot missing!**") 
  }
}

.limitations <- function(zz,limitations) {
  if(is.null(limitations)) limitations <- "There are no known limitations."
  limitations <- c("**Limitations**",limitations)
  limitations <- paste(">",limitations)
  .write(zz,limitations)
}

.updateImagePaths <- function(x){
  return(gsub("\\(([^/]*\\.(png|jpg))\\)","(images/\\1)",x))
}



