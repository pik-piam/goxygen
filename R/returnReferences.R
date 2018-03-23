#' returnReferences
#' 
#' Support function to create a reference file linking references with corresponding adresses.
#' 
#' @param names vector of reference names
#' @param targets vector of reference adresses (same order and lengths as names)
#' @param file name of the reference file to be written
#' @param level level of the "References" title to be written
#' 
#' @author Jan Philipp Dietrich

returnReferences <- function(names,targets,file,level=2) {
  if(length(names)!=length(targets)) stop("names and targets must have the same lengths!")
  x <- NULL
  zz <- textConnection("x",open = "w", local=TRUE)
  .header(zz,"References",level=level)
  close(zz)
  for(i in 1:length(names)) {
    x <- c(x,paste0("[",names[i],"]: ",targets[i]))
  }
  writeLines(x,file)  
}