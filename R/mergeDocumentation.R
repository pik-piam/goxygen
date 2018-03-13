mergeDocumentation <- function(x) {
  if(!anyDuplicated(names(x))) return(x)
  out <- list()
  for(i in unique(names(x))) {
    out[[i]] <- unlist(x[names(x)==i], use.names=FALSE)
  }
  return(out)
}