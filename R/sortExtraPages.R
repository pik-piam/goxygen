#' sortExtraPages
#'
#' helper transforming a flat list of extra page blocks into
#' sublists per page
#'
#' @param extraPage flat list of blocks with names indicating identifier and
#' extrapage (e.g. 'description-settings')
#' @author Jan Philipp Dietrich
sortExtraPages <- function(extraPage) {
  out <- list()
  attrPattern <- "^(\\w+)-(\\w+)"
  for (i in seq(extraPage)) {
    type <- sub(attrPattern, "\\1", names(extraPage)[i])
    page <- sub(attrPattern, "\\2", names(extraPage)[i])
    l <- list(extraPage[[i]])
    names(l) <- type
    out[[page]] <- append(out[[page]], l)
  }
  return(out)
}
