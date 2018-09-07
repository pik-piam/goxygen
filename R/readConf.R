#' readConf
#' 
#' Read goxygen configuration file
#' @param goxygen_conf path to a goxygen configuration file (will be ignored if file does not exist). The file
#' can be used to supply additional information about the model such as name or version number. 
#' Allowed settings are: Name (name of the model), Version (model version) and Logo (path to a model logo).
#' Entries are supplied in the format "Name: content", each entry must be a single line. 
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{buildHTML}}
#' @export

readConf <- function(goxygen_conf) {
  if(!file.exists(goxygen_conf)) return(NULL)
  a <- readLines(goxygen_conf)
  pattern <- "^([^:]*): (.*)$"
  #extract only settings from lines which follow the pattern:
  a <- grep(pattern,a,value=TRUE)
  item <- sub(pattern,"\\1",a)
  value <- sub(pattern,"\\2",a)
  names(value) <- item
  return(as.list(value))
}