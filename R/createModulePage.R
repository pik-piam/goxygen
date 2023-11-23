#' createModulePage
#'
#' Creates markdown code from a supplied data list
#'
#' @param data a list of data entries for the resulting markdown page. Following
#' entries can be provided:
#' \describe{
#'   \item{name}{Name of the module}
#'   \item{title}{Page title}
#'   \item{description}{General description}
#'   \item{input}{Table containing inputs to the module}
#'   \item{output}{Table containing outputs from the module}
#'   \item{realizations}{A list of realizations with entries
#'   "description" and "limitations" for each of them}
#'   \item{declarations}{Table of declarations for internal objects}
#'   \item{stes}{Table containing sets used in the module}
#'   \item{authors}{Module authors}
#'   \item{seealso}{A vector with names of relevant other documentation pages.}
#' }
#' @param docfolder folder the documentation should be written to relative to model folder
#' @return a character vector containing the produced markdown text
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}
#' @export


createModulePage <- function(data, docfolder) {
  out <- NULL
  zz <- textConnection("out", open = "w", local = TRUE)

  .header(zz, paste0(data$title, " (", data$name, ")"), 1, id = data$name)

  .section(data$description, zz, "Description", 2)

  .header(zz, "Interfaces", 2)

  .interfaceplot(zz, data$name, docfolder)

  .section(data$input, zz, "Input", 3)

  .section(data$output, zz, "Output", 3)

  .header(zz, "Realizations", 2)

  rdata <- data$realizations
  i <- 1
  for (r in names(rdata)) {
    title <- paste0("(", LETTERS[i], ") ", r)
    .header(zz, title, 3)
    .write(zz, rdata[[r]]$description)
    .limitations(zz, rdata[[r]]$limitations)
    i <- i + 1
  }

  .limitations(zz, data$limitations, emptyIfNULL = TRUE)

  if (any(!is.null(data$declarations), !is.null(data$sets))) {
    .header(zz, "Definitions", 2)

    .section(data$declarations, zz, "Objects", 3)

    .section(data$sets, zz, "Sets", 3)
  }

  .section(data$authors, zz, "Authors", 2)

  if (!is.null(data$seealso)) {
    data$seealso <- paste0("[", sort(data$seealso), "]", collapse = ", ")
  }

  .section(data$seealso, zz, "See Also", 2)

  close(zz)

  out <- .updateImagePaths(out)

  return(out)
}
