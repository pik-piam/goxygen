#' createListSimpleCode
#'
#' support function to create documentation of non-modular GAMS code.
#'
#' @param path path to the model to be documented
#' @param citation citation data read from a CFF file
#' @param mainfile main file of the model
#' @importFrom withr local_dir
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{codeCheck}}

createListSimpleCode <- function(path = ".", citation = NULL, mainfile = "main.gms") {

  local_dir(path)

  # write doc files
  full <- list()

  data <- extractDocumentation(mainfile)
  data <- flattenPageBlockList(data)
  extraPageBlocks <- data$extraPageBlocks
  data <- data$blocks

  data$citation <- citation
  full[["index"]] <- createIndexPage(data)

  files <- setdiff(list.files(pattern = "\\.gms$", recursive = TRUE), mainfile)
  for (f in files) {
    fname <- sub("\\.gms$", "", gsub("/", "_", f, fixed = TRUE))
    data <- extractDocumentation(f)
    extract <- flattenPageBlockList(data)
    data <- extract$blocks

    extraPageBlocks <- appendExtraPageBlocks(extraPageBlocks, extract$extraPageBlocks)

    if (length(data) > 0) {
      if (is.null(data$title)) data$title <- fname
      data$name <- f
      full[[fname]] <- createSimplePage(data)
    }
  }

  for (i in names(extraPageBlocks)) {
    data <- mergeDocumentation(extraPageBlocks[[i]])
    data$name <- i
    full[[i]] <- createSimplePage(data)
  }

  return(full)
}
