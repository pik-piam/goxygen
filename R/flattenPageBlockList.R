#' flattenPageBlockList
#'
#' A helper that processes additional attributes for a given list of code documentation
#' blocks. Code documentation blocks are described as lists consisting of `content`
#' containing the documentation and a `cfg` list containing attributes.
#'
#' Supports sublists with code documentation per realization.#'
#'
#' @param data a list of documentation pieces with type as name of each element
#' @return a list with two elements (1) `blocks` containing the documentation elements
#' with type as name of the element and (2) `extraPageBlocks` containing lists for
#' blocks to be put on an extra pages, sorted by page names
#'
#' @author Falk Benke
flattenPageBlockList <- function(data) {
  extraPageBlocks <- list()
  tmp <- data
  data <- list()

  for (i in seq_along(tmp)) {

    # recursive handling of realizations sublists
    if (names(tmp[i]) == "realizations") {

      for (j in seq_along(tmp[[i]])) {
        extract <- flattenPageBlockList(tmp[i]$realizations[[j]])
        l <- list(extract$blocks)
        names(l) <- names(tmp[i]$realizations[j])
        data <- append(data, l)
        extraPageBlocks <- appendExtraPageBlocks(extraPageBlocks, extract$extraPageBlocks)
      }

      # handle base case
      # separate extra page blocks from the rest and flatten the lists
    } else {

      cfg <- tmp[[i]]$cfg

      # cfg entries other than extrapage are ignored for now
      if (length(setdiff(names(cfg), "extrapage") > 0)) {
        warning(paste0("The following settings are not supported and will be ignored: ",
                       paste0(setdiff(names(cfg), "extrapage"), collapse = ", ")))
      }

      l <- list(tmp[[i]]$content)
      names(l) <- names(tmp)[i]

      if (!is.null(cfg$extrapage)) {
        # extra page blocks are sorted in sublists per page
        extraPageBlocks[[cfg$extrapage]] <- append(extraPageBlocks[[cfg$extrapage]], l)
      } else {
        data <- append(data, l)
      }
    }
  }

  data <- mergeDocumentation(data)

  return(list(blocks = data, extraPageBlocks = extraPageBlocks))
}
