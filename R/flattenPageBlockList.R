#' flattenPageBlockList
#'
#' A helper that processes additional attributes for a given list of code documentation
#' blocks. Code documentation blocks are described as lists consisting of `content`
#' containing the documentation and a `cfg` list containing attributes.
#'
#' If a block entry has the `cgf` attribute `extrapage`, it is moved to a separate list
#' `extraPageBlocks` in the output, as these need to be rendered separately later.
#'
#' Regular blocks without the `extrapage` attribute are moved to a list `blocks` and multiple
#' blocks with the same name are merged into one block.
#'
#' Cfg attributes other than `extrapage` are currently not supported and therefore
#' ignored, but a warning is thrown.
#'
#' After processing the `cfg` attributes, the code documentation blocks are flattened,
#' i.e. a list consisting of a `content` and `cfg` entry is replaced by the data in `cfg`.
#'
#' This helper supports nesting of blocks in `realizations` with code documentation
#' per realization.
#'
#' @param data a list of documentation pieces with type as name of each element
#' @return a list with two element (1) `blocks` containing the documentation elements
#' with type as name of the element and (2) `extraPageBlocks` containing lists for
#' blocks to be put on an extra pages, sorted by page names.
#'
#' @author Falk Benke
flattenPageBlockList <- function(data) {
  extraPageBlocks <- list()
  tmp <- data
  data <- list()

  for (i in seq_along(tmp)) {

    if (names(tmp[i]) == "realizations") {

      # recursive handling of realizations sublists
      data[["realizations"]] <- list()

      for (j in seq_along(tmp[[i]])) {
        extract <- flattenPageBlockList(tmp[i]$realizations[[j]])
        l <- list(extract$blocks)
        names(l) <- names(tmp[i]$realizations[j])
        data[["realizations"]] <- append(data[["realizations"]], l)
        extraPageBlocks <- appendExtraPageBlocks(extraPageBlocks, extract$extraPageBlocks)
      }

    } else {
      # handle base case
      cfg <- tmp[[i]]$cfg

      # cfg entries other than extrapage are ignored for now
      if (length(setdiff(names(cfg), "extrapage") > 0)) {
        warning(paste0("The following settings are not supported and will be ignored: ",
                       paste0(setdiff(names(cfg), "extrapage"), collapse = ", ")))
      }

      l <- list(tmp[[i]]$content)
      names(l) <- names(tmp)[i]

      if (!is.null(cfg$extrapage)) {
        # extra page blocks are separated from the rest and sorted in sublists per page
        extraPageBlocks[[cfg$extrapage]] <- append(extraPageBlocks[[cfg$extrapage]], l)
      } else {
        data <- append(data, l)
      }
    }
  }

  data <- mergeDocumentation(data)

  return(list(blocks = data, extraPageBlocks = extraPageBlocks))
}
