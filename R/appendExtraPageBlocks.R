#' appendExtraPageBlocks
#'
#' A helper to merge two nested lists for extra page blocks.
#' The lists have the page name on the first level and flattened documentation
#' blocks on the second level. It is ensured that elements for the
#' same page are grouped in the same list.
#'
#' @param blocks a nested list for extra page blocks per page
#' @param add a seccond nested list for extra page blocks per page to be appended
#'
#' @author Falk Benke
appendExtraPageBlocks <- function(blocks, add) {
  for (k in seq_along(add)) {
    page <- names(add[k])
    if (page %in% names(blocks)) {
      blocks[[page]] <- append(blocks[[page]], add[[k]])
    } else {
      blocks[[page]] <- add[[k]]
    }
  }
  return(blocks)
}
