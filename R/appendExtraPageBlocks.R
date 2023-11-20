#' appendExtraPageBlocks
#'
#' A helper to merge two nested lists for extra page blocks.
#' The lists are nested with lists for each page name on the first level and
#' all flattened documentation blocks on the second level.
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
