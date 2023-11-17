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
