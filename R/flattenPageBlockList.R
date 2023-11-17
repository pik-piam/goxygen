flattenPageBlockList <- function(data) {
  extraPageBlocks <- list()
  tmp <- data
  data <- list()

  for (i in seq_along(tmp)) {

    # recursive handling of realizations sublists
    if (names(tmp[i]) == "realizations") {

      for (j in seq_along(tmp[[i]])) {
        extract <- flattenList(tmp[i]$realizations[[j]])
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
