#' createListModularCode
#'
#' support function to create documentation of modular GAMS code.
#'
#' @param cc codeCheck information
#' @param interfaces interface information
#' @param path path to the model to be documented
#' @param citation citation data read from a CFF file
#' @param unitPattern pattern that is usedto identify the unit in the description, default =c("\\(","\\)")
#' @param includeCore Boolean whether core should be included or not, default=FALSE
#' @param mainfile main file of the model
#' @param docfolder folder the documentation should be written to relative to model folder
#' @author Jan Philipp Dietrich
#' @importFrom stringi stri_extract_all_regex stri_replace_all_regex stri_write_lines
#' @importFrom gms codeCheck modules_interfaceplot is.modularGAMS
#' @importFrom pander pandoc.table.return
#' @importFrom citation read_cff cff2bibentry
#' @importFrom yaml as.yaml
#' @importFrom utils tail toBibtex capture.output
#' @importFrom withr local_dir
#' @seealso \code{\link{codeCheck}}

createListModularCode <- function(cc, interfaces, path = ".", citation = NULL, unitPattern = c("\\(", "\\)"), # nolint
                                  includeCore = FALSE, mainfile = "main.gms", docfolder = "doc") {

  local_dir(path)

  collectTables <- function(cc, unitPattern) {
    .merge <- function(dec) {
      .cleanunit <- function(unit) {
        if (length(unit) == 0) return(unit)
        unit[!grepl(pattern, dec[, "description"])] <- ""
        unit <- sub("[Mm]i(lli|)on?\\.?", "10^6", unit)
        unit <- gsub("\\\\", "/", unit)
        unit <- gsub("$", "\\$", unit, fixed = TRUE)
        unit <- gsub("%", "\\%", unit, fixed = TRUE)
        unit <- gsub(" per ", "/", unit)
        unit <- gsub("USD([0-9]*(PPP|MER))", "USD_{\\1}", unit)
        unit <- paste0("$", unit, "$")
        unit[unit == "$$"] <- ""
        return(unit)
      }
      # create pattern to identify the unit in the describtion
      pattern <- paste0("^(.*) ", unitPattern[1], "(.*)", unitPattern[2], " *(|\\/.*\\/ *)$")
      description <- sub(pattern, "\\1", dec[, "description"])
      unit <- sub(pattern, "\\2", dec[, "description"])
      return(data.frame(name = dec[, "names"], sets = dec[, "sets"], description = description,
                        unit = .cleanunit(unit), stringsAsFactors = FALSE))
    }
    .format <- function(out, aps, ifs = NULL) {
      if (nrow(out) == 0 &&  is.null(ifs)) return(NULL)
      if (nrow(out) == 0 && !is.null(ifs)) return(list(input = NULL, output = NULL))

      # format information
      fout <- data.frame(Name = paste0(out$name, sub("()", "", paste0(" (", out$sets, ")"), fixed = TRUE)),
                         Description = out$description,
                         Unit = out$unit)
      aps[, ] <- ifelse(aps == 1, "x", "")
      fout <- cbind(fout, aps)
      if (is.null(ifs)) return(fout)
      return(list(input = fout[ifs[names(ifs) == "in"], ], output = fout[ifs[names(ifs) == "out"], 1:3]))
    }
    .clean <- function(x, caption, keepNames = 1:3, braketBreak = TRUE) {
      if (is.null(x)) return(NULL)
      if (nrow(x) == 0) return(NULL)
      sub <- NULL
      j <- 1
      for (i in setdiff(seq_along(x), keepNames)) {
        sub <- c(sub, paste0(toupper(letters[j]), ": ", names(x)[i]))
        names(x)[i] <- toupper(letters[j])
        j <- j + 1
      }
      if (!is.null(sub)) caption <- paste0(caption, " (", paste(sub, collapse = " | "), ")")
      rownames(x) <- make.unique(as.character(x[[1]]))
      rownames(x) <- gsub("\\,([^ ])", ", \\1", rownames(x))
      if (braketBreak) rownames(x) <- sub("(", "\\ \n(", rownames(x), fixed = TRUE)
      x <- x[sort(rownames(x)), ]
      x <- x[!grepl("^o[qv]", rownames(x)), ]
      split.cells <- c(15, 30, rep(1, length(x) - 2))
      return(pandoc.table.return(x[-1], "pandoc", caption = caption, split.tables = 100, split.cells = split.cells,
                                 emphasize.rownames = FALSE,  keep.line.breaks = TRUE))
    }
    interfaceTables <- function(cc, module) {
      # collect information about module interfaces
      ifs <- cc$interfaceInfo[[module]]
      ifs <- sort(ifs)
      dec <- cc$declarations[cc$declarations$names %in% ifs, , drop = FALSE]
      dec <- dec[!duplicated(dec$names), , drop = FALSE]
      aps <- cc$appearance[ifs, grepl(paste0("^", module, "\\."), colnames(cc$appearance)), drop = FALSE]
      colnames(aps) <- sub("^.*\\.", "", colnames(aps))
      aps <- aps[dec$names, , drop = FALSE]
      aps <- aps[!duplicated(rownames(aps)), , drop = FALSE]

      out  <- .merge(dec)
      out <- .format(out, aps, ifs)

      out$input  <- .clean(out$input, "module inputs")
      out$output <- .clean(out$output, "module outputs")
      return(out)
    }
    moduleTables <- function(cc, module) {
      # collect information about module interfaces
      dec <- cc$declarations[grepl(paste0("^", module, "\\."), cc$declarations$origin), , drop = FALSE]
      dec <- dec[dec$type != "set", ]
      dec <- dec[order(dec$names), , drop = FALSE]
      if (nrow(dec) == 0) return(NULL)
      dec <- dec[!(dec[, "names"] %in% cc$interfaceInfo[[module]]), , drop = FALSE]
      dec <- dec[!duplicated(dec[, "names"]), , drop = FALSE]

      aps <- cc$appearance[dec$names, grepl(paste0("^", module, "\\."), colnames(cc$appearance)), drop = FALSE]
      colnames(aps) <- sub("^.*\\.", "", colnames(aps))
      aps <- aps[!duplicated(rownames(aps)), , drop = FALSE]

      out <- .merge(dec)
      out <- .format(out, aps)
      return(.clean(out, "module-internal objects"))
    }

    setTables <- function(cc, module) {
      tmp <- cc$appearance[, grep(paste0(module, "."), colnames(cc$appearance), fixed = TRUE), drop = FALSE]
      elems <- rownames(tmp)[rowSums(tmp) > 0]
      elems <- grep("^o[qv]", elems, invert = TRUE, value = TRUE) # remove output objects
      sets <- cc$declarations$sets[cc$declarations$names %in% elems]
      sets <- unique(unlist(strsplit(sets, ",")))

      if (!is.null(cc$setappearance)) {
        # alternative method to find sets
        aps <- cc$setappearance[, grepl(paste0("^", module, "\\."), colnames(cc$setappearance)), drop = FALSE]
        aps <- aps[rowSums(aps) > 0, , drop = FALSE]
        sets2 <- rownames(aps)
        sets <- union(sets, sets2)
      }

      dec <- cc$declarations[cc$declarations$names %in% sets, ]
      dec <- dec[order(dec$names), , drop = FALSE]
      if (nrow(dec) == 0) return(NULL)
      dec <- dec[!duplicated(dec[, "names"]), , drop = FALSE]
      dec$names <- sub("\\(\\)$", "", paste0(dec$names, "(", dec$sets, ")"))

      return(.clean(dec[c("names", "description")], "sets in use", braketBreak = FALSE))
    }

    modInfo <- rbind(cc$modulesInfo, core = c(name = "core", number = "", folder = "core", realizations = ""))
    out <- list()
    for (m in names(cc$interfaceInfo)) {
      out[[modInfo[m, "folder"]]] <- interfaceTables(cc, m)
      out[[modInfo[m, "folder"]]]$declarations <- moduleTables(cc, m)
      out[[modInfo[m, "folder"]]]$sets <- setTables(cc, m)
    }
    return(out)
  }

  collectRealizations <- function(m, cc, modules = "modules/") {
    m <- sub("[0-9]*_", "", m)
    if (m == "core") {
      outSub <- list()
      outSub$realizations <- list()
      files <- list.files(path = "core", pattern = "\\.gms")
      paths <- file.path("core", files)

      documentation <- extractDocumentation(paths, start_type = "equations")

      # move extrapage items to toplevel
      outSub$extrapage <- append(outSub$extrapage, documentation$extrapage)
      documentation$extrapage <- NULL

      outSub$realizations[["core"]] <- documentation

    } else {
      rea <- strsplit(cc$modulesInfo[m, "realizations"], ",")[[1]]
      folder <- cc$modulesInfo[m, "folder"]
      modulegms <- paste0(modules, folder, "/module.gms")
      if (!file.exists(modulegms)) modulegms <- paste0(modules, folder, "/", folder, ".gms")
      outSub <- extractDocumentation(modulegms)
      outSub$realizations <- list()
      for (r in rea) {
        rmain <- paste0(modules, folder, "/", r, "/realization.gms")
        if (!file.exists(rmain)) rmain <- paste0(modules, folder, "/", r, ".gms")
        mentionedFiles <- sub(".*/([^.]*)\\.gms.*$", "\\1.gms",
                              grep(".gms", readLines(rmain), value = TRUE, fixed = TRUE))
        mentionedPaths <- c(rmain, paste0(modules, folder, "/", r, "/", mentionedFiles))
        existingPaths <- union(rmain, dir(paste0(modules, folder, "/", r), pattern = "\\.gms$", full.names = TRUE))

        # create path vector by taking all existingPaths (removing non-existing paths)
        # but ordering it based on the order of mention in realization.gms. Not
        # mentioned files will be added at the end.
        paths <- union(intersect(mentionedPaths, existingPaths), existingPaths)

        documentation <- extractDocumentation(paths, start_type = "equations")

        # move extrapage items to toplevel
        outSub$extrapage <- append(outSub$extrapage, documentation$extrapage)
        documentation$extrapage <- NULL

        outSub$realizations[[r]] <- documentation
      }
    }
    return(outSub)
  }

  collectSeealso <- function(interfaces, module, modulesInfo) {
    module <- sub("^.*_", "", module)
    seealso <- setdiff(unique(c(interfaces$to, interfaces$from)), module)
    modulesInfo <- rbind(modulesInfo, core = c("core", "", "core", ""))
    seealso <- modulesInfo[seealso, "folder"]
    return(seealso)
  }

  sortExtraPages <- function(extraPage) {
    out <- list()
    attrPattern <- "^(\\w+)=\"(\\w+)\"$"

    for (i in seq(extraPage)) {
      content <- extraPage[[i]][-1]
      page <- sub(attrPattern, "\\2", extraPage[[i]][1])
      type <- sub("-\\w+", "", names(extraPage[i]))
      l <- list(content)
      names(l) <- type
      out[[page]] <- append(out[[page]], l)
    }
    return(out)
  }

  out <- collectTables(cc, unitPattern)

  # write doc files
  full <- list()

  data <- extractDocumentation(mainfile)
  extraPage <- data$extrapage
  data$extrapage <- NULL

  data$citation <- citation
  full[["index"]] <- createIndexPage(data)

  # take only all modules into account or also core
  if (includeCore) {
    mLoop <- sort(names(out))
  } else {
    mLoop <- setdiff(sort(names(out)), "core")
  }

  for (m in mLoop) {
    realizations <- collectRealizations(m, cc)
    extraPage <- append(extraPage, realizations$extrapage)
    realizations$extrapage <- NULL
    data <- append(out[[m]], realizations)
    data$name <- m
    data$seealso <- collectSeealso(interfaces[[m]], m, cc$modulesInfo)
    full[[m]] <- createModulePage(data, docfolder = docfolder)
  }

  #browser()
  extraPage <- sortExtraPages(extraPage)
  for (i in names(extraPage)) {
    data <- mergeDocumentation(extraPage[[i]])
    data$name <- i
    full[[i]] <- createModulePage(data, docfolder = docfolder)
  }

  return(full)
}
