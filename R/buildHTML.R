#' buildHTML
#'
#' Converts a folder with markdown files and a corresponding literature library
#' (if available) to HTML files and creates cross-links between them.
#'
#' Pandoc (https://pandoc.org/) together with pandoc-citeproc need to be installed
#' on the system.
#'
#' @param style visualization style to be used for the creation. Currently available styles are
#' "classic" and "ming"
#' @param folder location the HTML files should be written to
#' @param mdfolder path to the markdown folder to be used as source
#' @param literature path to a bibliography, if available (will be ignored
#' if file does not exist)
#' @param citation Citation information in citation file format (optional)
#' @param supplementary a vector of files and/or folders required for the conversion
#' (e.g. an images subdirectory with figures to be shown in the documents)
#' @param debug logical which switches on/off a debug mode which will return additional
#' status updates and keep build files
#' @param templatefolder Folder in which goxygen will search for template files in addition to the pre-installed ones.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{goxygen}}, \code{\link{buildTEX}}
#' @importFrom utils packageVersion
#' @importFrom withr local_dir
#' @export

buildHTML <- function(style = "classic", folder = "html", mdfolder = "markdown", literature = "literature.bib",
                      citation = "../CITATION.cff", supplementary = "images", debug = FALSE, templatefolder = "..") {

  # check style
  if (style == "classic") return(oldBuildHTML(folder = folder, mdfolder = mdfolder, literature = literature,
                                              citation = citation, supplementary = supplementary))

  templateFolder   <- chooseTemplate(style, templatefolder)
  templatefileHTML5 <- chooseTemplate(style, templatefolder, "html5")

  message("Start HTML creation...")
  check_pandoc(error = TRUE)

  # check available md files
  files <- list.files(mdfolder, pattern = ".*\\.md$", full.names = TRUE)
  moduleNames <- sub("\\.[^.]*$", "", basename(files))

  # prepare folder
  if (!dir.exists(folder)) dir.create(folder)
  file.copy(templateFolder, folder, recursive = TRUE)
  for (elem in supplementary) file.copy(elem, folder, recursive = TRUE, overwrite = TRUE)

  # prepare reference file
  ref <- ifelse(debug, "ref.md", tempfile()) # nolint
  returnReferences(moduleNames, paste0(moduleNames, ".htm"), ref, level = 2)

  # prepare main navigation
  returnHTMLref <- function(files) {
    out <- data.frame(name = NULL, title = NULL, stringsAsFactors = FALSE)
    for (f in files) {
      tmp <- readLines(f, n = 2)
      # use title if title is detected
      out <- rbind(out,
                   data.frame(title = ifelse(grepl("^=*$", tmp[2]), sub(" *\\(.*$", "", tmp[1]), ""),
                              name = sub(".md", "", basename(f), fixed = TRUE), stringsAsFactors = FALSE))
    }
    return(out)
  }
  mainNav <- function(mdfolder) {
    local_dir(mdfolder)
    ref <- returnHTMLref(dir(".", pattern = "\\.md$"))
    ref$title[ref$title == ""] <- ref$name[ref$title == ""]

    bringToFront <- function(ref, name, newname = NULL) {
      if (name %in% ref$name) {
        # bring index page to the front
        i <- which(ref$name == name)
        ref <- ref[c(i, setdiff(seq_len(nrow(ref)), i)), ]
        # rename index to overview
        if (!is.null(newname)) ref$title[1] <- newname
      }
      return(ref)
    }
    ref <- bringToFront(ref, "core")
    ref <- bringToFront(ref, "index", "Overview")

    hasNumber <- grepl("^[0-9]{1,2}_", ref$name)
    if (sum(hasNumber) >= (nrow(ref) - 2)) {
      # if all names (expect of one, which might be the index page) begin
      # with a number, use that number in front of the title
      number <- as.integer(sub("_.*$", "", ref$name[hasNumber]))
      ref$title[hasNumber] <- paste0(format(number, width = 2), ". ", ref$title[hasNumber])
    }
    return(ref)
  }
  mainNav <- mainNav(mdfolder)

  mainpage <- ifelse("index" %in% mainNav$name, " -V mainpage=\"index.htm\"", "")

  if (is.character(citation) && file.exists(citation)) {
    citation <- read_cff(citation)
  } else if (!is.list(citation)) {
    citation <- list(title = "Model Documentation")
  }
  repo <- ifelse(!is.null(citation$`repository-code`), paste0(" -V repo=", citation$`repository-code`), "")
  bib <- ifelse(file.exists(literature), paste0("--bibliography=", literature), "")
  logo <- ifelse(file.exists(file.path(folder, "images/logo.png")), " -V logo", "")
  authors <- ""
  if (!is.null(citation$authors)) {
    .tmp <- function(x) return(paste(x$`given-names`, x$`family-names`))
    authors <- paste0(" -V author-meta=\"", vapply(citation$authors, .tmp, character(1)), "\"")
    authors <- paste(rev(authors), collapse = "")
  }

  for (m in moduleNames) {
    pagetitle <- mainNav$title[mainNav$name == m]
    if (length(pagetitle) == 0) pagetitle <- m
    ofile <- paste0(folder, "/", m, ".htm")
    pandocCall <- paste0("pandoc ", mdfolder, "/", m, ".md ", ref, " -o ", ofile,
                         " --css template.css ",
                         bib,
                         authors,
                         logo,
                         repo,
                         mainpage,
                         if (any(grepl("--citeproc", system2("pandoc", "--help", stdout = TRUE)))) "--citeproc" else "",
                         " --toc --mathjax --standalone --metadata link-citations=true",
                         " --template=", templatefileHTML5,
                         " --metadata title=", m,
                         " -V modeltitle=\"", citation$title, "\"",
                         " -V goxygenversion=", packageVersion("goxygen"),
                         " -V modelversion=", citation$version,
                         " -V pagetitle=\"", pagetitle, "\"",
                         " -V mainnav=\"", paste0("<a href='", mainNav$name, ".htm'>", mainNav$title, "</a>",
                                                  collapse = "\n"), "\"")
    if (debug) message(pandocCall, "\n") # nolint
    system(pandocCall)
  }
  if (!debug) unlink(ref) # nolint
  message("...finished HTML creation!")
}
