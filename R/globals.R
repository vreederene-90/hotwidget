options(shiny.maxRequestSize = 30 * 1024^2)

cached_func_with_file <- function(
    dir,
    file,
    func,
    case.sensitive = FALSE) {
  dir <- normalizePath(dir, mustWork = TRUE)
  value <- NULL
  filePattern <- getOption(
    "shiny.autoreload.pattern",
    ".*\\.(r|html?|js|css|png|jpe?g|gif)$"
  )
  last_mtimes <- NULL
  function(...) {
    file.path.func <- if (case.sensitive) file.path else file.path.ci
    fname <- file.path.func(dir, file)
    files <- list.files(dir, filePattern, recursive = TRUE, ignore.case = TRUE)
    files <- sort_c(files)
    mtimes <- file.info(files)$mtime
    names(mtimes) <- files
    if (!identical(last_mtimes, mtimes)) {
      value <<- func(fname, ...)
      last_mtimes <<- mtimes
    }
    value
  }
}

shiny_env <- environment(shiny:::cachedFuncWithFile)
unlockBinding("cachedFuncWithFile", shiny_env)
body(shiny_env$cachedFuncWithFile) <- body(cached_func_with_file)
lockBinding("cachedFuncWithFile", shiny_env)

loadSupport <- function(
    appDir = NULL,
    renv = new.env(parent = globalenv()),
    globalrenv = globalenv()) {
  require(shiny)
  if (is.null(appDir)) {
    appDir <- findEnclosingApp(".")
  }
  descFile <- file.path.ci(appDir, "DESCRIPTION")
  if (file.exists(
    file.path.ci(
      appDir, "NAMESPACE"
    )
  ) ||
  (file.exists(descFile) &&
   identical(
     as.character(read.dcf(descFile, fields = "Type")),
     "Package"
   )
  )
  ) {
    # warning("Loading R/ subdirectory for Shiny application, but this directory appears ",
    #         "to contain an R package. Sourcing files in R/ may cause unexpected behavior.")
  }
  if (!is.null(globalrenv)) {
    globalPath <- file.path.ci(appDir, "global.R")
    if (file.exists(globalPath)) {
      withr::with_dir(appDir, {
        sourceUTF8(basename(globalPath), envir = globalrenv)
      }
      )
    }
  }
  helpersDir <- file.path(appDir, "R")
  disabled <- list.files(
    helpersDir,
    pattern = "^_disable_autoload\\.r$",
    recursive = FALSE, ignore.case = TRUE
  )
  if (length(disabled) > 0) {
    return(invisible(renv))
  }
  helpers <- list.files(
    helpersDir,
    pattern = "\\.[rR]$",
    recursive = FALSE, full.names = TRUE
  )
  helpers <- sort_c(helpers)
  helpers <- normalizePath(helpers)
  withr::with_dir(appDir, {
    lapply(helpers, sourceUTF8, envir = renv)
  }
  )
  invisible(renv)
}

shiny_env <- environment(shiny:::loadSupport)
unlockBinding("loadSupport", shiny_env)
body(shiny_env$loadSupport) <- body(loadSupport)
lockBinding("loadSupport", shiny_env)


