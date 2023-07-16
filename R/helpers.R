# loadArgs ---------------------------------------------------------------------

#' Load Function Arguments and Results From Files
#'
#' Read the function arguments and function results that were stored in RData
#' files (in objects \code{args} and \code{result}, respectively) by a previous
#' call of \code{\link{saveArgs}}.
#'
#' @param functionName Name of the function to load arguments and results for.
#'   The name is used to create a search pattern for RData files in
#'   \code{data.dir}.
#' @param data.dir Directory in which to look for RData files matching
#'   \code{args_<functionName>_<hhmmss>_<no>.RData}. The default is the
#'   subfolder \code{test} in \code{tempdir()}.
#' @return list with as many items as there were files args_<functionName>_*
#'   in the directory given in \code{data.dir}. Each list element has two
#'   components: \code{args} containing the arguments that were given to
#'   the function and \code{result} containing what the function returned.
#' @export
#' @seealso \code{\link{saveArgs}}
#' @examples
#'
#' # Define a function that stores its arguments and result with saveArgs
#' double <- function(x) {
#'   result <- 2 * x
#'   saveArgs("double", args = list(x = x), result = result)
#'   result
#' }
#'
#' # Set global variable TESTMODE to "activate" saveArgs() in double()
#' TESTMODE <- TRUE
#'
#' # Call the function a couple of times
#' double(4)
#' double(-99)
#' double(1:10)
#'
#' # Load what was stored behind the scenes
#' testdata <- loadArgs("double")
#'
#' # "Deactivate" saveArgs() in double()
#' TESTMODE <- FALSE
#'
#' # Rerun the function with the stored arguments
#' results <- lapply(testdata, function(x) do.call("double", x$args))
#'
#' # Compare the new with the old results
#' identical(results, lapply(testdata, "[[", "result"))
loadArgs <- function
(
  functionName = NULL, data.dir = file.path(tempdir(), "test")
)
{
  kwb.utils::safePath(data.dir)

  template <- "^args_NAME_\\d{6}_.*$"

  pattern <- gsub(
    "NAME",
    kwb.utils::defaultIfNULL(functionName, ".*"),
    template
  )

  data.files <- dir(data.dir, pattern, full.names = TRUE)

  if (!length(data.files)) {
    stop(
      "No files matching '", pattern, "' found in '", data.dir, "'. ",
      "Available files: ", kwb.utils::stringList(dir(data.dir))
    )
  }

  getBasename <- function(x, short) {
    subst1 <- list("^args_" = "")
    subst2 <- if (short)
      list("\\d{6}_\\d+\\.RData$" = "")
    else
      list("\\.RData$" = "")
    kwb.utils::multiSubstitute(basename(x), c(subst1, subst2))
  }

  if (is.null(functionName)) {

    functionNames <- unique(getBasename(data.files, short = TRUE))

    stop(
      "No function name given. Functions for which arguments are stored ",
      "in ", data.dir, ": ", kwb.utils::stringList(functionNames)
    )
  }

  data.files <- data.files[grepl(pattern, basename(data.files))]

  result <- lapply(data.files, function(data.file) {

    list(
      args = kwb.utils::loadObject(data.file, "args"),
      result = kwb.utils::loadObject(data.file, "result")
    )
  })

  structure(result, names = getBasename(data.files, short = FALSE))
}

# saveArgs ---------------------------------------------------------------------

#' Save the Arguments and Result of a Function Call
#'
#' Save the list of named arguments given in \code{...} to an RData file
#' \code{args_<functionName>_<hhmmss>_<no>.RData} in the directory given in
#' \code{targetdir}. This function can be used to log the inputs given to a
#' function together with the result returned by the function.
#' \code{\link{test_function}} can then be used to check whether another version
#' of the function (e.g. obtained by code cleaning) can reproduce the stored
#' results from the stored arguments. Check out the example on the help page for
#' \code{\link{test_function}}.
#'
#' @param functionName name of the function to which the arguments to be saved
#'   belong. It will be used to generate a file name for the RData file.
#' @param ... named arguments representing the arguments that have been given
#'   to the function \code{functionName}.
#' @param targetdir directory in which to store the objects given in \code{...}
#'   Default: subdirectory \code{test} in \code{tempdir()}
#' @return path to the file written (invisibly)
#' @export
#' @seealso \code{\link{loadArgs}}
saveArgs <- function
(
  functionName,
  ...,
  targetdir = kwb.utils::createDirectory(file.path(tempdir(), "test"))
)
{
  if (! exists("TESTMODE")) {

    prompt <- paste0(
      "[Set global variable TESTMODE to FALSE to prevent this message]\n",
      sprintf("Save args to '%s' (y, n)? ", functionName)
    )

    TESTMODE <- (readline(prompt) == "y")
  }

  if (TESTMODE) {

    timestring <- format(Sys.time(), "%H%M%S")
    filename <- paste0("args_", functionName, "_", timestring, "_0")

    # Make the name unique within the existing files in targetdir (generated
    # within the same second)
    filenames <- gsub("\\.RData$", "", dir(targetdir))
    filename <- kwb.utils::hsSafeName(filename, filenames)

    file <- file.path(targetdir, paste0(filename, ".RData"))

    cat("saving args to", file, "... ")
    args <- list(...)
    save(file = file, list = names(args), envir = list2env(args))
    cat("ok.\n")

    # Return (invisibly) the path to the file to which data was stored
    invisible(file)
  }
}
