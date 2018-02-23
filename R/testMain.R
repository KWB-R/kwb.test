# test_function ----------------------------------------------------------------

#' Test if Function Reproduces Stored Results
#'
#' Call the function \code{functionName} with the arguments contained in
#' \code{testdata} and compare the results with the results in \code{testdata}
#' for identity.
#' @param functionName Name of the function to test. It must be callable, i.e.
#'   either defined in the global environment or on the search path.
#' @param testdata List of lists containing function arguments (in elemenet
#' \code{args}) and results (in element \code{result}), just as returned by
#'   \code{\link{loadArgs}}. If no \code{testdata} are given, it is tried to
#'   load test data by calling \code{loadArgs} on \code{functionName}.
#' @param dbg if \code{TRUE} (default) debug messages are shown
#' @return \code{TRUE} If the function \code{functionName} is able to reproduce
#'   the same results as given in the \code{result} elements in \code{testdata}
#'   for all the argument combinations given in the \code{args} elements in
#'   \code{testdata}.
#' @export
#' @examples
#' # Define a function using saveArgs() to save arguments and result
#' squareSum <- function(a, b) {
#'   result <- a * a + b * b
#'   saveArgs("squareSum", args = list(a = a, b = b), result = result)
#'   result
#' }
#'
#' # Set global variable TESTMODE to "activate" saveArgs() in squareSum()
#' TESTMODE <- TRUE
#'
#' # Call the function with different arguments
#' squareSum(1, 2)
#' squareSum(2, 3)
#' squareSum(-1, -2)
#'
#' # The arguments and function results were saved here:
#' dir(file.path(tempdir(), "test"))
#'
#' # Write a new (wrong) version of the function
#' squareSum.new <- function(a, b) {
#'   a * a - b * b
#' }
#'
#' # Check if it returns the same results
#' test_function("squareSum.new", loadArgs("squareSum"))
#'
#' # If no test data are given, loadArgs is called on the function to test,
#' # i.e. testing squareSum on the test data created by the same function will
#' # return TRUE if the function did not change in the meanwhile.
#' test_function("squareSum")
test_function <- function
(
  functionName,
  testdata = loadArgs(functionName, file.path(tempdir(), "test")),
  dbg = TRUE
)
{
  compare.available <- requireNamespace("compare", quietly = TRUE)

  if (length(testdata)) {
    kwb.utils::catIf(dbg, sprintf("Calling '%s' with arguments stored in...\n",
                                  functionName))
  }

  success <- lapply(names(testdata), function(element) {

    args <- testdata[[element]]$args
    result <- testdata[[element]]$result

    kwb.utils::catIf(dbg, "  ", element, "-> ")

    utils::capture.output(my.result <- do.call(functionName, args))

    isIdentical <- identical(result, my.result)

    kwb.utils::catIf(dbg, ifelse(isIdentical, "SAME", "DIFFERENT"), "result.\n")

    if (! isIdentical && compare.available) {

      cat("Comparison with compare::compare:\n")
      print(compare::compare(result, my.result, allowAll = TRUE))
    }

    isIdentical
  })

  if (! compare.available) {
    message("If the 'compare' package was installed I would have also been ",
            "used 'compare::compare' to compare the results")
  }

  all(unlist(success))
}

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

  pattern <- gsub("NAME", kwb.utils::defaultIfNULL(functionName, ".*"),
                  template)

  data.files <- dir(data.dir, pattern, full.names = TRUE)

  if (! length(data.files)) {
    stop("No files matching '", pattern, "' found in '", data.dir, "'. ",
         "Available files: ", kwb.utils::stringList(dir(data.dir)))
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

    stop("No function name given. Functions for which arguments are stored ",
         "in ", data.dir, ": ", kwb.utils::stringList(functionNames))
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

# testColumnwiseIdentity -------------------------------------------------------

#' Check Corresponding Columns in two Data Frames for Identity
#'
#' For all columns in the first data frame, check if the second data frame
#' has identical values in columns of the same name
#'
#' @param ... two data frames given as named arguments. The argument names will
#'   appear in the output. By doing so you can give a longer expression that
#'   returns a data frame a short name 'on-the-fly'.
#' @export
#' @examples
#' # Compare two identical data frames. Give them short names data.1 and data.2
#' testColumnwiseIdentity(data.1 = (x <- data.frame(a = 1:2, b = 2:3)),
#'                        data.2 = x)
#'
#' # Compare two data frames differing in one column
#' testColumnwiseIdentity(A = data.frame(x = 1:2, y = 2:3),
#'                        B = data.frame(x = 1:2, y = 3:4))
testColumnwiseIdentity <- function(...)
{
  args <- list(...)
  stopifnot(length(args) == 2)

  x <- args[[1]]
  y <- args[[2]]

  objectnames <- names(args)

  for (column in names(x)) {

    printTestMessage(
      sprintf(#"identical(%s[[\"%s\"]], %s[[\"%s\"]])",
              "identical(%s[, \"%s\"], %s[, \"%s\"])",
              objectnames[1], column, objectnames[2], column),
      identical(kwb.utils::selectColumns(x, column),
                kwb.utils::selectColumns(y, column)),
      newline = FALSE
    )
  }
}

# printTestMessage -------------------------------------------------------------

#' Print a Test with its Result
#'
#' Print a test with its result as a message and return the message as a
#' character string
#'
#' @param testexpression text description of what was tested
#' @param testresult boolean result (of length one) of the test
#' @param newline if \code{TRUE} (default) a new line character is appended
#'   to the message shown.
#' @return the message that was shown as a character string
#' @export
#' @examples
#' printTestMessage("apple == apple", 1 == 1)
#' printTestMessage("apple == pear", 1 == 2)
printTestMessage <- function(testexpression, testresult, newline = TRUE)
{
  message(sprintf(paste0("%s? %s", ifelse(newline, "\n", "")),
                  testexpression, testresult))
  testresult
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
