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
    kwb.utils::catIf(dbg, sprintf(
      "Calling '%s' with arguments stored in...\n",
      functionName
    ))
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

  if (!compare.available) {
    message(
      "If the 'compare' package was installed I would have also been ",
      "used 'compare::compare' to compare the results"
    )
  }

  all(unlist(success))
}
