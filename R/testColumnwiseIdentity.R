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
