# create_test_files ------------------------------------------------------------

#' Create Test Files
#'
#' Create test files for each source file containing one
#'   \code{\link[testthat]{test_that}} call for each function in the package
#'
#' Existing test files will not be overwritten.
#'
#' @param package_dir path to package directory in which to create the test
#'   files
#' @param target_dir directory in which to create the test files. Defaults to
#'   \code{<package_dir>/tests/testthat}.
#' @param file_per_function if \code{TRUE} (default), one test file
#'   \code{test-<function>.R} is generated for each function, otherwise one test
#'   file \code{test-<source-file>} is generated for each source file.
#' @param full if \code{TRUE}, test calls with many argument combinations are
#'   generated instead of only one call
#' @param dbg if \code{TRUE}, debug messages are shown
#'
#' @export
#' @importFrom usethis use_testthat
#' @importFrom kwb.utils createDirectory
#'
create_test_files <- function(
  package_dir = getwd(), target_dir = NULL, file_per_function = TRUE,
  full = FALSE, dbg = TRUE
)
{
  #package_dir = getwd(); file_per_function = TRUE; full = FALSE; dbg = TRUE

  pkg_name <- basename(package_dir)

  old_dir <- setwd(package_dir)

  on.exit(setwd(old_dir))

  usethis::use_testthat()

  scripts <- dir("R", pattern = "^[^.].*\\.[rR]$", full.names = TRUE)

  if (is.null(target_dir)) {
    target_dir <- kwb.utils::createDirectory("tests/testthat", dbg = dbg)
  }

  #script <- scripts[3]

  for (script in scripts) {

    create_tests_for_file(
      script,
      test_dir = target_dir,
      pkg_name = pkg_name,
      file_per_function = file_per_function,
      full = full,
      dbg = dbg
    )
  }
}

# create_tests_for_file --------------------------------------------------------
#' @importFrom kwb.utils resolve user
create_tests_for_file <- function(
  script,
  test_dir,
  pkg_name,
  file_per_function = TRUE,
  full = FALSE,
  dbg = TRUE
)
{
  # One test file per source file?
  if (!file_per_function) {

    test_file <- sprintf("%s/test-file-%s", test_dir, basename(script))

    if (isTRUE(warn_if_file_exists(test_file))) {
      return()
    }
  }

  # Parse the source file, find the function definitions and generate test
  # code for each function
  codes <- get_test_codes_for_functions_in_file(
    file = script,
    pkg_name = pkg_name,
    test_dir = test_dir,
    full = full
  )

  # Get the text to be put as an introduction in each generated file
  intro <- kwb.utils::resolve(
    "intro",
    get_templates(),
    datetime = Sys.time(),
    user = kwb.utils::user()
  )

  if (file_per_function) {

    # Write one file for each function in the source file
    write_one_file_per_function(codes, test_dir, intro, dbg)

  } else {

    # Write one file for all functions in the source file
    write_test_file(c(intro, do.call(c, codes)), test_file, dbg)
  }
}

# write_one_file_per_function --------------------------------------------------
#' @importFrom kwb.utils getAttribute
write_one_file_per_function <- function(codes, test_dir, intro, dbg = TRUE)
{
  for (code in codes) {

    fun_name <- kwb.utils::getAttribute(code, "fun_name")

    test_file <- path_to_testfile(test_dir, fun_name)

    if (! warn_if_file_exists(test_file)) {
      write_test_file(c(intro, code), test_file, dbg = dbg)
    }
  }
}

# write_test_file --------------------------------------------------------------
#' @importFrom kwb.utils catAndRun
write_test_file <- function(code, test_file, dbg = TRUE)
{
  kwb.utils::catAndRun(dbg = dbg, paste("Writing", test_file), {
    writeLines(code, test_file)
  })
}

