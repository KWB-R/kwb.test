# M A I N ----------------------------------------------------------------------
if (FALSE)
{
  # install.packages("usethis")

  create_test_files(package_dir = "~/RProgramming/github/kwb.dwa.m150")
}

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
#' @param dbg if \code{TRUE}, debug messages are shown
#'
#' @export
#'
create_test_files <- function(package_dir = getwd(), dbg = TRUE)
{
  package_name <- basename(package_dir)

  old_dir <- setwd(package_dir)

  on.exit(setwd(old_dir))

  usethis::use_testthat()

  source_files <- file.path("R", dir("R"))

  test_dir <- file.path("tests", "testthat")

  test_dir <- kwb.utils::createDirectory(test_dir, dbg = dbg)

  #source_file <- source_files[1]

  for (source_file in source_files) {

    test_code <- get_code_to_test_source_file(file = source_file, package_name)

    base_name <- kwb.utils::removeExtension(basename(source_file))

    test_file <- file.path(test_dir, sprintf("test-%s.R", base_name))

    if (file.exists(test_file)) {

      message(test_file, " exists.\nI will not overwrite it.")

    } else {

      kwb.utils::catIf(dbg, "Writing", test_file, "... ")

      writeLines(test_code, test_file)

      kwb.utils::catIf(dbg, "ok.\n")
    }
  }
}

# get_code_to_test_source_file -------------------------------------------------
get_code_to_test_source_file <- function(file, package_name)
{
  assignments <- get_function_assignments(file)

  do.call(c, lapply(assignments, get_test_for_function_assignment, package_name))
}

# get_function_assignments -----------------------------------------------------
get_function_assignments <- function(file)
{
  code <- as.list(parse(file))

  #expr <- code[[2]]

  is_function_assignment <- sapply(code, function(expr) {
    ok <- as.character(expr[[1]]) == "<-"
    ok && length(expr) >= 3 && as.character(expr[[3]][[1]]) == "function"
  })

  code[is_function_assignment]
}

# get_test_for_function_assignment ---------------------------------------------
get_test_for_function_assignment <- function(assignment, package_name)
{
  function_name <- as.character(assignment[[2]])

  template_pattern <- paste(
    "test_that(\"%s() works\", {",
    "",
    "  expect_error(%s:::%s())",
    "})",
    "",
    sep = "\n"
  )

  sprintf(template_pattern, function_name, package_name, function_name)
}
