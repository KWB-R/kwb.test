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
  source_file, test_dir, pkg_name, file_per_function = TRUE, full = FALSE,
  dbg = TRUE
)
{
  # One test file per source file?
  if (! file_per_function) {

    test_file <- sprintf("%s/test-file-%s", test_dir, basename(source_file))

    if (isTRUE(warn_if_file_exists(test_file))) {
      return()
    }
  }

  # Parse the source file, find the function definitions and generate test
  # code for each function
  codes <- get_test_codes_for_functions_in_file(
    file = source_file,
    pkg_name = pkg_name,
    test_dir = test_dir,
    full = full
  )

  # Get the text to be put as an introduction in each generated file
  intro <- kwb.utils::resolve(
    "intro", get_templates(), datetime = Sys.time(), user = kwb.utils::user()
  )

  if (file_per_function) {

    # Write one file for each function in the source file
    write_one_file_per_function(codes, test_dir, intro, dbg)

  } else {

    # Write one file for all functions in the source file
    write_test_file(c(intro, do.call(c, codes)), test_file, dbg)
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

# path_to_testfile -------------------------------------------------------------
path_to_testfile <- function(test_dir, fun_name)
{
  sprintf("%s/test-function-%s.R", test_dir, fun_name)
}

# get_test_codes_for_functions_in_file -----------------------------------------
get_test_codes_for_functions_in_file <- function(file, pkg_name, test_dir, ...)
{
  # Get the expressions that represent assignments of function definitions
  assignments <- get_function_assignments(file)

  # Name the assignments according to the function names
  names(assignments) <- sapply(assignments, function(x) as.character(x[[2]]))

  # Get the names of the exported functions
  exports <- getNamespaceExports(pkg_name)

  # Create a test_that-call for each function
  kwb.utils::excludeNULL(dbg = FALSE, lapply(
    stats::setNames(nm = names(assignments)),
    FUN = function(fun_name) {

      if (warn_if_file_exists(path_to_testfile(test_dir, fun_name))) {
        return()
      }

      get_test_for_function(
        fun_name = fun_name,
        fun_args = assignments[[fun_name]][[3]][[2]],
        pkg_name = pkg_name,
        exports = exports,
        ...
      )
    }
  ))
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

# get_test_for_function --------------------------------------------------------
get_test_for_function <- function(
  fun_name, fun_args, pkg_name, exports = getNamespaceExports(pkg_name),
  full = FALSE
)
{
  #assignment <- assignments[[1]]

  arg_combis <- if (full) {

    get_arg_combis(arg_names = get_no_default_args(fun_args))

  } else {

    data.frame()
  }

  #fun_name <- as.character(assignment[[2]])

  call_strings <- get_function_call_strings(fun_name, arg_combis, pkg_name)

  exported <- fun_name %in% exports

  get_test_for_function_calls(call_strings, fun_name, pkg_name, exported)
}

# get_test_for_function_calls --------------------------------------------------
#' @importFrom kwb.utils collapsed getAttribute resolve
get_test_for_function_calls <- function(
  call_strings, fun_name, pkg_name, exported
)
{
  templates_raw <- get_templates()

  # Remove the calls that generate the same error messages as previous calls
  fail_indices <- which_calls_fail(call_strings, dbg = FALSE)

  success_indices <- setdiff(seq_along(call_strings), fail_indices)

  fail_indices <- remove_duplicated_fails(fail_indices)

  errors <- kwb.utils::getAttribute(fail_indices, "errors")

  errors <- sapply(errors, get_error_message)

  full_fun_name <- kwb.utils::resolve(
    ifelse(exported, "pkg_fun_exported", "pkg_fun_private"),
    templates_raw,
    fun = fun_name,
    pkg = pkg_name
  )

  pattern <- paste0("(^|\\s)", full_fun_name, "\\(")

  use_shortcut <- function(x) gsub(pattern, "f(", x)

  expect_calls_fail <- sapply(seq_along(fail_indices), function(i) {

    kwb.utils::resolve(
      "fun_call_error",
      templates_raw,
      fun_call = use_shortcut(call_strings[fail_indices[i]]),
      quoted_error = gsub("\n", "\n# ", errors[i])
    )
  })

  expect_calls_success <- sapply(success_indices, function(i) {

    kwb.utils::resolve(
      "fun_call_alone",
      templates_raw,
      fun_call = use_shortcut(call_strings[i])
    )
  })

  #call_strings[fails] <- sprintf("expect_error(%s)", call_strings[fails])
  #test_that_body <- paste0("  ", call_strings, collapse = "\n")

  shortcut <- get_shortcut_assignment(templates_raw, fun_name, pkg_name)

  test_that_body <- paste0(
    "  ", shortcut, "\n\n",
    kwb.utils::collapsed(c(expect_calls_success, expect_calls_fail))
  )

  test_that_call <- kwb.utils::resolve(
    "test_that_call",
    templates_raw,
    fun = fun_name,
    #pkg = pkg_name,
    #pkg_fun = "f", #ifelse(exported, "<pkg_fun_exported>", "<pkg_fun_private>"),
    test_that_body = paste0(test_that_body, "\n")
  )

  structure(test_that_call, fun_name = fun_name)
}

# get_shortcut_assignment ------------------------------------------------------
get_shortcut_assignment <- function(templates, fun_name, pkg_name)
{
  sprintf(
    "f <- %s",
    kwb.utils::selectElements(
      kwb.utils::resolve(templates, fun = fun_name, pkg = pkg_name),
      ifelse(pkg_name == "", "pkg_fun_exported", "pkg_fun_private")
    )
  )
}

# single_quoted ----------------------------------------------------------------
single_quoted <- function(x)
{
  paste0("'", gsub("'", "\\\\'", x), "'")
}

# get_error_message ------------------------------------------------------------
get_error_message <- function(error)
{
  if (inherits(error, "error")) {

    error$message

  } else {

    error
  }
}

# remove_duplicated_fails ------------------------------------------------------
#' @importFrom kwb.utils getAttribute
remove_duplicated_fails <- function(fails)
{
  errors <- kwb.utils::getAttribute(fails, "errors")

  keep <- ! duplicated(sapply(errors, get_error_message))

  structure(fails[keep], errors = errors[keep])
}

# get_templates ----------------------------------------------------------------
get_templates <- function()
{
  templates <- list(
    intro = "<intro_1>\n<intro_2>\n<intro_3>\n<intro_4>",
    intro_1 = "#\n# This test file has been generated by <test_creator>",
    intro_2 = "# launched by user <user> on <datetime>.",
    intro_3 = "# Your are strongly encouraged to modify the dummy functions",
    intro_4 = "# so that real cases are tested. <hint_delete>\n#\n",
    hint_delete = "You should then delete this comment.",
    test_creator = "kwb.test::create_test_files()",
    test_that_call = "test_that(\"<fun>() works\", {\n\n<test_that_body>})",
    fun_call = "<pkg_fun>(<args>)",
    fun_call_alone = "  <fun_call>\n",
    fun_call_error = "<i1>expect_error(\n<expect_error_args>\n<i1>)\n",
    expect_error_args = "<i2><fun_call>\n<i2># <quoted_error>",
    fun_call_message = "<i1>expect_message(<fun_call>)\n",
    fun_call_silent = "<i1>expect_silent(<fun_call>)\n,",
    pkg_fun_exported = "<fun>",
    pkg_fun_private = "<pkg>:::<fun>",
    i2 = "<i1><i1>",
    i1 = "  "
  )
}

# which_calls_fail -------------------------------------------------------------
which_calls_fail <- function(call_strings, dbg = TRUE)
{
  results <- lapply(call_strings, function(call_string) {

    tryCatch(eval_text(call_string, dbg), error = identity)
  })

  is_error <- sapply(results, inherits, "simpleError")

  structure(which(is_error), errors = results[is_error])
}

# eval_text --------------------------------------------------------------------
#' @importFrom kwb.utils catAndRun
eval_text <- function(text, dbg = TRUE)
{
  kwb.utils::catAndRun(dbg = dbg, paste0("Evaluating:\n ", text, "\n"), {
    eval(parse(text = text))
  })
}

# get_function_call_strings ----------------------------------------------------
#' @importFrom kwb.utils asColumnList resolve
get_function_call_strings <- function(fun_name, arg_combis, pkg_name = "")
{
  arg_strings <- ""

  if (nrow(arg_combis) > 0) {

    arg_combi_list <- kwb.utils::asColumnList(as.matrix(arg_combis))

    assignment <- function(name) paste(name, "=", arg_combi_list[[name]])

    paste_args <- c(lapply(names(arg_combi_list), assignment), sep = ", ")

    arg_strings <- do.call(paste, paste_args)
  }

  sprintf(
    "%s(%s)",
    kwb.utils::selectElements(
      kwb.utils::resolve(get_templates(), fun = fun_name, pkg = pkg_name),
      ifelse(pkg_name == "", "pkg_fun_exported", "pkg_fun_private")
    ),
    arg_strings
  )
}

# get_arg_combis ---------------------------------------------------------------
#' @importFrom kwb.utils expandGrid
get_arg_combis <- function(arg_names, max_args = 2)
{
  string_values <- c(
    "1", "1:2",
    '"a"', 'c("a", "b")',
    "TRUE", "FALSE",
    'as.POSIXct("2018-06-03 23:50:00")',
    'list(key = c("a", "b"), value = 1:2)'
  )

  n <- min(max_args, length(arg_names))

  if (n == 1) {

    matrix(string_values, ncol = 1, dimnames = list(NULL, arg_names))

  } else {

    f <- rep(seq_len(n), each = length(string_values))

    arguments <- split(rep(string_values, n), f = f)

    names(arguments) <- arg_names[seq_len(n)]

    do.call(kwb.utils::expandGrid, arguments)
  }
}

# get_no_default_args ----------------------------------------------------------
get_no_default_args <- function(arguments)
{
  if (! is.null(arguments)) {

    names(which(sapply(arguments, is.symbol)))
  }
}
