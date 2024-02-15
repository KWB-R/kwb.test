# get_test_codes_for_functions_in_file -----------------------------------------
get_test_codes_for_functions_in_file <- function(
    file, pkg_name, test_dir, full = FALSE
)
{
  # Get the expressions that represent assignments of function definitions
  assignments <- kwb.code::get_function_assignments(file)

  # Exclude functions for which a test file already exists
  {
    file_exists <- sapply(names(assignments), function(fun_name) {
      warn_if_file_exists(path_to_testfile(test_dir, fun_name))
    })

    assignments <- assignments[!file_exists]
  }

  # Get the names of the exported functions
  exports <- getNamespaceExports(pkg_name)

  # Create a test_that-call for each function
  test_calls <- lapply(
    X = stats::setNames(nm = names(assignments)),
    FUN = function(fun_name) {
      arg_strings <- if (full) {
        arg_combis_to_arg_strings(
          arg_combis = get_arg_combis(
            arg_names = get_no_default_args(
              arguments = assignments[[fun_name]][[3L]][[2L]]
            )
          )
        )
      } else {
        ""
      }
      get_test_for_function_calls(
        call_strings = sprintf("%s(%s)", fun_name, arg_strings),
        fun_name = fun_name,
        pkg_name = pkg_name,
        exported = fun_name %in% exports
      )
    }
  )

  # Remove NULL elements
  kwb.utils::excludeNULL(test_calls, dbg = FALSE)
}

# path_to_testfile -------------------------------------------------------------
path_to_testfile <- function(test_dir, fun_name)
{
  sprintf("%s/test-function-%s.R", test_dir, fun_name)
}

# get_no_default_args ----------------------------------------------------------
get_no_default_args <- function(arguments)
{
  if (!is.null(arguments)) {
    names(which(sapply(arguments, is.symbol)))
  }
}

# get_arg_combis ---------------------------------------------------------------
#' @importFrom kwb.utils expandGrid
get_arg_combis <- function(arg_names, max_args = 2L)
{
  string_values <- c(
    "1", "1:2",
    '"a"', 'c("a", "b")',
    "TRUE", "FALSE",
    'as.POSIXct("2018-06-03 23:50:00")',
    'list(key = c("a", "b"), value = 1:2)'
  )

  n <- min(max_args, length(arg_names))

  if (n == 1L) {

    matrix(string_values, ncol = 1L, dimnames = list(NULL, arg_names))

  } else {

    f <- rep(seq_len(n), each = length(string_values))

    arguments <- split(rep(string_values, n), f = f)

    names(arguments) <- arg_names[seq_len(n)]

    do.call(kwb.utils::expandGrid, arguments)
  }
}

# arg_combis_to_arg_strings ----------------------------------------------------
arg_combis_to_arg_strings <- function(arg_combis)
{
  if (nrow(arg_combis) == 0L) {
    return("")
  }

  args_for_paste <- lapply(names(arg_combis), function(arg_name) {
    paste(arg_name, "=", arg_combis[[arg_name]])
  })

  do.call(paste, c(args_for_paste, sep = ", "))
}
