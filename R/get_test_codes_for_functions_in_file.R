# get_test_codes_for_functions_in_file -----------------------------------------
get_test_codes_for_functions_in_file <- function(file, pkg_name, test_dir, ...)
{
  # Get the expressions that represent assignments of function definitions
  assignments <- get_function_assignments(file)

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
      get_test_for_function(
        fun_name = fun_name,
        fun_args = assignments[[fun_name]][[3]][[2]],
        pkg_name = pkg_name,
        exports = exports,
        ...
      )
    }
  )

  # Remove NULL elements
  kwb.utils::excludeNULL(test_calls, dbg = FALSE)
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

  assignments <- code[is_function_assignment]

  # Name the assignments according to the function names
  stats::setNames(
    assignments,
    sapply(assignments, function(x) as.character(x[[2]]))
  )
}

# path_to_testfile -------------------------------------------------------------
path_to_testfile <- function(test_dir, fun_name)
{
  sprintf("%s/test-function-%s.R", test_dir, fun_name)
}

# get_test_for_function --------------------------------------------------------
get_test_for_function <- function(
    fun_name,
    fun_args,
    pkg_name,
    exports = getNamespaceExports(pkg_name),
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

# get_function_call_strings ----------------------------------------------------
#' @importFrom kwb.utils asColumnList resolve
get_function_call_strings <- function(fun_name, arg_combis, pkg_name = "")
{
  arg_strings <- if (nrow(arg_combis) > 0L) {

    arg_combi_list <- kwb.utils::asColumnList(as.matrix(arg_combis))

    assignment <- function(name) paste(name, "=", arg_combi_list[[name]])

    paste_args <- c(lapply(names(arg_combi_list), assignment), sep = ", ")

    arg_strings <- do.call(paste, paste_args)

  } else {

    ""
  }

  sprintf("f(%s)", arg_strings)
}
