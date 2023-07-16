# single_quoted ----------------------------------------------------------------
single_quoted <- function(x)
{
  paste0("'", gsub("'", "\\\\'", x), "'")
}

# warn_if_file_exists ----------------------------------------------------------
warn_if_file_exists <- function(test_file)
{
  exists <- file.exists(test_file)

  if (exists) {
    message("Skipping exising file ", basename(test_file))
  }

  exists
}
