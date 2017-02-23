# kwb.test
Test whether Different Functions Return the Same

# Install from GitHub 

```r
if (! require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("kwb-r/kwb.test)
```

# Usage

```r
library(kwb.qmra)

# Define a function using saveArgs() to save arguments and result
squareSum <- function(a, b) {
  result <- a * a + b * b
  saveArgs("squareSum", args = list(a = a, b = b), result = result)
  result
}

# Set global variable TESTMODE to "activate" saveArgs() in squareSum()
TESTMODE <- TRUE

# Call the function with different arguments
squareSum(1, 2)
squareSum(2, 3)
squareSum(-1, -2)

# The arguments and function results were saved here:
dir(file.path(tempdir(), "test"))

# Write a new (wrong) version of the function
squareSum.new <- function(a, b) {
  a * a - b * b
}

# Check if it returns the same results
test_function("squareSum.new", loadArgs("squareSum"))

# If no test data are given, loadArgs is called on the function to test,
# i.e. testing squareSum on the test data created by the same function will
# return TRUE if the function did not change in the meanwhile.
test_function("squareSum")
```
