# kwb.test

[![Appveyor build status](https://ci.appveyor.com/api/projects/status/88r5ooo4lv40tl45/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-test/branch/master)[![Build Status](https://travis-ci.org/KWB-R/kwb.test.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.test)
[![Build Status](https://travis-ci.org/KWB-R/kwb.test.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.test)
[![codecov](https://codecov.io/github/KWB-R/kwb.test/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.test)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/kwb.test)](http://cran.r-project.org/package=kwb.test)

Test whether Different Functions Return the Same

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.test' from GitHub

remotes::install_github("kwb-r/kwb.test")
```

## Documentation

Development version: [https://kwb-r.github.io/kwb.test/dev](https://kwb-r.github.io/kwb.test/dev)
Latest release: [https://kwb-r.github.io/kwb.test](https://kwb-r.github.io/kwb.test)
