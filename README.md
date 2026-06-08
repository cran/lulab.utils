# lulab.utils

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

`lulab.utils` provides utility functions commonly used by LuLab for data
checking, download setup, model-result extraction, and Table 1 generation.

## Installation

```r
install.packages(
  "lulab.utils",
  repos = c("https://leslie-lu.r-universe.dev", "https://cloud.r-project.org")
)
```

Development version:

```r
# install.packages("devtools")
devtools::install_github("Leslie-Lu/lulab.utils")
```

## Main functions

- `Table1()`: create a stratified Table 1 and save `Table1.xlsx`.
- `extract_logistic_model()`: extract formatted logistic-regression results.
- `check_cha()`: summarize missing-like values in a column.
- `test_mirror()`: compare CRAN mirror download speed.
- `check_wget()` and `use_wget()`: check and configure `wget` on Windows.
- `round2()`: round halves away from zero.
