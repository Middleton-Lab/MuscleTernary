# MuscleTernary 0.26

* Moved `ggtern`, `readxl`, `tidyverse`, and `animation` from `Depends` to
  `Imports` in `DESCRIPTION`.
* Added `URL` and `BugReports` fields to `DESCRIPTION`.
* Added `@return` documentation to `make_mel()` and `xfiber_to_maya()`.
* Added examples to all exported functions.
* Code quality improvements: replaced `sapply()` with `vapply()`, replaced
  `1:nrow()` / `1:length()` patterns with `seq_len()` / `seq_along()`,
  added `fixed = TRUE` to plain-string pattern matching calls.
