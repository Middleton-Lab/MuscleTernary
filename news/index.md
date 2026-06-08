# Changelog

## MuscleTernary 0.26

- Moved `ggtern`, `readxl`, `tidyverse`, and `animation` from `Depends`
  to `Imports` in `DESCRIPTION`.
- Added `URL` and `BugReports` fields to `DESCRIPTION`.
- Added `@return` documentation to
  [`make_mel()`](https://middleton-lab.github.io/MuscleTernary/reference/make_mel.md)
  and
  [`xfiber_to_maya()`](https://middleton-lab.github.io/MuscleTernary/reference/xfiber_to_maya.md).
- Added examples to all exported functions.
- Code quality improvements: replaced
  [`sapply()`](https://rdrr.io/r/base/lapply.html) with
  [`vapply()`](https://rdrr.io/r/base/lapply.html), replaced `1:nrow()`
  / `1:length()` patterns with
  [`seq_len()`](https://rdrr.io/r/base/seq.html) /
  [`seq_along()`](https://rdrr.io/r/base/seq.html), added `fixed = TRUE`
  to plain-string pattern matching calls.
