## R CMD check results

0 errors | 0 warnings | 0 note

# submission fabR 2.1.1

## Bug fixes and improvements

* `which_any_date()`, and `guess_date_format()`, `as_any_date()` now handles whole NA column

* Handle grouped tibbles in `get_duplicated_cols()`

* Bug in `as_any_integer()` has been corrected
https://github.com/GuiFabre/fabR/issues/20

* deprecated usage of summarise
https://github.com/GuiFabre/fabR/issues/22

* allow `file_index_create()` to read one file only.

* in `bookdown_template()`, hide hanchor when mouse over and better CSS for titles and sections.

* default behavior of `read_excel_allsheets()` is to keep file in a list when being read.


