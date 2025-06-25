## R CMD check results

0 errors | 0 warnings | 0 note

# submission for fabR 2.1.1

> side note: dear people of CRAN, please note that fabR, madshapR,
Rmonize, mlstrOpalr, and BanffIT belong to a suite that is going to be 
updated. fabR aside, madshapR and Rmonize mlstrOpalr and BanffIT will 
not be retrocompatible, as we discussed with all our community, 
update all the codes and tested for 3 months, along with git 
monitoring. Ultimately we are thrilled to see the next release 
published in CRAN. Best Regards, Guillaume FABRE.

## Bug fixes and improvements

* `which_any_date()`, and `guess_date_format()`, `as_any_date()` now handles whole NA column

* Handle grouped tibbles in `get_duplicated_cols()`

* Bug in `as_any_integer()` has been corrected
https://github.com/GuiFabre/fabR/issues/20

* deprecated usage of summarize
https://github.com/GuiFabre/fabR/issues/22

* allow `file_index_create()` to read one file only.

* in `bookdown_template()`, hide anchor when mouse over and better CSS for 
titles and sections.

* default behavior of `read_excel_allsheets()` is to keep file in a list when 
being read.
