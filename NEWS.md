
# fabR 2.1.1 (release : 2025-06-03)

## Bug fixes and improvements

- `which_any_date()`, and `guess_date_format()`, `as_any_date()` now
  handles whole NA column

- Handle grouped tibbles in `get_duplicated_cols()`

- Bug in `as_any_integer()` has been corrected
  <https://github.com/GuiFabre/fabR/issues/20>

- deprecated usage of summarize
  <https://github.com/GuiFabre/fabR/issues/22>

- allow `file_index_create()` to read one file only.

- in `bookdown_template()`, hide anchor when mouse over and better CSS
  for titles and sections.

- default behavior of `read_excel_allsheets()` is to keep file in a list
  when being read.

# fabR 2.1.0

## Bug fixes and improvements

- `as_any_date()` handles `lubridate::ym` and `lubridate::my`. Still,
  some ambiguous formats are not detected (“19 July” returns 2019-07-01)
  <https://github.com/GuiFabre/fabR/issues/15>

- enhance performance of `get_duplicated_cols()` and
  `get_duplicated_rows()`.

- The function `get_all_na_rows()` had a bug when a column was named
  “test”, that has been fixed.
  <https://github.com/GuiFabre/fabR/issues/13>

- The function `get_duplicated_rows()` had a bug when a tibble had only
  one column, that has been fixed.
  <https://github.com/GuiFabre/fabR/issues/14>

- The date functions `as_any_date()`, `as_any_date()`, `as_any_date()`,
  have a default format setting which is yyyy-mm-dd. Also if there is
  ambiguity, but 100 % match, the first format will be used as the
  format for the whole column.
  <https://github.com/GuiFabre/fabR/issues/12>

- The function `read_any_csv()` can handle 3 types of CSV (coma
  separator, semi column separator and a mix of columns containing
  both), which works better. The function also handles (a priori) the
  latin1 encoding Still experimental.
  <https://github.com/GuiFabre/fabR/issues/10>
  <https://github.com/GuiFabre/fabR/issues/9>

## New functions

- `as_any_integer()` which allows to coerce a vector compatible to
  integer. Text, boolean and numerics which are homogeneous to integers.

## deprecated functions

To avoid confusion with help(function), the function `fabR_help()` has
been renamed `fabR_website()`.

## Dependency changes

- set a minimum dplyr dependence to avoid bugs
- digest package is no longer needed in the package

# fabR 2.0.1

## Bug fixes and improvements

- the function add_index has been corrected to handle bug when the
  tibble in input contains a column named `start`.

# fabR 2.0.0

## Creation of NEWS feed !!

Addition of `NEWS.md` for the development version use “(development
version)”.

## Bug fixes and improvements

- Some improvements in the documentation of the package has been made.

- `read_excel_allsheets()` The function has a new parameter which allows
  the user to keep the ouput as a list or as a tibble when there is only
  one tab in the input excel file.

- `get_xxx()` functions to navigate in tibble have been modified to
  exclude index dependancy and be more coherente to each other.

## Dependency changes

**New Imports:** xfun, lifecycle

**No longer in Imports:** ggplot2, plotly, DT

## New functions

`bookdown_template()` replaces the deprecated function
`bookdown_template()` to improve stability (no more dependency of a
github repo) and open new possibilities. Such as:

- `bookdown_render()` which renders a Rmd collection of files into a
  docs/index.html website.

- `bookdown_open()` Which allows to open a docs/index.html document when
  the bookdown is rendered

This separation into 3 functions will allow future developments, such as
render as a ppt or pdf.

## deprecated functions

Due to another package development (see
[madshapR](https://maelstrom-research.github.io/madshapR-documentation/)),
all functions plot_xxx() and summary_xxx() have been deprecated in favor
of
[variable_visualize](https://maelstrom-research.github.io/madshapR-documentation/reference/variable_visualize.html))

# fabR 1.1.1

This package is a collection of wrapper functions used in data
pipelines.

This is still a work in progress, so please let me know if you used a
function before and is not working any longer.

## functions to navigate in a folder, file or R object.

`file_index_create()`, `file_index_read()`,`file_index_search()`

These functions allows to create, read and search into a tibble listing
files in a specified folder (recursively) with file path name and other
useful metadata. This index can be used to quickly find files in the
environment. The index also generates script to read files as R objects
into the environment. Names for R objects are generated automatically
from file names (R objects are not created at this step but the command
line is generated and stored in the column to_eval, ready to be
evaluated and generate R objects).

`collect_roxygen()` is a helper function that can read a structured
documentation of a package and turn it into a tibble.

`add_index()` adds a extra column in a tibble which is index of
observations.

## functions better than base R :

- `as_any_boolean()`

Create or test for objects of type “logical”, and the basic logical
constants. This function is a wrapper of the function base::as.logical()
and evaluates if the object to be coerced can be interpreted as a
boolean. Any object : NA, NA_integer, NA_Date\_, (…), 0, 0L, F, FALSE,
false, FaLsE, (…), 1, 1L,T, TRUE, true, TrUe, (…), will be converted as
NA, FALSE and TRUE. Any other other will return an error.

- `as_any_date()`, `which_any_date()`, `guess_any_date()`

These function takes a character string or a vector. This vector
evaluates one observation after the other, and casts the best matching
date format for each of them (independently). The best matching format
is tested across seven different formats provided by the lubridate
library. The user can specify the wanted matching format.

- `as_any_symbol()`

Create or test for objects of type “symbol”.

## Functions to go quicker in the code

- `silently_run()` allows to generate code avoiding suppressMessages,
  suppressWarnings, try and tryCatch.

- `parceval()` reads and evaluate a string character as a code and runs
  it.

- `message_on_prompt()` allows to run text to prompt in the console.

## Helper functions

- `fabR_help()` Call the help center for full documentation

## Navigate into a list

- `make_name_list()` Generate a name for an element in a list. This
  function is targeted for functions creations which handle lists.

- `get_path_list()` Function that recursively go through a list object
  and store in a tibble the path of each element in the list. The paths
  can be after that edited and accessed using `parceval()` for example.

## QA functions for tibbles

These helper functions evaluate content of a column to extract what they
are supposed to extract for all observations.

`get_all_na_cols()`, `get_all_na_rows()`,`get_duplicated_cols()`,
`get_duplicated_rows()`,`get_unique_value_cols()`

## Write and read excel and csv

- `read_csv_any_formats()` The csv file is read twice to detect the
  number of lines to use in attributing the column type (‘guess_max’
  parameter of read_csv). This avoids common errors when reading csv
  files.

- `read_excel_allsheets()` The Excel file is read and the values are
  placed in a list of tibbles, with each sheet in a separate element in
  the list. If the Excel file has only one sheet, the output is a single
  tibble.

- `write_excel_allsheets()` Write all Excel sheets using
  `xlsx::write.xlsx()` recursively.

## Plot and summary functions used in a visual report

`plot_bar()`, `plot_box()`, `plot_date()`, `plot_density()`,
`plot_histogram()`, `plot_main_word()`, `plot_pie_valid_value()`,
`summary_category()`, `summary_numerical()`,`summary_text()`

These functions draw a plot or create datatable of the values of a
column. Missing values can be given as input to non-valid and valid
values separately, or grouped by another column. The output can be
editable (using plotly library) or static (using ggplot2 library). The
R-code is also editable for coding recycling purpose.

`template_visual_report()` is a helper function creates a template for
the visual report bookdown. This template is taken from the following
link:
<https://github.com/jtr13/bookdown-template/archive/refs/heads/master.zip>
folder.

The plot_xxx() and summary_xxx() functions can be used to generate code
in R chunks.
