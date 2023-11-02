## R CMD check results

0 errors | 0 warnings | 0 note


# submission : fabR 2.0.1

- the function add_index has been corrected to handle bug when the
  tibble in input contains a column named `start`. No change or impact for the 
  user.

# submission : fabR 2.0.0

message for cran submission :
> package fabR_2.0.0.1000.tar.gz has been auto-processed. The auto-check found 
problems when checking the first order strong reverse dependencies.Please 
reply-all and explain: Is this expected or do you need to fix anything in your 
package? If expected, have all maintainers of affected packages been informed 
well in advance? Are there false positives in our results?

Response :
I am trying my best to figure out the best way to handle the correct linkage of 
two packages I developped. The first one, fabR had many functions (starting with
fabR::plot_xxx() and fabR::summary_xxx()) which are now gathered in one and only 
function madshapR::variable_visualize(). This function is in the other package 
(madshapR) and will be proposed to CRAN after fabR is accepted. These changes 
reflect the difference in the contributions for both packages. Everyone involved
in the package development is well informed of these changes.

Other changes : 
- addition of Matrix library in suggest to pass R CMD-check in github.
- Due to another package development 
(see [madshapR](https://maelstrom-research.github.io/madshapR-documentation/)),
all functions plot_xxx() and summary_xxx() have been suppressed in favor of
[variable_visualize](https://maelstrom-research.github.io/madshapR-documentation/reference/variable_visualize.html)). These functions are in the next CRAN version of madshapR.



## Bug fixes and improvements

* Addition of `NEWS.md` for the development version use "(development version)".

* Some improvements in the documentation of the package has been made.

* `read_excel_allsheets()` The function has a new parameter which allows the user
to keep the ouput as a list or as a tibble when there is only one tab in the
input excel file.

* `get_xxx()` functions to navigate in tibble have been modified to exclude index
dependancy and be more coherente to each other.

## Dependency changes

**New Imports:** xfun, lifecycle

**No longer in Imports:** ggplot2, plotly, DT

## New functions

`bookdown_template()` replaces the deprecated function `bookdown_template()` to 
improve stability (no more dependency of a github repo) and open new possibilities. 
Such as:

* `bookdown_render()` which renders a Rmd collection of files into a docs/index.html
website.

* `bookdown_open()` Which allows to open a docs/index.html document when the bookdown
is rendered

This separation into 3 functions will allow future developments, such as render as
a ppt or pdf.

## deprecated functions

Due to another package development 
(see [madshapR](https://maelstrom-research.github.io/madshapR-documentation/)),
all functions plot_xxx() and summary_xxx() have been deprecated in favor of
[variable_visualize](https://maelstrom-research.github.io/madshapR-documentation/reference/variable_visualize.html)). These functions are in the next CRAN version of madshapR

--------------------------------------------------------------------------------

## Previous submission : fabR 1.1.1

2023-15-04:
1. correction of the package due to dependancies (message sent by CRAN).
2. suppression of magrittr dependancy (redundant)
3. typo and corrections of functions.these corrections do not change the purpose 
of each function but help enhance the performance of the function. 
 - add_index()
 - as_any_date()
 _ as_any_boolean()
 - get_duplicated_rows()
 _ get_duplicated_cols()
 - get_all_na_rows()
 
* https://win-builder.r-project.org/incoming_pretest/fabR_1.1.0_20230307_011952/Windows/00check.log
1. corrections have been made
* Second release:
1. use the Authors@R in description
2. replace \dontrun with \ where possible
3. ensure that functions do not write by default in the user env.
4. suppress print in the file index functions
5. ensure there is no default path in writing functions, and replace by tempfile()/tempdir()
6. addition of haven and xlsx in the package dependancies
* 3rd release:
1. uncomment all examples
 



