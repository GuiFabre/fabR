## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release

## Latest submission
* release 1.1.0 - 2023-15-04:
1. correction of the package due to dependancies (message sent by CRAN).
2. suppression of magrittr dependancy (redundant)
3. typo and corrections of functions.these corrections do not change the purpose 
of each function but help enhance the performance of the function. 
 - add_index()
 - as_any_date()
 _ as_any_boolean()
 - get_duplicated_rows()
 _ get_duplicated_cols()
 - get_qll_na_rows()

## Previous submission 
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
 



