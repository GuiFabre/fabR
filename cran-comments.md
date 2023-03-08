## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release

## Resubmission 
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
