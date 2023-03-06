#' @title
#' Extract columns that have same values in a tibble
#'
#' @description
#' This helper function extracts the names of the columns in a tibble having
#' identical values for all observations.
#'
#' @param tbl R object(dataframe or tibble) of the input tbl
#'
#' @return
#' A tibble indicating which columns which values is the same in
#' the tibble
#'
#' @examples
#' \dontrun{
#'
# mtcars_duplicated <-
#  mtcars %>%
#  mutate(
#   cyl_2 = cyl,
#   cyl_3 = cyl,
#   mpg_2 = mpg)
#'
#'  get_duplicated_cols(mtcars_duplicated)
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_duplicated_cols <- function(tbl){

  test <- tibble(condition = as.character(), name_var = as.character() )
  if(tbl %>% nrow() == 0) return(test)

  test <-
    bind_rows(
      tbl %>% janitor::remove_empty("cols") %>%
        slice_sample(n = 10, replace = TRUE)) %>%
    rowwise() %>%
    mutate_all(~ digest::digest(.,algo = "md5")) %>%
    mutate_all(~ stringr::str_sub(., 1, 2)) %>%
    ungroup() %>%
    summarise_all(~ paste0(.,collapse = "")) %>%
    tidyr::pivot_longer(
      everything(),
      names_to = "condition",
      values_to = "col_1") %>%
    group_by(.data$col_1) %>%
    add_count() %>%
    filter(n > 1) %>%
    group_by(.data$col_1) %>%
    summarise(across(everything(), ~ paste("[INFO] - Possible duplicated columns:", paste0(., collapse = " ; "))),.groups = "drop") %>%
    ungroup() %>% select(.data$condition) %>%
    tidyr::separate(col = .data$condition, into = c("to_remove","name_col"), sep = "\\:", remove = FALSE) %>%
    tidyr::separate_rows(.data$name_col, sep = ";") %>%
    select(-.data$to_remove) %>%
    mutate(across(everything(), ~stringr::str_squish(.))) %>%
    mutate(across(everything(), ~as.character(.)))

  return(test)
}

#' @title
#' Extract observations(rows) that have same values in a tibble
#'
#' @description
#' This helper function extracts the row number (or first column value) in a
#' tibble having identical values for all columns. This function can be used
#' either on the whole columns or excluding the first column (id) (which can be
#' useful to identify repeated observation across different ids)
#'
#' @param tbl R object(dataframe or tibble) of the input tbl
#' @param id_col A character string specifying the column to ignore in
#' identification of repeated observations. If NULL (by default), all of the
#' columns will be taken in account for repeated observation identification.
#' The row number will be used to identify those observations.
#'
#' @return
#' A tibble indicating which row which values is the same in
#' the tibble
#'
#' @examples
#' \dontrun{
#'
#' # the row numbers are returned to identify which observations have repeated values
#' library(tidyverse)
#' get_duplicated_rows(bind_rows(mtcars,mtcars[1,]))
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_duplicated_rows <- function(tbl, id_col = NULL){

  test <- tibble(condition = as.character(), name_var = as.character())
  if(tbl %>% nrow() == 0) return(test)

  test <- tbl %>% janitor::remove_empty("cols")

  if(is.null(id_col)) {
    tbl <- tbl %>% ungroup %>% add_index("__Mlstr_index__")
    test <- test %>% add_index("__Mlstr_index__")
  }else{

    tbl  <- tbl %>% ungroup %>% select(!! id_col, everything())
    test <- tbl %>% ungroup %>% select(!! id_col, everything())
  }

  test <-
    test %>%
    select(sample(2:length(test), 50, replace = TRUE)) %>%
    rowwise() %>%
    mutate_all(~ digest::digest(.,algo = "md5")) %>%
    mutate_all(~ stringr::str_sub(., 1, 2)) %>%
    tidyr::unite(col = "row_duplicate", sep = "") %>%
    mutate(id_duplicate = tbl[[1]]) %>%
    select(2, 1) %>%
    group_by(.data$row_duplicate) %>%
    add_count() %>%
    filter(n > 1) %>%
    group_by(.data$row_duplicate) %>%
    summarise_all(~ paste("[INFO] - Possible duplicated observations:", paste0(., collapse = " ; "))) %>%
    ungroup() %>% select(condition = .data$id_duplicate)

  return(test)
}

#' @title
#' Extract columns that are all 'NA' from a tbl
#'
#' @description
#' This helper function extracts the names of the columns in a tbl having NA
#' values for all observations.
#'
#' @param tbl R object(dataframe or tibble) of the input tbl
#'
#' @return
#' A vector string indicating either that the tbl does not have empty
#' columns or the names of the empty columns.
#'
#' @examples
#' \dontrun{
#' # Example 1: All columns have observation
#' get_all_na_cols(iris)
#'
#' # Example 2: One column doesn't have any observations
#' library(tidyverse)
#' get_all_na_cols(mutate(iris, new_col = NA))
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_all_na_cols <- function(tbl){

  # identify columns containing all NA's
  test <-
    tbl %>% summarise(across(everything(), ~ n_distinct(., na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "name_col", values_to = "condition") %>%
    filter(.data$condition == 0) %>%
    mutate(
      condition = "[INFO] - Empty column")

  return(test)
}

#' @title
#' Extract columns that are all 'NA' from a tbl
#'
#' @description
#' This helper function extracts the names of the columns in a tbl having NA
#' values for all observations.
#'
#' @param tbl R object(dataframe or tibble) of the input tbl
#'
#' @return
#' A vector string indicating either that the tbl does not have empty
#' columns or the names of the empty columns.
#'
#' @examples
#' \dontrun{
#' # Example 1: All rows have observation
#' get_all_na_cols(iris)
#'
#' # Example 2: One row doesn't have any observations
#' library(tidyverse)
#' get_all_na_rows(bind_rows(iris, tibble(Species = NA)))
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_all_na_rows <- function(tbl){

  # identify participants containing all NA's exept ID
  test <- tbl %>% select(-1)
  test <- test %>% mutate(is_na = rowSums(is.na(test)))
  test <-
    test %>%
    mutate(is_na = ncol(test) - .data$is_na) %>%
    bind_cols(tbl[1]) %>%
    filter(.data$is_na == 1) %>%
    select(participant = last_col()) %>%
    mutate(
      participant = toString(.data$`participant`),
      condition = "[INFO] - Empty observation")
  return(test)
}

#' @title
#' Extract columns that are all 'NA' from a tbl
#'
#' @description
#' This helper function extracts the names of the columns in a tbl having NA
#' values for all observations.
#'
#' @param tbl R object(dataframe or tibble) of the input tbl
#'
#' @return
#' A vector string indicating either that the tbl does not have empty
#' columns or the names of the empty columns.
#'
#' @examples
#' \dontrun{
#' # Example 1: All columns have distinct observation
#' get_unique_value_cols(iris)
#'
#' # Example 2: One column doesn't have distinct observations
#' get_unique_value_cols(slice(iris,1:50))
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_unique_value_cols <- function(tbl){

  # identify columns containing one value
  test <-
    tbl %>% summarise(across(everything(), ~ n_distinct(., na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "name_col", values_to = "condition") %>%
    filter(.data$condition == 1) %>%
    mutate(
      condition = "[INFO] - Unique value in the column")

  return(test)
}
