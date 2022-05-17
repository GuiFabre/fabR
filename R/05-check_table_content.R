#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param dataset xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx xxx xxx.
#'
#'
#' }
#'
#' @import dplyr magrittr
get_duplicated_cols <- function(dataset){

  test <- tibble(condition = as.character(), name_var = as.character() )
  if(dataset %>% nrow() == 0) return(test)

  test <-
    bind_rows(
      dataset %>% janitor::remove_empty("cols") %>%
        slice_sample(n = 10,replace = TRUE)) %>%
    rowwise() %>%
    mutate_all(~ digest::digest(.,algo = "md5")) %>%
    mutate_all(~ stringr::str_sub(.,1,2)) %>%
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
    summarise(across(everything(), ~paste("[INFO] - Possible duplicated variables:",paste0(., collapse = " ; "))),.groups = "drop") %>%
    ungroup() %>% select(.data$condition) %>%
    tidyr::separate(col = .data$condition,into = c("to_remove","name_var"),sep = "\\:", remove = FALSE) %>%
    tidyr::separate_rows(.data$name_var,sep = ";") %>%
    select(-.data$to_remove) %>%
    mutate(across(everything(), ~str_squish(.))) %>%
    mutate(across(everything(), ~as.character(.)))

  return(test)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param dataset xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx xxx xxx.
#'
#'
#' }
#'
#' @import dplyr magrittr
get_duplicated_rows <- function(dataset){

  test <- tibble(condition = as.character(), name_var = as.character() )
  if(dataset %>% nrow() == 0) return(test)

  test <- dataset %>% janitor::remove_empty("cols")
  test <- test
    select(sample(2:length(test), 50, replace = TRUE)) %>%
    rowwise() %>%
    mutate_all(~ digest::digest(.,algo = "md5")) %>%
    mutate_all(~ stringr::str_sub(.,1,2)) %>%
    tidyr::unite(.data$row_duplicate, sep = "") %>%
    tibble::add_column(dataset %>% select(id_duplicate = 1)) %>%
    select(2,1) %>%
    group_by(.data$row_duplicate) %>%
    add_count() %>%
    filter(n > 1) %>%
    group_by(.data$row_duplicate) %>%
    summarise_all(~ paste("[INFO] - Possible duplicated participants :", paste0(., collapse = " ; "))) %>%
    ungroup() %>% select(condition = .data$id_duplicate)

  return(test)
}


#' Extract columns that are all 'NA' from a dataset
#'
#' This helper function extracts the names of the columns in a dataset having NA
#' values for all observations.
#'
#' @param dataset A character string or tibble of the input dataset
#'
#' @return A vector string indicating either that the dataset does not have empty
#' columns or the names of the empty columns.
#'
#' @examples
#' \dontrun{
#' # Example 1: All columns have observation
#' get_all_na_cols(iris)
#'
#' # Example 2: One column doesn't have any observations
#' get_all_na_cols(iris %>% mutate(new_col = NA))
#' }
#'
#' @import dplyr magrittr
#' @export
get_all_na_cols <- function(dataset){

  # identify columns containing all NA's
  test <-
    dataset %>% summarise(across(everything(), ~ n_distinct(., na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(),names_to = "name_var",values_to = "condition") %>%
    filter(.data$condition == 0) %>%
    mutate(
      condition = "[INFO] - Empty column")

  return(test)
}

#' Extract columns that are all 'NA' from a dataset
#'
#' This helper function extracts the names of the columns in a dataset having NA
#' values for all observations.
#'
#' @param dataset A character string or tibble of the input dataset
#'
#' @return A vector string indicating either that the dataset does not have empty
#' columns or the names of the empty columns.
#'
#' @examples
#' \dontrun{
#' # Example 1: All columns have observation
#' get_all_na_cols(iris)
#'
#' # Example 2: One column doesn't have any observations
#' get_all_na_cols(iris %>% mutate(new_col = NA))
#' }
#'
#' @import dplyr magrittr
#' @export
get_all_na_rows <- function(dataset){

  # identify participants containing all NA's exept ID
  test <- dataset %>% select(-1)
  test <- test %>% mutate(is_na = rowSums(is.na(test)))
  test <-
    test %>%
    mutate(is_na = ncol(test) - .data$is_na) %>%
    bind_cols(dataset[1]) %>%
    filter(.data$is_na == 0) %>%
    select(participant = last_col()) %>%
    mutate(
      condition = "[ERR] - Empty participant")
  return(test)
}

#' Extract columns that are all 'NA' from a dataset
#'
#' This helper function extracts the names of the columns in a dataset having NA
#' values for all observations.
#'
#' @param dataset A character string or tibble of the input dataset
#'
#' @return A vector string indicating either that the dataset does not have empty
#' columns or the names of the empty columns.
#'
#' @examples
#' \dontrun{
#' # Example 1: All columns have observation
#' get_all_na_cols(iris)
#'
#' # Example 2: One column doesn't have any observations
#' get_all_na_cols(iris %>% mutate(new_col = NA))
#' }
#'
#' @import dplyr magrittr
#' @export
get_unique_value_cols <- function(dataset){

  # identify columns containing one value
  test <-
    dataset %>% summarise(across(everything(), ~ n_distinct(., na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(),names_to = "name_var",values_to = "condition") %>%
    filter(.data$condition == 1) %>%
    mutate(
      condition = "[INFO] - Unique value in the colomn")

  return(test)
}
