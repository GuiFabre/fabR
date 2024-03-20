#' @title
#' Extract columns that have same values in a tibble
#'
#' @description
#' This helper function extracts the names of the columns in a tibble having
#' identical values for all observations.
#'
#' @param tbl R object(dataframe or tibble) of the input tibble
#'
#' @return
#' A tibble indicating which columns which values is the same in the tibble
#'
#' @examples
#' {
#'
#' library(dplyr)
#' tbl <-
#'   mtcars %>%
#'   mutate(
#'    cyl_2 = cyl,
#'    cyl_3 = cyl,
#'    mpg_2 = mpg)
#'
#'  get_duplicated_cols(tbl)
#'
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom janitor remove_empty
#' @importFrom digest digest
#' @importFrom rlang .data
#' @export
get_duplicated_cols <- function(tbl){

  test <- tibble(condition = as.character(), col_name = as.character())
  if(tbl %>% nrow() == 0) return(test)
  if(ncol(tbl %>% remove_empty("cols"))) return(test)

  sample_num <- ifelse(nrow(tbl) > 500,500,nrow(test))


  test1 <-
    bind_rows(
      tbl %>% remove_empty("cols") %>%
        mutate(across(everything(), as.character)) %>%
        slice_sample(n = sample_num, replace = TRUE)) %>%
    rowwise() %>%
    mutate(across(everything(), ~ digest(.,algo = "md5"))) %>%
    mutate(across(everything(), ~ str_sub(., 1, 5))) %>%
    ungroup() %>%
    summarise_all(~ paste0(.,collapse = ""))
    pivot_longer(
      everything(),
      names_to = "condition",
      values_to = "col_1") %>%
    group_by(.data$col_1) %>%
    add_count() %>%
    dplyr::filter(n > 1)

  if(nrow(test1) > 0){

    test2 <-
      bind_rows(
        tbl %>%
          select(all_of(test1$condition)) %>%
          mutate(across(everything(), as.character))) %>%
      rowwise() %>%
      mutate(across(everything(), ~ digest(.,algo = "md5"))) %>%
      mutate(across(everything(), ~ str_sub(., 1, 5))) %>%
      ungroup() %>%
      summarise_all(~ paste0(.,collapse = "")) %>%
      pivot_longer(
        everything(),
        names_to = "condition",
        values_to = "col_1") %>%
      group_by(.data$col_1) %>%
      add_count() %>%
      dplyr::filter(n > 1)

    test <- test2

  }else{ test <- test1 }

  test <-
    test %>%
    group_by(.data$col_1) %>%
    summarise(
      across(
        everything(),
        ~ paste("Possible duplicated columns:",
                paste0(., collapse = " ; "))),.groups = "drop") %>%
    ungroup() %>% select("condition") %>%
    separate(
      col = "condition",
      into = c("to_remove","col_name"),
      sep = "\\:",
      remove = FALSE) %>%
    separate_rows("col_name", sep = ";") %>%
    select("condition", "col_name") %>%
    mutate(across(everything(), ~str_squish(.))) %>%
    mutate(across(everything(), ~as.character(.)))

  return(test)
}

#' @title
#' Extract observations (rows) that have same values in a tibble
#'
#' @description
#' This helper function extracts the row number (or first column value) in a
#' tibble having identical values for all columns. This function can be used
#' either on the whole columns or excluding the first column (id) (which can be
#' useful to identify repeated observation across different ids)
#'
#' @param tbl R object(dataframe or tibble) of the input tibble
#' @param id_col A character string specifying the column to ignore in
#' identification of repeated observations. If NULL (by default), all of the
#' columns will be taken in account for repeated observation identification.
#' The row number will be used to identify those observations.
#'
#' @return
#' A tibble indicating which row which values is the same in the tibble
#'
#' @examples
#' {
#'
#' # the row numbers are returned to identify which observations have repeated
#' # values
#' library(dplyr)
#' get_duplicated_rows(tbl = bind_rows( tbl = mtcars, mtcars[1,]))
#'
#' get_duplicated_rows(
#'   tbl = bind_rows(mtcars,mtcars[1,]) %>%
#'         add_index() %>%
#'         mutate(index = paste0('obs_',index)),
#'   id_col = 'index')
#'
#' }
#'
#' @import dplyr stringr tidyr
#' @importFrom janitor remove_empty
#' @importFrom digest digest
#' @importFrom rlang .data
#' @export
get_duplicated_rows <- function(tbl, id_col = NULL){

  test <- tibble(condition = as.character(), row_number = as.character())
  if(tbl %>% nrow() == 0) return(test)

  test <- tbl %>% remove_empty("cols")

  if(is.null(id_col)) {
    tbl <-   tbl %>% ungroup %>% add_index("fabR::index",.force = TRUE)
    test <-  tbl
    id_col <- "fabR::index"
  }else{
    tbl  <- tbl %>% ungroup %>% select(!! id_col, everything())
    test <- tbl #%>% ungroup %>% select(!! id_col, everything())
  }

  # avoid one column
  if(ncol(tbl) == 1) {
    tbl <-   tbl %>% add_index("fabR::col_id",.force = TRUE)
    test <-  tbl
    id_col <- "fabR::col_id"}

  sample_col <- 1:ifelse(ncol(test) > 20,20,ncol(test))

  if(length(sample_col) == 20)
    sample_col <- sample(1:ncol(test), max(sample_col), replace = TRUE)

  test1 <-
    test %>%
    select(sample_col) %>%
    rowwise() %>%
    mutate(across(-1, ~ digest(.,algo = "md5"))) %>%
    mutate(across(-1, ~ str_sub(., 1, 5))) %>%
    unite(-1, col = "fabR::row_duplicate", sep = "") %>%
    group_by(.data$`fabR::row_duplicate`) %>%
    add_count() %>%
    dplyr::filter(.data$`n` > 1)

  if(nrow(test1) > 0){
    test2 <-
      test %>%
      dplyr::filter(if_any(.cols = 1, ~ . %in% c(unique(test1[[1]])))) %>%
      # select(-1) %>%
      rowwise() %>%
      mutate(across(-1, ~ digest(.,algo = "md5"))) %>%
      mutate(across(-1, ~ str_sub(., 1, 5))) %>%
      unite(-1, col = "fabR::row_duplicate", sep = "") %>%
      # select(2, 1) %>%
      group_by(.data$`fabR::row_duplicate`) %>%
      add_count() %>%
      dplyr::filter(n > 1)

    test <- test2

  }else{ test <- test1 }

  names(test)[1] <- 'index'
  test <-
    test %>%
    group_by(.data$`fabR::row_duplicate`) %>%
    distinct() %>%
    summarise(
      row_number = paste0(.data$`index`, collapse = " ; ")) %>%
    mutate(condition = "Duplicated observations") %>%
    ungroup() %>% select("condition", "row_number")

  return(test)
}


#' @title
#' Extract columns that are all 'NA' from a tibble
#'
#' @description
#' This helper function extracts the names of the columns in a tibble having NA
#' values for all observations.
#'
#' @param tbl R object(dataframe or tibble) of the input tibble
#'
#' @return
#' A vector string indicating either that the tibble does not have empty
#' columns or the names of the empty columns.
#'
#' @examples
#' {
#'
#' ##### Example 1 -------------------------------------------------------------
#' # All columns have observation
#' get_all_na_cols(iris)
#'
#' ##### Example 2 -------------------------------------------------------------
#' # One column doesn't have any observations
#' library(dplyr)
#' get_all_na_cols(mutate(iris, new_col = NA))
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#' @export
get_all_na_cols <- function(tbl){


  test <- tibble(condition = as.character(), col_name = as.character())
  if(tbl %>% nrow() == 0) return(test)

  # identify columns containing all NA's
  test <-
    tbl %>% summarise(across(everything(), ~ n_distinct(., na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "col_name", values_to = "condition") %>%
    dplyr::filter(.data$condition == 0) %>%
    mutate(
      condition = "Empty column") %>%
    select("condition","col_name")

  return(test)
}


#' @title
#' Extract observations (rows) that have all NA values in a tibble
#'
#' @description
#' This helper function extracts the row number(s) having NA value for all
#' columns.
#'
#' @param tbl R object(dataframe or tibble) of the input tibble
#' @param id_col A character string specifying the column to ignore in
#' identification of repeated observations. If NULL (by default), all of the
#' columns will be taken in account for repeated observation identification.
#' The row number will be used to identify those observations.
#'
#' @return
#' A vector string indicating either that the tibble does not have empty
#' observation or the row number of the empty observations.
#'
#' @examples
#' {
#'
#' ##### Example 1 -------------------------------------------------------------
#' # All rows have observation
#' get_all_na_rows(iris)
#'
#' ##### Example 2 -------------------------------------------------------------
#' # One row doesn't have any observations
#' library(dplyr)
#' get_all_na_rows(bind_rows(iris, tibble(Species = c(NA,NA))))
#' get_all_na_rows(
#'   tbl = bind_rows(iris, tibble(Species =  c('id_151', 'id_152'))),
#'   id_col = 'Species')
#'
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
get_all_na_rows <- function(tbl, id_col = NULL){

  `{fabR::test}` <- tibble(condition = as.character(), row_number = as.character())
  if(tbl %>% nrow() == 0) return(`{fabR::test}`)

  if(is.null(id_col)) {
    tbl <- tbl %>% ungroup %>% add_index("{fabR::index}",.force = TRUE)
  }else{
    tbl  <- tbl %>% ungroup %>% select(!! id_col, everything())
  }

  # identify participants containing all NA's exept ID
  `{fabR::test}` <- tbl %>% select(-1)
  `{fabR::test}` <- `{fabR::test}` %>%
    mutate(`{fabR::is_na}` = rowSums(is.na(`{fabR::test}`)))
  `{fabR::test}` <-
    `{fabR::test}` %>%
    tibble %>%
    mutate(`{fabR::is_na}` = ncol(`{fabR::test}`) - .data$`{fabR::is_na}`) %>%
    bind_cols(tbl[1]) %>%
    dplyr::filter(.data$`{fabR::is_na}` == 1) %>%
    select(row_number = last_col()) %>%
    mutate(row_number = as.character(.data$`row_number`)) %>%
    mutate(
      condition = "Empty observation") %>%
    select("condition", "row_number") %>%
    distinct

  return(`{fabR::test}`)
}


#' @title
#' Extract columns that have unique values in a tibble
#'
#' @description
#' This helper function extracts the names of the columns in a tibble having
#' unique value for all observations.
#'
#' @param tbl R object(dataframe or tibble) of the input tibble
#'
#' @return
#' A vector string indicating either that the tibble does not have empty
#' columns or the names of the empty columns.
#'
#' @examples
#' {
#'
#' ##### Example 1 -------------------------------------------------------------
#' # All columns have distinct observation
#' get_unique_value_cols(iris)
#'
#' ##### Example 2 -------------------------------------------------------------
#' # One column doesn't have distinct observations
#' get_unique_value_cols(tbl = iris[1:50,])
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#' @export
get_unique_value_cols <- function(tbl){

  test <- tibble(
    condition = as.character(),
    col_name = as.character(),
    value = as.character())

  if(tbl %>% nrow() == 0) return(test)

  tbl <- tbl %>% mutate(across(everything(),as.character))

  # identify columns containing one value
  test <-
    tbl %>% summarise(across(everything(), ~ n_distinct(., na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "col_name",
      values_to = "condition") %>%
    rowwise() %>%
    mutate(
      value =
        ifelse(.data$condition == 1,
               toString(max(pull(tbl[.data$`col_name`]),na.rm = TRUE)),
               NA_character_)) %>%
    dplyr::filter(.data$condition == 1) %>%
    mutate(
      condition = "Unique value in the column") %>%
    select("condition","col_name","value")

  return(test)
}
