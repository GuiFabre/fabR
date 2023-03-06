#' @title
#' xxx xxx xxx
#'
#' @description
#' xxx xxx xxx.
#'
#' @param col xxx
#' @param tbl xxx
#'
#' @return
#' xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr lubridate tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
guess_date_format <- function(tbl, col = NULL){

  if(is.null(col)) col <- tbl %>% names
  tbl <- tbl %>% select(all_of(col))

  test <- tibble(
    name_var = as.character(),
    `Date format` = as.character(),
    `% values formated` = as.numeric())

  for(i in tbl %>% names){

    test <-
      bind_rows(
        test,
        tbl %>%
          select(var = all_of(i)) %>%
          filter(!is.na(.data$var)) %>%
          distinct() %>%
          rowwise() %>%
          mutate(
            dmy = dmy(.data$var, quiet = TRUE),
            dym = dym(.data$var, quiet = TRUE),
            ymd = ymd(.data$var, quiet = TRUE),
            ydm = ydm(.data$var, quiet = TRUE),
            mdy = mdy(.data$var, quiet = TRUE),
            myd = myd(.data$var, quiet = TRUE)) %>%
          ungroup %>%
          summarise(across(-.data$`var`, ~ sum(!is.na(.)))) %>%
          pivot_longer(cols = everything(), names_to = "Date format", values_to = "nb_values") %>%
          mutate(
            name_var = i,
            `% values formated` = round(100*(.data$nb_values / (tbl %>% select(var = all_of(i)) %>% distinct %>% filter(!is.na(.data$var)) %>% nrow)),2),
            `% values formated` = ifelse(is.na(.data$`% values formated`),0,.data$`% values formated`),
            `Date match` = case_when(
              .data$`% values formated` == 0   ~ "No match",
              .data$`% values formated` == 100 ~ paste0("Exact match"),
              TRUE                       ~ paste0("Closest match")),
          )  %>%
          arrange(-.data$nb_values) %>%
          slice(1) %>%
          select(-.data$nb_values)
      )
  }
  return(test)
}

#' @title
#' xxx xxx xxx
#'
#' @description
#' Creates objects of type "Date".
#'
#' @param x object to be coerced.
#' @param format object to be coerced.
#'
#' @return
#' xxx xxx xxx
#'
#' @examples
#' \dontrun{
#' # Example 1: This example
#'
#'
#' }
#'
#' @import dplyr lubridate stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
which_any_date <- function(x, format = c("dmy","dym","ymd","ydm","mdy","myd","as_date")){

  test = c()
  x_origin <- x
  x <- unique(x)

  if(length(x) == 0) return("ymd")

  for(i in 1:length(x)){

    if(is.na(x[i])){ test[i] <- NA_character_ }
    else{

      test[i] <-
        c(if("dmy" %in% format & !is.na(dmy(x[i], quiet = TRUE))) "dmy",
          if("dym" %in% format & !is.na(dym(x[i], quiet = TRUE))) "dym",
          if("ymd" %in% format & !is.na(ymd(x[i], quiet = TRUE))) "ymd",
          if("ydm" %in% format & !is.na(ydm(x[i], quiet = TRUE))) "ydm",
          if("mdy" %in% format & !is.na(mdy(x[i], quiet = TRUE))) "mdy",
          if("myd" %in% format & !is.na(myd(x[i], quiet = TRUE))) "myd",
          if("as_date" %in% format & !is.na(suppressWarnings(as_date(x[i])))) "as_date") %>% toString }}

  test <-
    test %>%
    na_if("") %>%
    str_remove(pattern = ", as_date")

  test <- full_join(tibble(x = x_origin),tibble(test, x),by = 'x')$test
  return(test)
}

#' @title
#' xxx xxx xxx
#'
#' @description
#' Creates objects of type "Date".
#'
#' @param x object to be coerced.
#' @param format object to be coerced.
#'
#' @return
#' xxx xxx xxx
#'
#' @examples
#' \dontrun{
#' # Example 1: This example
#'
#'
#' }
#'
#' @import dplyr lubridate stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
as_any_date <- function(x = as.character(), format = c("dmy","dym","ymd","ydm","mdy","myd","as_date")){

  date_test <- which_any_date(x, format)

  wrn3 <- wrn4 <- FALSE

  for(i in 1:length(date_test)){
    # stop()}

    if(!is.na(date_test[i]) & length(x) == 0){date_test = as.character(x)}
    else if(is.na(date_test[i])  & is.na(x[i])){date_test[i] <- NA_Date_}
    else if(is.na(date_test[i])  & !is.na(x[i])){date_test[i] <- NA_Date_;wrn3 <- TRUE}
    else if(str_detect(date_test[i], ",")){date_test[i] <- NA_Date_;wrn4 <- TRUE}
    else {date_test[i] <- do.call(date_test[i], list(x[i])) %>% as.character}

  }

  if(wrn3) warning(call. = FALSE,
                   "All formats failed to parse for some values.",
                   "\n","\n\nUseful tip:"," Use which_any_date(x) to get formats.")

  if(wrn4) warning(call. = FALSE,
                   "Ambiguous date format (",date_test[i],"). Please provide format in parameters",
                   "\n","\n\nUseful tip:"," Use which_any_date(x) to get formats.")

  date_final <- ymd(date_test)

  return(date_final)
}

