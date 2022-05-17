#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param col xxx
#' @param tbl xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr
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

    test <- bind_rows(test,
                      tbl %>%
                        select(var = all_of(i)) %>%
                        filter(!is.na(.data$var)) %>%
                        rowwise() %>%
                        mutate(
                          dmy = lubridate::dmy(.data$var, quiet = TRUE),
                          dym = lubridate::dym(.data$var, quiet = TRUE),
                          ymd = lubridate::ymd(.data$var, quiet = TRUE),
                          ydm = lubridate::ydm(.data$var, quiet = TRUE),
                          mdy = lubridate::mdy(.data$var, quiet = TRUE),
                          myd = lubridate::myd(.data$var, quiet = TRUE)) %>%
                        ungroup %>%
                        summarise(across(c(.data$dmy,.data$dym,.data$ymd,.data$ydm,.data$mdy,.data$myd), ~ n_distinct(., na.rm = TRUE))) %>%
                        tidyr::pivot_longer(cols = everything(), names_to = "Date format", values_to = "nb_values") %>%
                        mutate(
                          name_var = all_of(i),
                          `% values formated` = round(100*(.data$nb_values / (tbl %>% select(var = all_of(i)) %>% filter(!is.na(.data$var)) %>% nrow)),2),
                          `% values formated` = ifelse(is.na(.data$`% values formated`),0,.data$`% values formated`),
                          `Date format` = case_when(
                            .data$`% values formated` == 0   ~ "No match",
                            .data$`% values formated` == 100 ~ paste0("Exact match : ", .data$`Date format`),
                            TRUE                       ~ paste0("Closest match : ", .data$`Date format`)),
                        )  %>%
                        arrange(-.data$nb_values) %>%
                        slice(1) %>%
                        select(-.data$nb_values)
    )
  }
  return(test)
}

#' xxx xxx xxx
#'
#' Creates objects of type "Date".
#'
#' @param x object to be coerced.
#' @param format object to be coerced.
#'
#' @return xxx xxx xxx
#'
#' @examples
#' \dontrun{
#' # Example 1: This example
#'
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
which_any_date <- function(x, format = c("dmy","dym","ymd","ydm","mdy","myd")){

  test = c()

  if(length(x) == 0) return("ymd")

  for(i in 1:length(x)){

    if(is.na(x[i])){ test[i] <- NA_character_ }
    else{

      test[i] <-
        c(if("dmy" %in% format & !is.na(lubridate::dmy(x[i], quiet = TRUE))) "dmy",
          if("dym" %in% format & !is.na(lubridate::dym(x[i], quiet = TRUE))) "dym",
          if("ymd" %in% format & !is.na(lubridate::ymd(x[i], quiet = TRUE))) "ymd",
          if("ydm" %in% format & !is.na(lubridate::ydm(x[i], quiet = TRUE))) "ydm",
          if("mdy" %in% format & !is.na(lubridate::mdy(x[i], quiet = TRUE))) "mdy",
          if("myd" %in% format & !is.na(lubridate::myd(x[i], quiet = TRUE))) "myd") %>% toString }}

  test <- test %>% na_if("")

  return(test %>% na_if(""))
}

#' xxx xxx xxx
#'
#' Creates objects of type "Date".
#'
#' @param x object to be coerced.
#' @param format object to be coerced.
#'
#' @return xxx xxx xxx
#'
#' @examples
#' \dontrun{
#' # Example 1: This example
#'
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
as_any_date <- function(x, format = c("dmy","dym","ymd","ydm","mdy","myd")){

  date <- which_any_date(x, format)

  for(i in 1:length(date)){

    if(is.na(date[i])) {date[i] <- lubridate::NA_Date_}

    else{if(stringr::str_detect(date[i], ",")) {
      warning(paste0("Ambiguous date format (",date[i],"). Please provide format in parameters")) ; date[i] <- lubridate::NA_Date_}

      else{
        date[i] <- do.call(date[i], list(x[i])) %>% as.character()
      }}

    date[i] <- ifelse(is.na(date[i]), lubridate::as_date(x[i]) %>% as.character(), date[i])
  }

  date = lubridate::ymd(date)

  return(date)
}
