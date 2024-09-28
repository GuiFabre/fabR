#' @title
#' Evaluate and gives the best match to any date format using lubridate library
#'
#' @description
#' This function takes a tibble and a specific column. This column is evaluated
#' one observation after the other, and finally gives the best matching date
#' format for the whole column. The best matching format is tested across seven
#' different formats provided by the lubridate library. Along with the format,
#' the percentage of matching is given in the output tibble. The information of
#' the best matching format can be used to mutate a column using
#' [as_any_date()]. The default format is yyyy-mm-dd.
#'
#' @details
#' Contrary to lubridate library or [as.Date()], the function evaluates
#' the column as a whole, and does not cast the column if there is ambiguity
#' between values. For example, ('19-07-1983', '02-03-1982') implies that 02
#' refers to the day and 03 refers to the month, since that order works for the
#' first element, and doesn't otherwise.
#'
#' @param tbl R object(dataframe or tibble) of the input tbl
#' @param col A character string specifying a column of interest
#'
#' @seealso
#' [lubridate::ymd()],[lubridate::ydm()],[lubridate::dmy()],
#' [lubridate::myd()],[lubridate::mdy()],[lubridate::dym()],
#' [lubridate::my()] ,[lubridate::ym()],
#' [lubridate::as_date()],[as.Date()],
#' [which_any_date()],[as_any_date()]
#'
#' @return
#' A tibble with information concerning the best matching date format, given an
#' object to be evaluated.
#'
#' @examples
#' {
#'
#' library(tidyr)
#'
#' ##### Example 1 -------------------------------------------------------------
#' # Non-ambiguous dates ----------------------------------------------------
#' time <-
#'   tibble(time = c(
#'   "1983-07-19",
#'   "2003-01-14",
#'   "2010-09-29",
#'   "2023-12-12",
#'   "2009-09-03",
#'   "1509-11-30",
#'   "1809-01-01"))
#' guess_date_format(time)
#'
#' ##### Example 2 -------------------------------------------------------------
#' # Ambiguous dates ----------------------------------------------------
#' time <-
#'  tibble(time = c(
#'  "1983-19-07",
#'  "1983-10-13",
#'  "2009-09-03",
#'  "1509-11-30"))
#' guess_date_format(time)
#'
#'
#' ##### Example 3 -------------------------------------------------------------
#' # Non date format dates --------------------------------------------------
#' time <-
#'   tibble(time = c(
#'   "198-07-19",
#'   "200-01-14",
#'   "201-09-29",
#'   "202-12-12",
#'   "2000-09-03",
#'   "150-11-3d0",
#'   "180-01-01"))
#' guess_date_format(time)
#'
#' }
#'
#' @import dplyr lubridate tidyr stringr
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
    # stop()}

    column <-
      tbl %>%
      select(var = all_of(i)) %>%
      dplyr::filter(!is.na(.data$var)) %>%
      distinct()

    nb_test <- 8
    test_sample <-
      column %>%
      sample_n(min(nrow(column), 100)) %>%
      distinct() %>%
      rowwise() %>%
      mutate(
        ymd = ymd(.data$var, quiet = TRUE),
        ydm = ydm(.data$var, quiet = TRUE),
        dmy = dmy(.data$var, quiet = TRUE),
        myd = myd(.data$var, quiet = TRUE),
        mdy = mdy(.data$var, quiet = TRUE),
        dym = dym(.data$var, quiet = TRUE),
        my  =  my(.data$var, quiet = TRUE),
        ym  =  ym(.data$var, quiet = TRUE)) %>%
      ungroup %>%
      reframe(across(-c("var"), ~ sum(!is.na(.)))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Date format",
        values_to = "nb_values") %>%
      dplyr::filter(.data$`nb_values` == max(.data$`nb_values`))

    test_all <- column %>%
      mutate(ymd = NA_Date_ ,
             ydm = NA_Date_ ,
             dmy = NA_Date_ ,
             myd = NA_Date_ ,
             mdy = NA_Date_ ,
             dym = NA_Date_,
             my = NA_Date_ ,
             ym = NA_Date_)

    if(sum(test_sample$nb_values) != 0){

      if(sum(test_sample$`Date format` %in% "ymd") == 1)
        test_all <- test_all %>% mutate(ymd = ymd(.data$var, quiet = TRUE))

      if(sum(test_sample$`Date format` %in% "ydm") == 1)
        test_all <- test_all %>% mutate(ydm = ydm(.data$var, quiet = TRUE))

      if(sum(test_sample$`Date format` %in% "dmy") == 1)
        test_all <- test_all %>% mutate(dmy = dmy(.data$var, quiet = TRUE))

      if(sum(test_sample$`Date format` %in% "myd") == 1)
        test_all <- test_all %>% mutate(myd = myd(.data$var, quiet = TRUE))

      if(sum(test_sample$`Date format` %in% "mdy") == 1)
        test_all <- test_all %>% mutate(mdy = mdy(.data$var, quiet = TRUE))

      if(sum(test_sample$`Date format` %in% "dym") == 1)
        test_all <- test_all %>% mutate(dym = dym(.data$var, quiet = TRUE))

      if(sum(test_sample$`Date format` %in% "my") == 1)
        test_all <- test_all %>% mutate(my = my(.data$var, quiet = TRUE))

      if(sum(test_sample$`Date format` %in% "ym") == 1)
        test_all <- test_all %>% mutate(ym = ym(.data$var, quiet = TRUE))

    }


    test_col <-
      test_all %>%
      # ungroup %>%
      reframe(across(-'var', ~ sum(!is.na(.)))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Date format",
        values_to = "nb_values") %>%
      mutate(
        name_var = i,
        `% values formated` =
          round(100*(.data$nb_values / (
            tbl %>% select(var = all_of(i)) %>%
              distinct %>% dplyr::filter(!is.na(.data$var)) %>% nrow)),2),
        `% values formated` =
          ifelse(is.na(.data$`% values formated`),
                 0,.data$`% values formated`),
        `Date match` = case_when(
          .data$`% values formated` == 0   ~ "No match",
          .data$`% values formated` == 100 ~ paste0("Exact match"),
          TRUE                       ~ paste0("Ambiguous match")),
      ) %>%
      group_by(
        .data$name_var,.data$`Date match`,.data$`% values formated`,
        .data$nb_values) %>%
      reframe(
        "Date format" = paste0(.data$`Date format`,collapse = ", ")) %>%
      arrange(-.data$nb_values) %>%
      slice(1) %>%

      mutate(
        `Date format` = ifelse(
          str_count(.data$`Date format`,",") == nb_test - 1, "All formats",.data$`Date format`),

        `Date match` = ifelse(str_detect(.data$`Date format`,","),
          "Ambiguous match",.data$`Date match`)

        # `Date format` = ifelse(
        #   .data$`% values formated` == 100 & ,
        #   str_split_1(.data$`Date format`,",")[[1]],.data$`Date format`),
      ) %>%
      select(c("name_var","Date format","% values formated","Date match"))

    test <- bind_rows(test,test_col)
  }

  return(test)
}

#' @title
#' Evaluates and gives the possible format(s) for an object to be evaluated
#'
#' @description
#' This function takes a character string or a vector. This vector is evaluates
#' one observation after the other, and gives the best matching date format
#' for each of them (independently). The best matching format is tested across
#' seven different formats provided by the lubridate library. The information of
#' the best matching format can be used to mutate a column using
#' [as_any_date()].
#'
#' @details
#' Contrary to lubridate library or [as.Date()], the function evaluates
#' the different possibilities for a date. For example, c('02-03-1982') can be
#' either March the 2nd or February the 3rd. The function will provide
#' "mdy, dmy" as possible formats. If no format is found, the function returns
#' NA.
#'
#' @param x object to be coerced. Can be a character string or a vector.
#' @param format A character identifying the format to apply to the object to
#' test.
#' That format can be 'ymd','ydm','dmy','myd','mdy','dym', 'ym', 'my' or
#' 'as_date' in that specific order ('ymd" will be chose as a default format,
#' then 'ymd', etc.).
#'
#' @return
#' A character string of the possible date formats given a parameter to be
#' tested. The length of the vector is the length of the input object.
#'
#' @seealso
#' [lubridate::ymd()],[lubridate::ydm()],[lubridate::dmy()],
#' [lubridate::myd()],[lubridate::mdy()],[lubridate::dym()],
#' [lubridate::my()] ,[lubridate::ym()],
#' [lubridate::as_date()],[as.Date()],
#' [guess_date_format()],[as_any_date()]
#'
#' @examples
#' {
#'
#' time <- c(
#'   "1983-07-19",
#'   "31 jan 2017",
#'   "1988/12/17",
#'   "31-02-05",
#'   "02-02-02",
#'   "2017 october the 2nd",
#'   "02-07-2012",
#'   "19-07-83",
#'   "19-19-19")
#'
#' which_any_date(time)
#'
#' }
#'
#' @import dplyr lubridate stringr
#' @importFrom rlang .data
#' @export
which_any_date <- function(
    x,format = c("ymd","ydm","dmy","myd","mdy","dym","my", "ym","as_date")){

  test <- c()
  x_origin <- x
  x <- unique(x)

  if(length(x) == 0) return(as.Date())

  for(i in seq_len(length(x))){
    # stop()}

    if(is.na(x[i])){ test[i] <- NA_character_ }
    else{

      test[i] <-
        c(
          if("ymd" %in% format & !is.na(ymd(x[i], quiet = TRUE))) "ymd",
          if("ydm" %in% format & !is.na(ydm(x[i], quiet = TRUE))) "ydm",
          if("dmy" %in% format & !is.na(dmy(x[i], quiet = TRUE))) "dmy",
          if("myd" %in% format & !is.na(myd(x[i], quiet = TRUE))) "myd",
          if("mdy" %in% format & !is.na(mdy(x[i], quiet = TRUE))) "mdy",
          if("dym" %in% format & !is.na(dym(x[i], quiet = TRUE))) "dym",

          if("my" %in% format & !is.na(my(x[i], quiet = TRUE))) "my",
          if("ym" %in% format & !is.na(ym(x[i], quiet = TRUE))) "ym",

          if("as_date" %in% format & !is.na(suppressWarnings(as_date(x[i]))))
            "as_date") %>%
        toString }}

  test <-
    test %>%
    na_if("") %>%
    str_remove(pattern = ", as_date")

  test

  test <-
    full_join(tibble(x = x_origin),tibble(test, x),by = 'x') %>%
    mutate(test = ifelse(str_detect(.data$`test`,"ymd"),"ymd",.data$`test`)) %>%
    pull(.data$`test`)

  return(test)
}

#' @title
#' Create objects of class "Date"
#'
#' @description
#' This function takes a character string or a vector. This vector is evaluates
#' one observation after the other, and casts the best matching date format
#' for each of them (independently). The best matching format is tested across
#' seven different formats provided by the lubridate library. The user can
#' specify the wanted matching format (and can be helped using
#' [which_any_date()] for each value or [guess_date_format()]
#' for the values as a whole.
#'
#' @details
#' Contrary to lubridate library or [as.Date()], the function evaluates
#' the different possibilities for a date. For example, c('02-03-1982') can be
#' either March the 2nd or February the 3rd. The function will cast the value as
#' NA, and a warning, since there is an ambiguity that cannot be solved, unless
#' the user provides the format to apply.
#'
#' @param x object to be coerced.
#' @param format A character identifying the format to apply to the object.
#' That format can be 'ymd','ydm','dym','dmy','mdy','myd','my','ym'.
#'
#' @seealso
#' [lubridate::ymd()],[lubridate::ydm()],[lubridate::dmy()],
#' [lubridate::myd()],[lubridate::mdy()],[lubridate::dym()],
#' [lubridate::my()] ,[lubridate::ym()],
#' [lubridate::as_date()],[as.Date()],
#' [guess_date_format()],[which_any_date()]
#'
#' @return
#' A R Object of class 'Date'.
#'
#'
#' @examples
#' {
#'
#' library(dplyr)
#' library(tidyr)
#'
#' ##### Example 1 -------------------------------------------------------------
#' # Ambiguous dates -----------------------------------------------------------
#' as_any_date('19 02 12')
#' as_any_date('19 02 12', format = "ymd")
#' as_any_date('19 02 12', format = "dym")
#'
#' ##### Example 2 -------------------------------------------------------------
#' # Non-ambiguous dates -------------------------------------------------------
#' time <-
#'   tibble(time = c(
#'   "1983 07-19",
#'   "14-01-1925",
#'   "12/13/2015",
#'   "2009-09-13",
#'   "17-12-12",
#'   "coucou",
#'   "2025 jan the 30th",
#'   "1809-01-19"))
#'
#' time %>% mutate(new_time = as_any_date(time))
#'
#' }
#'
#' @import dplyr lubridate stringr
#' @importFrom rlang .data
#' @export
as_any_date <- function(
    x = as.character(),
    format = c("dmy","dym","ymd","ydm","mdy","myd","my", "ym", "as_date")){

  if(length(x) == 0) return(as.Date())

  date_guess <- guess_date_format(tibble(x))

  if(date_guess$`Date match` == "Exact match"){
    date_test <- rep(date_guess$`Date format`,length(x))
  }else{
    date_test <- which_any_date(x, format)
  }

  wrn3 <- wrn4 <- 0

  for(i in seq_len(length(date_test))){
  # stop()}

    if(!is.na(date_test[i]) & length(x) == 0){date_test <- as.character(x)}
    else if(
      is.na(date_test[i]) & is.na(x[i])){date_test[i] <- NA_Date_}
    else if(
      is.na(date_test[i]) & !is.na(x[i])){date_test[i] <- NA_Date_;wrn3 <- i}
    else if(
      str_detect(date_test[i], ",")){date_test[i] <- NA_Date_;wrn4 <- i}
  }

  if(date_guess$`Date match` == "Exact match"){
    date_test <- as.character(do.call(date_test[1], list(x)))
  }else{

    date_test <-
      tibble(x,date_test) %>%
      rowwise() %>%
      mutate(
        x = ifelse(is.na(.data$`date_test`),
                   NA_character_,
                   as.character(eval(parse(text =
                   paste0("lubridate::",.data$`date_test`,"('",.data$`x`,"')")
        ))))) %>%
      pull(.data$`x`)
  }


  if(wrn3 > 0)
    warning(
      call. = FALSE,
      "All formats failed to parse for some values.",
      "\n","\n\nUseful tip:"," Use which_any_date(x) to get formats.")

  if(wrn4 > 0)
    warning(
      call. = FALSE,
      "Ambiguous date format (",x[wrn4],"). Please provide format.",
      "\n","\n\nUseful tip:"," Use which_any_date(x) to get formats.")

  date_final <- ymd(date_test)

  return(date_final)
}

