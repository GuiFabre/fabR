#' @title
#' Read all Excel sheets using [readxl::read_excel()] recursively
#'
#' @description
#' The Excel file is read and the values are placed in a list of tibbles, with
#' each sheet in a separate element in the list. If the Excel file has only one
#' sheet, the output is a single tibble.
#'
#' @param filename A character string of the path of the Excel file.
#' @param sheets A vector containing only the sheets to be read.
#' @param keep_as_list A Boolean to say whether the object should be a list or
#' a tibble, when there is only one sheet provided. FALSE by default.
#'
#' @return
#' A list of tibbles corresponding to the sheets read, or a single tibble
#' if the number of sheets is one.
#'
#' @seealso
#' [readxl::read_excel()]
#'
#' @examples
#' {
#'
#' try(read_excel_allsheets(filename = tempfile()), silent = TRUE)
#'
#' }
#'
#' @import dplyr readxl purrr
#' @importFrom rlang .data
#' @export
read_excel_allsheets <- function(filename, sheets = "", keep_as_list = FALSE) {

  if(toString(sheets) == ""){
    sheets_name <- excel_sheets(filename)
  }else{
    sheets_name <-
      excel_sheets(filename) %>%
      as_tibble %>% dplyr::filter(.data$value %in% c(sheets)) %>%
      pull(.data$value)

    if(length(sheets_name) != length(sheets)){
      stop(call. = FALSE,
      "{",sheets[(sheets %in% sheets_name)] %>% toString, "}
Sheet name(s) not found in the excel file")}}

  if(is_empty(sheets_name)){
    stop(call. = FALSE, "The sheet name(s) you provided do not exist")}else{
      x <- lapply(sheets_name,
                  function(X) read_excel(
                    path      = filename,
                    sheet     = X,
                    guess_max =
                      suppressWarnings(
                        read_excel(filename, sheet = X) %>% nrow)))
      names(x) <- sheets_name
      if(length(x) == 1 & keep_as_list == FALSE){return(x[[1]])}else{return(x)}
    }
}


#' @title
#' Write all Excel sheets using [writexl::write_xlsx()] recursively
#'
#' @description
#' The R objects are read and the values are placed in separated sheets.
#' This function is inspired by the function proposed in
#' https://statmethods.wordpress.com/2014/06/19/quickly-export-multiple-r-objects-to-an-excel-workbook/
#'
#' @param list R objects, coma separated.
#' @param filename A character string of the path of the Excel file.
#'
#' @seealso
#' [writexl::write_xlsx()]
#'
#' @return
#' Nothing to be returned. The file is created at the path declared in the
#' environment.
#'
#' @examples
#' {
#'
#' unlink(
#'   write_excel_allsheets(
#'     list = list(iris = iris, mtcars = mtcars),
#'     filename = tempfile()))
#'
#' }
#'
#' @import dplyr stringr fs writexl
#' @importFrom rlang .data
#' @export
write_excel_allsheets <- function(list, filename){

  objnames <- list %>% names
  fargs <- as.list(match.call(expand.dots = TRUE))

  if(is.null(objnames)) {
    objnames <-
      as.character(fargs[['expand.dots']]) %>%
      str_remove("^list\\(") %>%
      str_remove("\\)$") %>%
      str_split(", ") %>% unlist
    names(list) <- objnames}

  dir_create(dirname(filename))
  write_xlsx(x = list, path = filename)

}


#' @title
#' Create objects of type "integer".
#'
#' @description
#' Create or test for objects of type "integer".
#' This function is a wrapper of the function [as.integer()] and evaluates
#' if the object to be coerced can be interpreted as a integer. Any object :
#' NA, NA_integer, NA_Date_, (...),
#' Boolean, such as 0, 0L, F, FALSE, false, FaLsE, (...),
#' Any string "1", "+1", "-1", "1.0000"
#' will be converted as NA or integer. Any other other will return an
#' error.
#'
#' @param x Object to be coerced or tested. Can be a vector.
#'
#' @return
#' An integer object of the same size.
#'
#' @seealso
#' [as.logical()]
#'
#' @examples
#' {
#'
#' library(dplyr)
#'
#' as_any_integer("1")
#' as_any_integer(c("1.000","2.0","1","+12","-12"))
#' try(as_any_integer('foo'))
#'
#' tibble(values = c("1.000","2.0","1","+12","-12")) %>%
#'   mutate(bool_values = as_any_integer(values))
#'
#' }
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#' @importFrom rlang is_integerish
#'
#' @export
as_any_integer <- function(x){

  err <- FALSE

  if(class(x)[[1]] == "Date"){ err <- TRUE }else{
    if(length(x)     == 0)                     return(as.integer(x))
    if(all(is.na(x)))                          return(as.integer(x))
    if(all(str_squish(x) %in% c("","NaN",NA))) return(as.integer(x))
    if(is_integerish(x))                       return(x)
  }

  # check if the col is empty
  if(is.list(x) & nrow(x) %>% sum <= 1){ return(as_any_integer(x = x[[1]])) }

  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to type 'integer'")


  xtemp_bool <- silently_run(as_any_boolean(x))
  if(is.logical(xtemp_bool)){
    x <- as.integer(xtemp_bool)
    return(x)
  }

  xtemp_init <- unique(str_squish(x))
  xtemp_init <- x[!is.na(x)]

  xtemp_num  <- silently_run(as.numeric(xtemp_init))
  xtemp_int  <- silently_run(as.integer(xtemp_init))

  if(sum(is.na(xtemp_num)) >= 1) err <- TRUE
  if(sum(is.na(xtemp_int)) >= 1) err <- TRUE
  if(!all(toString(as.character(xtemp_num)) ==
          toString(as.character(xtemp_int)))) err <- TRUE

  if(err == TRUE)
    stop(
      "x is not in a standard unambiguous format to be coerced into type 'integer'")

  x <- as.integer(x)

  return(x)
}

#' @title
#' Create objects of type "logical".
#'
#' @description
#' Create or test for objects of type "logical", and the basic logical
#' constants.
#' This function is a wrapper of the function [as.logical()] and evaluates
#' if the object to be coerced can be interpreted as a boolean. Any object :
#' NA, NA_integer, NA_Date_, (...),
#' 0, 0L, F, FALSE, false, FaLsE, (...),
#' 1, 1L,T,  TRUE,  true, TrUe, (...),
#' will be converted as NA, FALSE and TRUE. Any other other will return an
#' error.
#'
#' @param x Object to be coerced or tested. Can be a vector.
#'
#' @return
#' An logical object of the same size.
#'
#' @seealso
#' [as.logical()]
#'
#' @examples
#' {
#'
#' library(dplyr)
#'
#' as_any_boolean("TRUE")
#' as_any_boolean(c("1"))
#' as_any_boolean(0L)
#' try(as_any_boolean(c('foo')))
#' as_any_boolean(c(0,1L,0,TRUE,"t","F","FALSE"))
#' tibble(values = c(0,1L,0,TRUE,"t","F","FALSE")) %>%
#'   mutate(bool_values = as_any_boolean(values))
#'
#' }
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @export
as_any_boolean <- function(x){

  if(length(x)     == 0)         return(as.logical(x))
  if(all(is.na(x)))              return(as.logical(x))
  if(typeof(x)     == "logical") return(x)

  # check if the col is empty
  if(is.list(x) & nrow(x) %>% sum <= 1){ return(as_any_boolean(x = x[[1]])) }

  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to type 'logical'")

  x <- str_squish(x)

  xtemp <- tibble(x = unique(x), xtemp = unique(x))
  for(i in seq_len(nrow(xtemp))){
    # stop()}

    if(is.na(xtemp$xtemp[i]))
      xtemp$xtemp[i] <- NA_character_ else
    if(toString(tolower(xtemp$xtemp[i])) %in% c("1","t","true"))
      xtemp$xtemp[i] <- "TRUE" else
    if(silently_run(toString(as.numeric(xtemp$xtemp[i]))) == "1")
      xtemp$xtemp[i] <- "TRUE" else
    if(toString(tolower(xtemp$xtemp[i])) %in% c("0", "f","false"))
      xtemp$xtemp[i] <- "FALSE" else
    if(silently_run(toString(as.numeric(xtemp$xtemp[i]))) == "0")
      xtemp$xtemp[i] <- "FALSE" else xtemp$xtemp[i] <- "NaN"

    if(toString(xtemp$xtemp[i]) == "NaN")
      stop("x is not in a standard unambiguous format")
  }

  xtemp <- tibble(x = x) %>% left_join(xtemp,by = 'x') %>% pull('xtemp')

  x <- as.logical(xtemp)
  return(x)
}


#' @title
#' Create objects of type "symbol"
#'
#' @description
#' Create or test for objects of type "symbol".
#'
#' @param x Object to be coerced or tested. Can be a vector, a character string,
#' a symbol.
#'
#' @return
#' Object of type "symbol".
#'
#' @examples
#' {
#'
#' as_any_symbol(coucou)
#' as_any_symbol("coucou")
#'
#' }
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#' @export
as_any_symbol <- function(x){

  if(class(try(eval(x),silent = TRUE))[1] == "try-error"){
    x <- substitute(x)
  }else if(class(try(as.symbol(x),silent = TRUE))[1] == "try-error"){
    x <- as.symbol(as.list(match.call(expand.dots = TRUE))[['x']])
  }else{
    x <- as.symbol(x)}

  if(as.symbol(x) == 'x' & typeof(substitute(x)) == "symbol"){
    x <- substitute(x)}

  return(x)
}

