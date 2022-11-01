#' Shortcut to display a message and acceptation on prompt
#'
#' Shortcut allowing to provide user a prompt and a message that is to be read and
#' validated before pursuing process. This function is targeted for function creators
#' where user interaction is required.
#'
#' @param ... String character to put in a message
#'
#' @examples
#' \dontrun{
#' # example xxx
#'
#' message_on_prompt("Do you want continue? Press \dontrun{[enter]} to continue or
#' \dontrun{[esc]} to skip this part" )
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
message_on_prompt <- function(...){
  invisible(readline(cat(prompt = paste(...))))
}

#' Shortcut to silently run a code chunk avoiding error (with try), messages and warnings
#'
#' Shortcut avoiding user to get messages, warnings and being stopped by an error.
#' The usage is very similar to [base::suppressWarnings()]. This function is targeted
#' for function creators where user experience enhancement is sought.
#'
#' @param ... R code
#'
#'
#' @examples
#' \dontrun{
#' # example xxx
#'
#' silently_run()
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
silently_run <- function(...){
  return(suppressWarnings(suppressMessages(try(...,silent = TRUE))))
}

#' Shortcut to turn String character into R code
#'
#' Shortcut to [base::parse()] and [base::eval()]uate R expression in a character string,
#' and turn it into runable R code. This function is targeted for interaction with
#' external files (where expression is stored in text format) ; for tidy elements where
#' code expression is generated using [tidyr::mutate()], combined with [base::paste0()] ;
#' in for while, map, etc. loops where character string expression can be indexed or
#' iteratively generated and evaluated ; objects to be created (using assign, <- or
#' <<- obj) where the name of the R object is stored in a string. Some issues may occur
#' when parceval is used in a different environment, such as in a function.
#' Prefer eval(parse(text = ...) instead.
#'
#' @param ... String character to be parsed and evaluated
#'
#'
#' @examples
#' \dontrun{
#' # example xxx
#'
#'
#' # Simple assignation will assignate 'b' in parceval environment (which is associated
#' # to a function and different from .GlobalEnv, by definition).
#' # Double assignation will put 'b' in .GlobalEnv.
#' # (similar to assign(x = "b",value = 1,envir = .GlobalEnv))
#' a <- 1
#' parceval("b <<- 1")
#' print(a)
#' print(b)
#'
#' parceval("b <<- b + a")
#' print(b)
#'
#' my_code <- paste0("b <<- b + ",rep(1,3), "; message('value of b: ', b)")
#' parceval(my_code)
#'
#' # use rowwise to directly use parceval in a tibble, or use a for loop.
#' as_tibble(cars) %>%
#' mutate(
#'   to_eval = paste0(speed,"/",dist)) %>%
#' rowwise() %>%
#' mutate(
#'   eval = parceval(to_eval))
#'
#' # parceval can be parcevaled itself!
#'
#' code_R <-
#' 'as_tibble(cars) %>%
#'   mutate(
#'     to_eval = paste0(speed,"/",dist)) %>%
#'   rowwise() %>%
#'   mutate(
#'     eval = parceval(to_eval))'
#'
#' code_R %>% cat
#' code_R %>% parceval
#'
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
parceval <- function(...){
  eval(parse(text = stringr::str_squish(...) %>% stringr::str_remove_all("\\\r")))
}

#' Read all Excel sheets using readxl::read_excel recursively
#'
#' The Excel file is read and the values are placed in a list of tibbles, with each
#' sheet in a separate element in the list. If the Excel file has only one sheet,
#' the output is a single tibble. See [readxl::read_excel()]
#'
#' @param filename A character string of the path of the Excel file.
#' @param sheets A vector containing only the sheets to be read.
#'
#' @return A list of tibbles corresponding to the sheets read, or a single tibble
#' if the number of sheets is one.
#'
#' @examples
#' \dontrun{
#'
#' # xxx
#'
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
read_excel_allsheets <- function(filename, sheets = "") {

  if(toString(sheets) == ""){
    sheets_name <- readxl::excel_sheets(filename)
  }else{
    sheets_name <-
      readxl::excel_sheets(filename) %>%
      as_tibble %>% filter(.data$value %in% c(sheets)) %>%
      pull(.data$value)

    if(length(sheets_name) != length(sheets)){
      message("{",sheets[!(sheets %in% sheets_name)] %>% toString, "} sheet name(s) not found in the excel file")}}

  if(purrr::is_empty(sheets_name)){message("The sheet name(s) you provided do not exist")}else{
    x <- lapply(sheets_name,
                function(X) readxl::read_excel(
                  path      = filename,
                  sheet     = X,
                  guess_max = suppressWarnings(readxl::read_excel(filename, sheet = X) %>% nrow)))
    names(x) <- sheets_name
    if(length(x) == 1){return(x[[1]])}else{return(x)}
  }
}

#' Write all Excel sheets using xlsx::write.xlsx recursively
#'
#' The R objects are read and the values are placed in separated sheets.
#' This function is inspired by the function proposed in
#' https://statmethods.wordpress.com/2014/06/19/quickly-export-multiple-r-objects-to-an-excel-workbook/
#'
#' @param list R objects, coma separated.
#' @param filename A character string of the path of the Excel file.
#'
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
write_excel_allsheets <- function(list, filename){

  objnames <- list %>% names
  fargs <- as.list(match.call(expand.dots = TRUE))

  if(is.null(objnames)) {
    objnames <-
      as.character(fargs[['expand.dots']]) %>%
      stringr::str_remove("^list\\(") %>%
      stringr::str_remove("\\)$") %>%
      stringr::str_split(", ") %>% unlist
    names(list) <- objnames}

  fs::dir_create(dirname(filename))
  writexl::write_xlsx(x = list, path = filename)

  }

#' Read a csv file using read_csv and avoid errors
#'
#' The csv file is read twice to detect the number of lines to use in attributing
#' the column type ('guess_max' parameter of read_csv). This avoids common errors
#' when reading csv files.
#'
#' @param csv_name A character string of the path of the csv file.
#'
#' @return a tibble corresponding to the csv read.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
read_csv_any_formats <- function(csv_name){
  guess_max <-
    suppressMessages(suppressWarnings(readr::read_csv(csv_name, progress = FALSE))) %>% nrow

  csv <- readr::read_delim(file = csv_name, guess_max = guess_max)
  return(csv)
}

#' Add an index column at the first place of a tibble
#'
#' Add an index, possibly by group, at the first place of a data frame or a tibble
#' The name by default is 'index' but can be named. If 'index' already exists, or
#' the given name, the column can be forced to be created, and replace the other
#' one
#'
#' @param tbl tibble or data frame
#' @param name_index A character string of the name of the column.
#' @param start integer indicating firt index number. 1 by default.
#' @param .force TRUE or FALSE, that parameter indicates wheter or not the column
#' is created if already exists. FALSE by default.
#'
#' @return A tibble or a data frame containing one extra first column 'index' or
#' any given name.
#'
#' @examples
#' \dontrun{
#'
#' tbl = tibble(iris)
#' name_index = "Species"
#' .force = FALSE
#' .force = TRUE
#'
#' tibble(iris) %>% add_index
#' add_index(tibble(iris), name_index = 'Species')
#' add_index(tibble(iris), name_index = name_index, .force = TRUE)
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
add_index <- function(tbl, name_index = "index", start = 1, .force = FALSE){

  if(.force == FALSE){

    if(name_index %in% (tbl %>% names)){
      stop(paste0("\n\nThe column ",name_index," already exists.\n",
                  "Please specifie another name or use .force = TRUE\n"))}

    tbl <-
      tbl %>%
      tibble::add_column(!! name_index,.before = TRUE) %>%
      rename_with(.cols = all_of(paste0('"',!! name_index,'"')), ~ name_index) %>%
      mutate(across(all_of(name_index), ~ row_number() + start - 1))
    }

  if(.force == TRUE){
    tbl <-
      tbl %>%
      select(-any_of(name_index)) %>%
      tibble::add_column(!! name_index,.before = TRUE) %>%
      rename_with(.cols = all_of(paste0('"',!! name_index,'"')), ~ name_index) %>%
      mutate(across(all_of(name_index), ~ row_number() + start - 1))
    }

  return(tbl)
}


#' Get the paths of branches in a list
#'
#' Function that recursively go through a list object and store in a tibble the path
#' of each element in the list. The paths can be after that edited and accessed using
#' [fabR::parceval()] for example.
#'
#' @param list_obj R list object to be evaluated
#' @param .map_list non usable parameter. This parameter is only there to ensure
#' recursivity. Any modification of this object returns NULL
#'
#' @return A tibble containing all the paths of each element of the list and the
#' class of each leaf (can be a list, or R objects).
#'
#' @examples
#' \dontrun{
#'
#' MyList <-
#' list(
#'   My_iris = tibble(iris),
#'   My_car = list(
#'     car_data = tibble(cars),
#'     model_data = tibble(mtcars)
#'     )
#' )
#'
#' get_path_list(MyList)#'
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_path_list     <- function(list_obj, .map_list = NULL){

  if(is.null(.map_list)){

    .map_list <-
      tibble(root_name = quote(list_obj) %>% as.character()) %>%
      mutate(
        leaf_class = eval(parse(text = paste0(.data$root_name," %>% class %>% toString()"))))

    .map_list <- list(
      map_list = .map_list,
      big_list = .map_list
    )

    return(get_path_list(list_obj, .map_list))

  }else{

    while(stringr::str_detect(.map_list$map_list$leaf_class %>% toString, "list")){

      .map_list$map_list <-
        .map_list$map_list %>%
        rowwise() %>%
        mutate(
          leaf_name = names(eval(parse(text = .data$root_name))) %>% toString()) %>%
        tidyr::separate_rows(.data$leaf_name, sep = ", ") %>%
        rowwise() %>%
        mutate(
          leaf_class2  = eval(parse(text = paste0(.data$root_name,"[[",shQuote(.data$leaf_name),"]] %>% class %>% toString()"))),
          leaf_name   = ifelse(.data$leaf_class2 == "list", paste0("[[",shQuote(.data$leaf_name),"]]"),paste0("[",shQuote(.data$leaf_name),"]")),
          root_name   = ifelse(.data$leaf_class == "list",  paste0(.data$root_name,.data$leaf_name),.data$root_name),
          leaf_class = .data$leaf_class2) %>%
        select(.data$root_name, .data$leaf_class)

      .map_list$big_list <-
        .map_list$big_list %>%
        bind_rows(.map_list$map_list) %>%
        distinct

      return(get_path_list(list_obj, .map_list))}}

  return(.map_list$big_list)
}


#' Shortcut to create beautiful names in a list
#'
#' Generate a name for an element in a list. This function is targeted for functions
#' creations which handle lists. Those lists may need names to go through each elements.
#' This function works with [stats::setNames()] and allows the user to provide name
#' shorter, more user-friendly in their lists.
#'
#' @param args_list xxx xxx xxx
#' @param list_elem xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#'
#' # make_name_list generates names that are informative through a line of code or
#' function. tibble(iris), iris %>% tibble and list(iris = tibble(mytibble) %>% select(Species))
#' will have 'iris' as name,
#'
#' list(tibble(iris), tibble(mtcars)) %>%
#'   setNames(
#'     make_name_list(.,
#'        args_list = c(
#'           "IRIS %>% complicated_code",
#'           "complicated_function(MTCARS)")))
#'
#'
#' make_name_list can be used when a function uses arguments provided by the user
#' to generate a list. The name is simplified and given to the list itself
#' my_function = function(df){
#'
#'   .fargs <- as.list(match.call(expand.dots = TRUE))
#'
#'   list_df <-
#'     list(df) %>%
#'     stats::setNames(make_name_list(.,as.character(.fargs['df'])))
#'
#'   return(list_df)
#' }
#'
#' my_function(iris %>% as_tibble %>% select(Species))
#' my_function(iris %>% as_tibble %>% select(Species))
#'
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
make_name_list <- function(args_list, list_elem){

  name_list <-
    args_list %>%
    stringr::str_squish() %>%
    stringr::str_split(",") %>%
    unlist %>%
    stringr::str_remove_all("\\(\\)") %>%
    stringr::str_remove("\\=.*") %>%
    stringr::str_remove("\\%\\>\\%.*") %>%
    stringr::str_remove(".*\\([\\`]+") %>%
    stringr::str_remove("[\\`]+\\).*") %>%
    stringr::str_remove("\\[.*") %>%
    stringr::str_remove_all("\\`") %>%
    stringr::str_remove(".*\\(") %>%
    stringr::str_remove("\\).*") %>%
    stringr::str_remove("\\$.*") %>%
    stringr::str_squish()

  if(length(list_elem) != length(name_list)) {
    warning(
"\nThe names of your elements in your list might have been wrongly parsed. Please
verify the names of your elements and reparse.\n", call. = FALSE)
  }

  return(name_list[c(1:length(list_elem))])

}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param x xxx xxx xxx
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
#' @import dplyr stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
as_any_boolean <- function(x){

  # x1 = list(as.character(),
  #          as.logical(),
  #          NA,
  #          NA_complex_,
  #          "TRUE ",
  #          "TrUe",
  #          F,
  #          FALSE,
  #          "False",
  #          "bonjour",
  #          1,
  #          sqrt(2)^2/2 - 1)
  #
  # x2 = c(as.character(),
  #          as.logical(),
  #          NA,
  #          NA_complex_,
  #          "TRUE ",
  #          "TrUe",
  #          F,
  #          FALSE,
  #          "False",
  #          1)


  if(length(x) == 0) return(as.logical(x))
  if(typeof(x) == "logical") return(x)

  # check if the col is empty
  if(is.list(x) & nrow(x) %>% sum <= 1){ return(as_any_boolean(x = x[[1]])) }

  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to type 'logical'")

  x <- str_squish(x)

  xtemp <- x
  for(i in 1:length(x)){
    # stop()}

    xtemp[i] <-
      silently_run(case_when(
        is.na(x[i])                                          ~ NA_character_,
        toString(tolower(x[i]))   %in% c("1", "t","true")    ~ "TRUE" ,
        toString(as.numeric(x[i])) ==    "1"                 ~ "TRUE",
        toString(tolower(x[i]))   %in% c("0", "f","false")   ~ "FALSE",
        toString(as.numeric(x[i])) ==    "0"                 ~ "FALSE",
        TRUE                                                 ~ "NaN"
      ))
  }
    if(sum(as.character(xtemp) %in% "NaN") > 0){
      stop("x is not in a standard unambiguous format")
    }

  x <- as.logical(xtemp)
  return(x)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param x xxx xxx xxx
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
#' @import dplyr stringr
#' @importFrom magrittr %>%
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
    x = substitute(x)}

  return(x)
}




#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param folder_r xxx xxx xxx
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
#' @import dplyr tidyr stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
collect_roxygen <- function(folder_r = "R"){

  # collect
  idx <- file_index_create(folder_r)
  doc <- tibble()
  for(i in idx$file_path){doc <- bind_rows(doc,tibble(value = readLines(i), page = basename(i)))}

  doc <-
  # trim
    doc %>%
    mutate(value = str_squish(.data$`value`)) %>%
    filter(str_detect(.data$`value`,"^#'") | str_detect(.data$`value`, '<- function\\(')) %>%

  # classify
    mutate(
      class = ifelse(str_detect(.data$`value`,"^#' \\@export"),"EXPORT",NA_character_),
      class = ifelse(str_detect(.data$`value`,"^#' \\@examples"),"EXAMPLE",.data$`class`),
      class = ifelse(str_detect(.data$`value`,"^#' \\@import"),"IMPORT",.data$`class`),
      class = ifelse(str_detect(.data$`value`,"^#' \\@param"),"PARAM",.data$`class`),
      class = ifelse(str_detect(.data$`value`,"^#' \\@return"),"RETURN",.data$`class`),
      class = ifelse(str_detect(.data$`value`,"<- function\\("),"FUNCTION",.data$`class`))

  doc <-
    doc %>%
    mutate(class_2 = c("FUNCTION",doc$`class`[2:(length(doc$`class`))-1])) %>%
    mutate(class = ifelse(.data$`class_2` == "FUNCTION" & is.na(.data$`class`), "TITLE", .data$`class`)) %>%
    fill(.data$`class`,.direction = "down") %>%
    mutate(class = ifelse(.data$`class`   == "TITLE"    & is.na(.data$`class_2`), "DESCRIPTION", .data$`class`)) %>%
    fill(.data$`class`,.direction = "down") %>%
    mutate(class = ifelse(str_detect(.data$`value`,"^#'$"),"SPACE",.data$`class`))

  doc <-
    doc %>%
    mutate(class_2 = c("DESCRIPTION", doc$`class`[2:(length( doc$`class`))-1])) %>%
    mutate(class_3 = c("DESCRIPTION","DESCRIPTION", doc$`class`[3:(length( doc$`class_2`))-2])) %>%
    mutate(
      class_4 = ifelse(.data$`class`   == "DESCRIPTION" & .data$`class_2` == "SPACE" & .data$`class_3` == "TITLE", "DESCRIPTION", NA_character_),
      class_4 = ifelse(.data$`class`   == "DESCRIPTION" & .data$`class_2` == "SPACE" & .data$`class_3` == "DESCRIPTION", "ATTENTION", .data$`class_4`)) %>%
    fill(.data$`class_4`,.direction = "down") %>%
    mutate(
      class_4 = ifelse(.data$`class` == "DESCRIPTION",.data$`class_4`, NA_character_),
      class   = ifelse(.data$`class` == "DESCRIPTION",.data$`class_4`, .data$`class`)) %>% select(-.data$`class_2`,-.data$`class_3`,-.data$`class_4`) %>%
    filter(!.data$`class` %in% c("SPACE"))

    # pivot
  doc <-
    doc %>%
    group_by(.data$`class`) %>%
    add_index() %>%
    mutate(function_rank = ifelse(.data$`class` == 'FUNCTION',.data$`index` ,NA_integer_)) %>%
    ungroup () %>%
    select(-.data$`index`) %>%
    fill(.data$`function_rank`,.direction = "up") %>%
    group_by(.data$`function_rank`, .data$`class`, .data$`page`) %>%
    summarise(value = paste0(.data$`value`,collapse = "\n"),.groups = "keep") %>%
    pivot_wider(names_from = .data$`class`, values_from = .data$`value`) %>%
    ungroup %>%
    select(.data$`function_rank`, .data$`page`, .data$`FUNCTION`,.data$`TITLE`,matches('DESCRIPTION'), matches("ATTENTION"),
           matches("PARAM"), matches('EXAMPLE'), matches('IMPORT'), matches('EXPORT'))

  return(doc)
}
