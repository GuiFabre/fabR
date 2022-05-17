#' xxx
#'
#' xxx
#'
#' @param ... String character to put in a message
#'
#' @return xxx
#'
#' @examples
#' \dontrun{
#' # example xxx
#'
#' xxx
#'
#' }
#'
#' @export
message_on_prompt <- function(...){
  invisible(readline(cat(prompt = paste(...))))
}

#' xxx
#'
#' xxx
#'
#' @param ... String character to put in a message
#'
#' @return A list of tibbles corresponding to the sheets read, or a single tibble
#' if the number of sheets is one.
#'
#' @examples
#' \dontrun{
#' # example xxx
#'
#' xxx
#'
#' }
#'
#' @export
silently_run <- function(...){
  return(suppressWarnings(suppressMessages(try(...,silent = TRUE))))
}

#' xxx
#'
#' xxx
#'
#' @param ... String character to put in a message
#'
#' @return A list of tibbles corresponding to the sheets read, or a single tibble
#' if the number of sheets is one.
#'
#' @examples
#' \dontrun{
#' # example xxx
#'
#' xxx
#'
#' }
#'
#' @import dplyr magrittr
#' @export
parceval <- function(...){
  eval(parse(text = str_squish(...) %>% str_remove_all("\\\r")))
}

#' Read all Excel sheets using readxl::read_excel recursively
#'
#' The Excel file is read and the content is placed in a list of tibbles, with each
#' sheet in a separate element in the list. If the Excel file has only one sheet,
#' the output is a single tibble.
#'
#' @param filename A character string of the path of the Excel file.
#' @param sheets A vector containing only the sheets to be read.
#'
#' @return A list of tibbles corresponding to the sheets read, or a single tibble
#' if the number of sheets is one.
#'
#' @examples
#' \dontrun{
#' # xxx
#'
#'
#' }
#'
#' @import readxl dplyr magrittr tibble
#' @export
read_excel_allsheets <- function(filename, sheets = "") {

  if(toString(sheets) == ""){
    sheets_name <- excel_sheets(filename)
  }else{
    sheets_name <- excel_sheets(filename) %>% as_tibble %>% filter(.data$value %in% c(sheets)) %>% pull(.data$value)
    if(length(sheets_name) != length(sheets)){
      message("{",sheets[!(sheets %in% sheets_name)] %>% toString, "} sheet name(s) not found in the excel file")}}

  if(purrr::is_empty(sheets_name)){message("The sheet name(s) you provided do not exist")}else{
    x <- lapply(sheets_name,
                function(X) read_excel(
                  path      = filename,
                  sheet     = X,
                  guess_max = suppressWarnings(read_excel(filename,sheet = X) %>% nrow)))
    names(x) <- sheets_name
    if(length(x) == 1){return(x[[1]])}else{return(x)}
  }
}

#' Write all Excel sheets using xlsx::write.xlsx recursively
#'
#' The R objects are read and the content is placed in separated sheets.
#' This function is inspired by the function proposed in
#' https://statmethods.wordpress.com/2014/06/19/quickly-export-multiple-r-objects-to-an-excel-workbook/
#'
#' @param filename A character string of the path of the Excel file.
#' @param list R objects, coma separated.
#'
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx
#' }
#'
#' @import writexl fs stringr
#' @export
write_excel_allsheets <- function (filename, list){

  objnames <- list %>% names
  fargs <- as.list(match.call(expand.dots = TRUE))

  if(is.null(objnames)) {
    objnames <-
      as.character(fargs[3]) %>%
      str_remove("^list\\(") %>%
      str_remove("\\)$") %>%
      str_split(", ") %>% unlist
    names(list) <- objnames}

  dir_create(dirname(filename))
  write_xlsx(x = list, path = filename)

  }

#' Read a csv file using readr::read_csv and avoid errors
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
#' @import readr dplyr magrittr
#' @export
read_csv_any_formats <- function(csv_name){
  guess_max <- suppressMessages(suppressWarnings(read_csv(csv_name, progress = FALSE))) %>% nrow
  csv <- read_delim(file = csv_name, guess_max = guess_max)
  return(csv)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param tibble xxx xxx xxx
#' @param name_index xxx xxx xxx
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
#' @export
add_index <- function(tibble, name_index = "index"){

  tibble <- tibble %>% add_column(!!! name_index) %>%
    select(last_col(), everything())

  repair_name <- tibble %>% ungroup() %>% select(1) %>% clean_names() %>% names
  tibble_name <- tibble %>% ungroup() %>% select(-1) %>% names

  tibble <-
    stats::setNames(tibble, c(repair_name,tibble_name)) %>%
    mutate(across(all_of(name_index), ~ row_number()))
  return(tibble)  }


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param map_list xxx xxx xxx
#' @param .obj xxx xxx xxx
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
#' @export
get_index_list     <- function(.obj, map_list = NULL){

  if(is.null(map_list)){

    map_list <-
      tibble(root_name = quote(.obj) %>% as.character()) %>%
      mutate(
        leaf_class = eval(parse(text = paste0(.data$root_name," %>% class %>% toString()"))))

    map_list <- list(
      map_list = map_list,
      big_list = map_list
    )

    return(get_index_list(.obj, map_list))

  }else{

    while(str_detect(map_list$map_list$leaf_class %>% toString, "list")){

      map_list$map_list <-
        map_list$map_list %>%
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

      map_list$big_list <-
        map_list$big_list %>%
        bind_rows(map_list$map_list) %>%
        distinct

      return(get_index_list(.obj, map_list))}}

  return(map_list$big_list)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param args_list xxx xxx xxx
#' @param list_elem xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx xxx xxx.
#' # Example 1: xxx
#'
#' }
#'
#' @import dplyr magrittr
#' @export
make_name_list <- function(args_list, list_elem){

  name_list <-
    args_list %>%
    str_squish() %>%
    str_split(",") %>%
    unlist %>%
    str_remove_all("\\(\\)") %>%
    str_remove("\\=.*") %>%
    str_remove("\\%\\>\\%.*") %>%
    str_remove(".*\\([\\`]+") %>%
    str_remove("[\\`]+\\).*") %>%
    str_remove("\\[.*") %>%
    str_remove_all("\\`") %>%
    str_remove(".*\\(") %>%
    str_remove("\\).*") %>%
    str_remove("\\$.*") %>%
    str_squish()

  if(length(list_elem) != length(name_list)) {
    warning(
      "\nThe names of your elements in your list might have been wrongly parsed. Please
verify the names of your elements and reparse.\n", call. = FALSE)
  }

  return(name_list[c(1:length(list_elem))])

}

