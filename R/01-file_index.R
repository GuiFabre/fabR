#' @title
#' Create an index of files in a folder
#'
#' @description
#' Creates a tibble listing files in a specified folder (recursively) with file
#' path name and other useful metadata. This index can be used to quickly find
#' files in the environment. The index also generates script to read files as
#' R objects into the environment. Names for R objects are generated
#' automatically from file names (R objects are not created at this step but the
#' command line is generated and stored in the column to_eval, ready to be
#' evaluated and generate R objects).
#'
#' @details
#' The user must make sure their files are in the folder to be indexed.
#'
#' @param folder A character string identifying the folder to index. If not
#' specified, the current folder is the default
#' @param pattern A character string defining a pattern to sub-select within
#' folder. Can be useful for excluding certain folders from indexing (matching
#' by regex is supported).
#' @param negate logical. If TRUE, return non-matching elements.
#'
#' @return
#' A tibble with folder_path, file_path, file_name, extension, file_type
#' columns and a last column to_eval which is R code in a character vector to
#' read the file into the environment.
#'
#' @examples
#' \dontrun{
#'
#' file_index_create(tempdir())
#'
#' }
#'
#' @import dplyr fs stringr purrr tidyr
#' @importFrom rlang .data
#' @export
file_index_create <- function(folder = getwd(), pattern = "^", negate = FALSE){

  message(
    "Your files contained in your R environment are currently being indexed.
    Please wait...\n")

  index <- tibble(
    folder_path = path_abs(folder),
    file_path  = NA_character_,
    file_name  = NA_character_,
    extension = NA_character_,
    file_type = NA_character_,
    to_eval = NA_character_)

  if(is_file(path_abs(folder))){
    all_files_list <- folder
  }else{
    all_files_list <-
      str_subset(
        list.files(folder, full.names = TRUE, recursive = TRUE),
        pattern = pattern, negate = negate)}

  if(is_empty(all_files_list)){
    message("Your folder is empty or do not exists.")
  }else{

    for(i in all_files_list){
      index <-
        index %>%
        add_row(
          file_path = path_abs(i)) %>%
        fill(.data$folder_path,.direction = "down")}

    index <-
      index %>% dplyr::filter(!is.na(.data$file_path)) %>%
      mutate(
        file_name = paste0(basename(.data$file_path)),
        extension = path_ext(.data$file_path),
        extension =
          ifelse(nchar(.data$extension) == 0,.data$file_name,.data$extension),
        to_eval = case_when(
          extension %in%c("spss","sav")     ~
            paste0("haven::read_spss('",.data$file_path,"')"),
          extension %in%c("Rmd","md","R")   ~
            paste0("readLines('",.data$file_path,"') %>% as_tibble()"),
          extension == "dta"                ~
            paste0("haven::read_dta('",.data$file_path,"')"),
          extension %in%c("sas","sas7bdat") ~
            paste0("haven::read_sas('",.data$file_path,"')"),
          extension == "xlsx"               ~
            paste0("read_excel_allsheets('",.data$file_path,"',keep_as_list = TRUE)"),
          extension == "csv"                ~
            paste0("suppressMessages(
                   read_csv_any_formats('",.data$file_path,"'))"),
          TRUE                              ~
            NA_character_),
        file_type = case_when(
          tolower(extension) == "r"                    ~ "R script",
          tolower(extension) %in% c("rmd","md")        ~ "Markdown file",
          tolower(extension) == "html"                 ~ "html page",
          tolower(extension) == "rdata"                ~ "RData file",
          tolower(extension) == "sav"                  ~ "excel-like file",
          tolower(extension) == "dta"                  ~ "excel-like file",
          tolower(extension) == "sas7bdat"             ~ "excel-like file",
          tolower(extension) == "sas"                  ~ "excel-like file",
          tolower(extension) == "xlsx"                 ~ "excel-like file",
          tolower(extension) == "csv"                  ~ "excel-like file",
          tolower(extension) %in%c("png","jpg","jpeg") ~ "image",

          TRUE                                         ~ "other file")) %>%
      distinct

    message("\n\nYour files have been indexed.\n")

    return(index)

  }
}

#' @title
#' Search an index of files
#'
#' @description
#' Searches in file index R object (tibble) based on pattern and other query
#' options and provides a table where all the files in a specified folder and
#' corresponding to the query are listed (recursively). If no index tibble is
#' provided, the function creates one from the working directory.
#'
#' @details
#' The function displays the tree of your files. You can enable this
#' functionality with 'show_tree = TRUE'
#'
#' @param index The index (tibble) of a folder with file locations and metadata,
#' either previously generated by file_index_create() or created from folder.
#' @param file_path A character string specifying a file path to search by.
#' Can be the full string or substring (matching by regex is supported)
#' @param file_name A character string a file name to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param extension A character string a file extension to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param file_type A character string a file type to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param show_tree If TRUE, return the file tree of the query.
#'
#' @return
#' A tibble with indexed information for files matching the query.
#'
#' @examples
#' \dontrun{
#'
#' index <- file_index_create(tempdir())
#' file_index_search(index, file_name = my_file_name)
#'
#' }
#'
#' @import dplyr fs stringr
#' @importFrom rlang .data
#' @export
file_index_search <- function(
    index, file_path = "^",
    file_name = "^",
    extension = "^",
    file_type = "^",
    show_tree = FALSE){

  index <-
    index %>%
    dplyr::filter(str_detect(
      string = .data$file_path,
      pattern = !! file_path)) %>%
    dplyr::filter(str_detect(
      string = .data$file_name,
      pattern = !! file_name)) %>%
    dplyr::filter(str_detect(
      string = .data$file_type,
      pattern = !! file_type)) %>%
    dplyr::filter(str_detect(
      string = .data$extension,
      pattern = !! extension))

  if(show_tree == TRUE){
    # visualization of the dir tree
    temporary_folder <- path_temp() %>% basename
    dir_create(temporary_folder)
    folder_tp <- str_remove(
      string = index$file_path,
      pattern = dirname(index$folder_path))
    for(i in folder_tp){
      dir_create(paste0(temporary_folder,"/",dirname(i)))
      file_create(paste0(temporary_folder,"/",i))}
    dir_tree(paste0(temporary_folder))
    dir_delete(temporary_folder)
  }
  return(index)
}

#' @title
#' Read, source and open objects from an index of files
#'
#' @description
#' Reads all files from a file index tibble as R objects to generate in the
#' environment or R scripts to be sourced. Any other file types will be opened
#' in browser (html files) or in environment. If no index tibble is provided,
#' the function creates one from the working directory. (matching by regex is
#' supported).
#'
#' @details
#' for each file selected,
#' xlsx files will be read using the function [read_excel_allsheets()],
#' csv files will be read using the function [read_csv_any_formats()],
#' spss and sav files will be read using the function [haven::read_spss()],
#' dta files will be read using the function [haven::read_dta()],
#' sas7bdat and sas files will be read using the function [haven::read_sas()],
#' R scripts, Rmd and md files be read using the function [readLines()],
#' The whole files will be created in a list, which name is the name of the
#' file.
#'
#' @param index The index (tibble) of a folder with file locations and metadata,
#' either previously generated by file_index_create() or created from folder.
#' @param file_path A character string specifying a file path to search by.
#' Can be the full string or substring (matching by regex is supported)
#' @param file_name A character string a file name to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param extension A character string a file extension to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param file_type A character string a file type to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param assign If TRUE, the name is automatically assigned from the name
#' of the object read.
#' @param .envir The environment to use. parent.frame() by default
#'
#' @seealso
#' [read_excel_allsheets()], [read_csv_any_formats()], [haven::read_spss()],
#' [haven::read_dta()], [haven::read_sas()], [readLines()]
#'
#' @return
#' R objects generated in the environment or R scripts. R object names are
#' created automatically from their file names. Otherwise return messages
#' indicating what objects were created, or files opened, and if any troubles
#' occurred.
#'
#' @examples
#' \dontrun{
#'
#' index <- file_index_create(tempdir())
#' file_index_read(index, file_name = my_file_name)
#'
#' }
#'
#' @import dplyr stringr fs haven
#' @importFrom rlang .data
#' @importFrom usethis edit_file
#' @importFrom utils browseURL
#' @export
file_index_read <- function(
  index,
  file_path = "^",
  file_name = "^",
  extension = "^",
  file_type = "^",
  assign = FALSE,
  .envir = parent.frame()){

  index <-
    file_index_search(
      index = index,
      file_path = file_path,
      file_name = file_name,
      extension = extension,
      file_type = file_type) %>%
    dplyr::filter(!is.na(.data$to_eval))

  files_append <- list()

  if(nrow(index) > 0){

    for(i in seq_len(nrow(index))){

      if(str_detect(
        index$to_eval[i],
        "^source\\('|^utils::browseURL\\('|^usethis::edit_file\\('|^load\\('")){
        index$to_eval[i] %>% parceval()
        message(
          "the file: ",
          basename(index$file_path[i]),
          " has been sourced, loaded or opened in your environment")

      }else{

        if(assign == TRUE){

          try({assign(
            x = path_ext_remove(index$file_name[i]),
            value = index$to_eval[i] %>% parceval(),
            envir = .envir)
            message("the R object: ",index$file_name[i]," has been created")})

        }else{

          file_name <- path_ext_remove(index$file_name[i])
          files_append[[file_name]] <- index$to_eval[i] %>% parceval()

        }}}

    if(length(files_append) > 0) return(files_append)

  }else stop(call. = FALSE,
"The file(s) you try to read does not exists in the index, or cannot be read.")
}
