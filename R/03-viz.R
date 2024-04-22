#' @title
#' Create a bookdown template.
#'
#' @description
#' This helper function creates a template for a bookdown.
#'
#' @param bookdown_path A character string identifying the folder path where
#' the bookdown will be generated.
#'
#' @param overwrite whether to overwrite existing files. FALSE by default.
#'
#' @return
#' A folder containing all files (Rmd, yml, css) to generate the bookdown.
#'
#' @seealso [bookdown_render()],[bookdown_open()]
#'
#' @examples
#' {
#'
#' bookdown_path = tempdir()
#' bookdown_template(bookdown_path, overwrite = TRUE)
#'
#' }
#'
#' @import dplyr fs utils
#' @importFrom rlang .data
#' @importFrom readr write_lines
#' @export
bookdown_template <- function(bookdown_path, overwrite = FALSE){

  # check input
  if(!is.logical(overwrite))
    stop(call. = FALSE,'`overwrite` must be TRUE or FALSE (TRUE by default).')

  if(!is.character(bookdown_path))
    stop(call. = FALSE,'`bookdown_path` must be a character string.')

  if(!dir.exists(bookdown_path)) dir_create(bookdown_path)

  if(overwrite == FALSE){

    if(length(dir(bookdown_path,
       pattern =
       "^_bookdown.yml$|^_output\\.yml$|^style\\.css$|^index\\.Rmd$")) > 0){

      stop(call. = FALSE,
'The path folder already exists or is not empty.
Please provide another name folder or add `overwrite = TRUE` as parameter.')

    }
  }

  paste0(
'book_filename: "bookdownproj"
output_dir: docs
delete_merged_file: true
language:
  ui:
    chapter_name: ""

') %>% write_lines(
  file = paste0(bookdown_path,"/_bookdown.yml"),append = FALSE)

paste0(
'bookdown::gitbook:
  css: style.css
  config:
    sharing: null
    toc:
      before: |
        <li><a href="./">SHORT TITLE HERE</a></li>
      after: |
        <li><a
          href="https://github.com/rstudio/bookdown"
          target="blank">Published with bookdown</a></li>

') %>% write_lines(
  file = paste0(bookdown_path,"/_output.yml"),append = FALSE)


  paste0(
'body{ /* Normal  */
      font-size: 14px;
  }
td {  /* table  */
  font-size: 12px;
}
h1.title {
  font-size: 28px;
  color: DarkRed;
}
h1 { /* Header 1 */
  font-size: 22px;
  color: DarkBlue;
}
h2 { /* Header 2 */
    font-size: 14px;
  color: DarkBlue;
}
h3 { /* Header 3 */
  font-size: 28px;
  color: green;
}
h4 { /* Header 4 */
  font-size: 12px;
  font-style: italic;
  color: DarkRed;
}
code.r{ /* Code block */
    font-size: 14px;
}
pre { /* Code block - determines code spacing between lines */
    font-size: 14px;
}
.center {
   width: 70%;
   margin-right: auto;
}
.twoColomns {
   width: 90%;
   display: flex;
   margin-right: auto;
}

.twoColomns {
   width: 90%;
   display: flex;
   margin-right: auto;
}

') %>% write_lines(
  file = paste0(bookdown_path,"/style.css"), append = FALSE)

  paste0(
'---
title: "XXX"
author: "xxx"
date: "`r Sys.Date()`"
site: bookdown::bookdown_site
---

# XXX

') %>% write_lines(
  file = paste0(bookdown_path,"/index.Rmd"),
  append = FALSE)

}

#' @title
#' Render a bookdown into a bookdown site
#'
#' @description
#' This helper function renders an existing bookdown folder (containing at
#' least 'index.Rmd file)
#'
#' @param bookdown_path A character string identifying the folder path
#' where the bookdown report files are.
#'
#' @param overwrite whether to overwrite existing files. FALSE by default.
#'
#' @return
#' A folder containing htlm files (in docs, ...) generated from a bookdown
#' report.
#'
#' @seealso [bookdown_template()],[bookdown_open()]
#'
#' @examples
#' {
#'
#' bookdown_path = tempdir()
#' bookdown_template(bookdown_path, overwrite = TRUE)
#' bookdown_render(bookdown_path, overwrite = TRUE)
#'
#' }
#'
#' @import bookdown
#' @importFrom xfun in_dir
#' @importFrom rlang .data
#' @importFrom readr write_lines
#' @export
bookdown_render <- function(bookdown_path, overwrite = FALSE){

  # check input
  if(!is.logical(overwrite))
    stop(call. = FALSE,'`overwrite` must be TRUE or FALSE (TRUE by default).')

  if(!is.character(bookdown_path))
    stop(call. = FALSE,'`bookdown_path` must be a character string.')

  if(!dir.exists(bookdown_path))
    stop(call. = FALSE, 'The path folder does not exists')

  if(overwrite == FALSE){

    if(length(dir(bookdown_path,
       pattern =
       "^docs$")) > 0){

      stop(call. = FALSE,
'The path folder already exists or is not empty.
Please provide another name folder or add `overwrite = TRUE` as parameter.')

    }
  }

  dir_delete(dir(bookdown_path,pattern = "^docs$",full.names = TRUE))

  suppressMessages({
    in_dir(
      dir = bookdown_path,
      expr = render_book(
        input = paste0("index.Rmd")))
  })

}

#' @title
#' Open a a web-based bookdown folder in a browser
#'
#' @description
#' Opens a previously generated HTML bookdown site from files in the specified
#' folder. This is a shortcut function to access 'index.html' in the specified
#' folder.
#'
#' @seealso [bookdown_template()],[bookdown_open()]
#'
#' @param bookdown_path A character string identifying the folder path
#' containing the files to open the bookdown site.
#'
#' @returns
#' Nothing to be returned. The function opens a web page.
#'
#' @examples
#' {
#'
#' bookdown_path = tempdir()
#' bookdown_template(bookdown_path, overwrite = TRUE)
#' bookdown_render(bookdown_path, overwrite = TRUE)
#' bookdown_open(bookdown_path)
#'
#' }
#'
#' @import stringr
#' @importFrom utils browseURL
#'
#' @export
bookdown_open <- function(bookdown_path){

  bookdown_path <- str_remove(paste0(bookdown_path,"/docs/index.html"), '^/')
  utils::browseURL(bookdown_path)

}


