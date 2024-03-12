#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [bookdown_template()] instead of `template_visual_report()`.
#'
#' @name deprecated
#' @keywords internal
#' @import dplyr
#' @importFrom lifecycle deprecate_warn
#' @export
template_visual_report <- function(...) {

  deprecate_warn(
    "2.0.0", "template_visual_report(to)", "bookdown_template(bookdown_path)")

  # Unquote-splice to avoid argument matching

  bookdown_template(...)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [fabR_website()] instead of `fabR_help()`.
#'
#' @name deprecated
#' @keywords internal
#' @import dplyr
#' @importFrom lifecycle deprecate_warn
#' @export
fabR_help <- function(...) {

  deprecate_warn(
    "2.0.2", "fabR_help()", "fabR_website()")

  # Unquote-splice to avoid argument matching

  fabR_website(...)

}
