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
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
plot_main_word <- function(...) {

  deprecate_warn(
    "2.0.0", "plot_main_word(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
plot_histogram <- function(...) {

  deprecate_warn(
    "2.0.0", "plot_histogram(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
plot_box <- function(...) {

  deprecate_warn(
    "2.0.0", "plot_box(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
plot_bar <- function(...) {

  deprecate_warn(
    "2.0.0", "plot_bar(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
plot_date <- function(...) {

  deprecate_warn(
    "2.0.0", "plot_date(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
plot_density <- function(...) {

  deprecate_warn(
    "2.0.0", "plot_density(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
plot_pie <- function(...) {

  deprecate_warn(
    "2.0.0", "plot_pie(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
plot_pie_valid_value <- function(...) {

  deprecate_warn(
    "2.0.0", "plot_pie_valid_value(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
summary_text <- function(...) {

  deprecate_warn(
    "2.0.0", "summary_text(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
summary_numerical <- function(...) {

  deprecate_warn(
    "2.0.0", "summary_numerical(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' For any fabR::plot_xxx() or summary_xxx() use
#' [madshapR::variable_visualize()] instead.
#'
#' @name deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @export
summary_category <- function(...) {

  deprecate_warn(
    "2.0.0",
    "summary_category(tbl)",
    "madshapR::variable_visualize(dataset)")

  return(NULL)

}
