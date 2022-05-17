#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param ... xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_args           <- function(...){

  # get arguments of function and inputs
  args <- as.list(args(get_args))

  # add to pipeline
  invisible(args)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param ... xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
cast_error_fargs           <- function(...){

  # get arguments of function and inputs
  args <- as.list(args(get_args))

  # add to pipeline
  invisible(args)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param attributes xxx xxx xxx
#' @param version xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
set_template           <- function(attributes, version = NULL){

  # get arguments of function and inputs
  args <- as.list(args(set_template))
  appen_fargs <- as.list(match.call(expand.dots = TRUE))

  # trasform to tibble
  fargs <- get_args(args, appen_fargs) %>%
    mutate(order = 0)

  # test validity of user input
  cast_error_fargs(fargs)

  # add to pipeline
  invisible(fargs)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param .fargs xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
add_template           <- function(.fargs){

  # get arguments of function and inputs
  args <- as.list(args(add_template))
  fargs <- .fargs
  appen_fargs <- as.list(match.call(expand.dots = TRUE))
  appen_fargs$.fargs = NULL
  args$.fargs = NULL

  # trasform to tibble
  args <- get_args(args, appen_fargs)
  max_order = fargs %>% pull(order) %>% max
  fargs <- fargs %>% bind_rows(args) %>%
    mutate(order = tidyr::replace_na(order, max_order + 1))

  # test validity of user input
  cast_error_fargs(fargs)

  # add to pipeline
  invisible(fargs)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param .fargs xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
prep_template          <- function(.fargs){

  # get arguments of function and inputs
  args <- as.list(args(prep_template))
  fargs <- .fargs
  appen_fargs <- as.list(match.call(expand.dots = TRUE))
  appen_fargs$.fargs = NULL
  args$.fargs = NULL

  # trasform to tibble
  args <- get_args(args, appen_fargs)
  max_order = fargs %>% pull(order) %>% max
  fargs <- fargs %>% bind_rows(args) %>%
    mutate(order = tidyr::replace_na(order, max_order + 1))

  # test validity of user input
  cast_error_fargs(fargs)

  # add to pipeline
  invisible(fargs)

}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param attributes xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
set_template_run       <- function(attributes){

  obj <- list()
  obj$attributes <- attributes

  return(obj)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param obj xxx xxx xxx
#' @param append_obj xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
add_template_run       <- function(obj, append_obj){

  # get arguments of function and inputs
  args <- as.list(args(add_template_run))
  appen_fargs <- as.list(match.call(expand.dots = TRUE))

  # trasform to tibble
  fargs <- get_args(args, appen_fargs)

  append_obj_name <- fargs %>%
    filter(.data$key == "append_obj") %>% pull(.data$value)
  append_obj <- get(x = append_obj_name, envir = parent.frame())

  if(is.null(obj$append_obj)){
    obj$append_obj <- list()}

  obj$append_obj[[append_obj_name]] <- append_obj

  return(obj)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param obj xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
prep_template_run      <- function(obj){

  message("Manipulate the way you want the want, and return obj")

  return(obj)

}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param .fargs xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
show_pipeline          <- function(.fargs){

  .fargs %>%
    group_by(.data$order,.data$function_name) %>% slice(1) %>%
   tidyr::unite(.data$order,.data$function_name, col = "process", sep = " - ") %>%
    pull(.data$process) %>% paste0("\n") %>% message
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param .fargs xxx xxx xxx
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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
show_template           <- function(.fargs){

  show_pipeline(.fargs)

  pipeline <- .fargs %>%
    tidyr::unite(.data$key, .data$value, col = "args", sep = " = ") %>%
    group_by(.data$order,.data$function_name) %>%
    summarise(across(c(args), ~ paste0(.,collapse = ", ")), .groups = "drop") %>%
    mutate(
      function_name = paste0(.data$function_name, "_run(", args,")"),
      function_name = stringr::str_remove(.data$function_name, "(.fargs = NULL, |.fargs = NULL)")) %>%
    summarise(across(c(.data$function_name), ~ paste0(.,collapse = " %>% \n")), .groups = "drop") %>%
    pull(.data$function_name)

  parceval(pipeline)

}



