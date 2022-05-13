#### TEMPLATE FONCTIONS #########################################################

set_template           <- function(attributes, version = NULL){

  # get arguments of function and inputs
  args <- as.list(args(set_template))
  appen_fargs <- as.list(match.call(expand.dots = TRUE))

  # trasform to tibble
  fargs <- get_args(args,appen_fargs) %>%
    mutate(order = 0)

  # test validity of user input
  cast_error_fargs(fargs)

  # add to pipeline
  invisible(fargs)
}
add_template           <- function(.fargs){

  # get arguments of function and inputs
  args <- as.list(args(add_template))
  fargs <- .fargs
  appen_fargs <- as.list(match.call(expand.dots = TRUE))
  appen_fargs$.fargs = NULL
  args$.fargs = NULL

  # trasform to tibble
  args <- get_args(args,appen_fargs)
  max_order = fargs %>% pull(order) %>% max
  fargs <- fargs %>% bind_rows(args) %>%
    mutate(order = replace_na(order,max_order + 1))

  # test validity of user input
  cast_error_fargs(fargs)

  # add to pipeline
  invisible(fargs)
}
prep_template          <- function(.fargs){

  # get arguments of function and inputs
  args <- as.list(args(prep_template))
  fargs <- .fargs
  appen_fargs <- as.list(match.call(expand.dots = TRUE))
  appen_fargs$.fargs = NULL
  args$.fargs = NULL

  # trasform to tibble
  args <- get_args(args,appen_fargs)
  max_order = fargs %>% pull(order) %>% max
  fargs <- fargs %>% bind_rows(args) %>%
    mutate(order = replace_na(order,max_order + 1))

  # test validity of user input
  cast_error_fargs(fargs)

  # add to pipeline
  invisible(fargs)

}

set_template_run       <- function(attributes){

  obj <- list()
  obj$attributes <- attributes

  return(obj)
}
add_template_run       <- function(obj, append_obj){

  # get arguments of function and inputs
  args <- as.list(args(add_template_run))
  appen_fargs <- as.list(match.call(expand.dots = TRUE))

  # trasform to tibble
  fargs <- get_args(args,appen_fargs)

  append_obj_name <- fargs %>% filter(key == "append_obj") %>% .$value
  append_obj <- get(x = append_obj_name, envir = parent.frame())

  if(is.null(obj$append_obj)){
    obj$append_obj <- list()}

  obj$append_obj[[append_obj_name]] <- append_obj

  return(obj)
}
prep_template_run      <- function(obj){

  message("Manipulate the way you want the want, and return obj")

  return(obj)

}

show_template          <- function(.fargs){

  .fargs %>%
    group_by(order,function_name) %>% slice(1) %>%
    unite(order,function_name, col = process, sep = " - ") %>%
    pull(process) %>% paste0("\n") %>% message
}
run_template           <- function(.fargs){

  show_pipeline(.fargs)

  pipeline <- .fargs %>%
    unite(key, value, col = args, sep = " = ") %>%
    group_by(order,function_name) %>%
    summarise(across(c(args), ~ paste0(.,collapse = ", ")), .groups = "drop") %>%
    mutate(
      function_name = paste0(function_name, "_run(",args,")"),
      function_name = str_remove(function_name, "(.fargs = NULL, |.fargs = NULL)")) %>%
    summarise(across(c(function_name), ~ paste0(.,collapse = " %>% \n")), .groups = "drop") %>%
    pull(function_name)

  parceval(pipeline)

}
