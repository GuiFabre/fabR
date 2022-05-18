#' Draw bar plot of one (possibly grouped) open-text column in a tbl
#'
#' This function draws a bar plot of the values of open text column. This
#' plot shows the x-th first most cited words in a column having open text content using
#' tidytext library.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param max integer specifying the x-th first most cited words
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return bar plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_main_word()
#'
#' # Example 2: words contains in Species
#' plot_main_word(tbl = tbl, col = "Species", out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_main_word        <- function(tbl = "iris", col = "Species", filter = 'c()', negate = FALSE, missing_values = 'c()', max = 10, out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  plot <- paste0(
    tbl_name," %>% "                                                              ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                             ,"\n",
    "  filter(!(",col," %in% ",missing_values," | is.na(",col,"))) %>% "              ,"\n",
    "  group_by(",group_by,") %>%"                                                    ,"\n",
    "  mutate(",col," = as.character(",col,")) %>%"                                   ,"\n",
    "  unnest_tokens(output = word, input = ",col,") %>%"                             ,"\n",
    "  anti_join(tidytext::stop_words) %>%"                                           ,"\n",
    "  count(word, sort = TRUE) %>%"                                                  ,"\n",
    "  mutate(word = reorder(word, n)) %>%"                                           ,"\n",
    "  slice(1:min(",max,",nrow(.))) "                                                     )

  if(stringr::str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                                         ,"\n",
      "  ggplot(aes(word, n)) +"                                                           ,"\n",
      "  geom_col() +"                                                                     ,"\n",
      "  coord_flip() +"                                                                   ,"\n",
      "  labs(x = 'Word', y = ' Count', title = 'Frequent words in ",col,"') +"            ,"\n",
      "  geom_text(aes(label = n), hjust = 1.2, colour = 'white') +"                       ,"\n",
      "  theme(plot.title = element_text(hjust = 0.5),"                                    ,"\n",
      "       axis.title.x = element_text(face='bold', colour='darkblue', size = 12),"     ,"\n",
      "       axis.title.y = element_text(face='bold', colour='darkblue', size = 12))",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(stringr::str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                                                 ,"\n",
      "  ggplot(aes(word, n)) +"                                                        ,"\n",
      "  geom_col() +"                                                                  ,"\n",
      "  coord_flip() +"                                                                ,"\n",
      "  labs(x = 'Word', y = ' Count', title = 'Frequent words in ",col,"') +"         ,"\n",
      "  geom_text(aes(label = n), hjust = 1.2, colour = 'white') +"                    ,"\n",
      "  theme(plot.title = element_text(hjust = 0.5),"                                 ,"\n",
      "       axis.title.x = element_text(face='bold', colour='darkblue', size = 12),"  ,"\n",
      "       axis.title.y = element_text(face='bold', colour='darkblue', size = 12))",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," ) ")
  }

  if(stringr::str_detect(out,"code"))                  { return(plot)
  }else if(stringr::str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw histogram of one (possibly grouped) column in a tbl
#'
#' This function draws a histogram plot of the values of a column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return hist plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_histogram()
#'
#' # Example 2: graph of Petal.Length
#' plot_histogram(tbl = tbl, col = "Petal.Length", out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_histogram        <- function(tbl = "airquality", col = "Ozone", filter = 'c()', negate = FALSE, missing_values = 'c()',out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  plot <- paste0(
    tbl_name," %>% "                                                       ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                             ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  ))  "                                     )

  if(stringr::str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                                               ,"\n",
      "  ggplot(aes(x = ", col, ifelse(!is.null(group_by), paste0(", fill = ", group_by),""),")) +"  ,"\n",
      "  geom_histogram(color = '#e9ecef',alpha = 0.9, stat = 'count') +"                        ,"\n",
      "  ggtitle('distribution of ",col,"') +"                         ,"\n",
      "  theme(plot.title = element_text(size = 15))",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(stringr::str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot, " %>% "                                                         ,"\n",
      "  ggplot(aes(x = ", col, ifelse(!is.null(group_by),paste0(", fill = ", group_by),""),")) +"  ,"\n",
      "  geom_histogram(color = '#e9ecef',alpha = 0.9, stat = 'count') +"                        ,"\n",
      "  ggtitle('distribution of ",col,"') +"                         ,"\n",
      "  theme(plot.title = element_text(size = 15))",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," ) "                   )
  }

  if(stringr::str_detect(out,"code"))                  { return(plot)
  }else if(stringr::str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw box plot of one (possibly grouped) column in a tbl
#'
#' This function draws a box plot of the values of a column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return box plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_box()
#'
#' # Example 2: graph of Petal.Length
#' plot_box(tbl = tbl, col = "Petal.Length", out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_box              <- function(tbl = "airquality", col = "Month", filter = 'c()', negate = FALSE, missing_values = 'c()',               out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  plot <- paste0(
    tbl_name," %>% "                                                       ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                         ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  )) %>% "                         ,"\n",
    "  tibble::add_column(participants = 'participants')"                                      )

  if(stringr::str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                    ,"\n",
      "  ggplot(aes(x = ",group_by,", y = ",col,", fill = ",group_by,")) +"         ,"\n",
      "  geom_boxplot() + "                                                         ,"\n",
      "  coord_flip() +"                                                            ,"\n",
      "  theme(legend.position = 'none',plot.title = element_text(size=11)) +"      ,"\n",
      "  ggtitle('Box plot representation of ",col,"') +"                           ,"\n",
      "  ylab('') +"                                                                ,"\n",
      "  xlab('')"                                                                       )
  }

  if(stringr::str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                                                     ,"\n",
      "  ggplot(aes(x = ",group_by,", y = ",col,", fill = ",group_by,")) +"         ,"\n",
      "  geom_boxplot() + "                                                         ,"\n",
      "  coord_flip() +"                                                            ,"\n",
      "  theme(legend.position = 'none',plot.title = element_text(size=11)) +"      ,"\n",
      "  ggtitle('Box plot representation of ",col,"') +"                           ,"\n",
      "  ylab('') +"                                                                ,"\n",
      "  xlab('') )"                                                                     )
  }

  if(stringr::str_detect(out,"code"))                  { return(plot)
  }else if(stringr::str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw lollipop plot of one (possibly grouped) time-related column in a tbl
#'
#' This function draws a lollipop plot of the values of time related column.
#' the 'time' parameter uses lubridate synthax to specify the period of time to consider.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param time parameter following lubridate synthaxe to specify the period of time
#' to consider. Can be ymd, mdy, year, months, etc. See lubridate documentation.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return lollipop plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_date()
#'
#' # Example 2: graph of
#'
#' tbl = read_csv_any_formats("study_TOKYO.csv")
#' plot_date(tbl, col = "dob", out = "ggplot2",time = "year")
#' plot_date(tbl, col = "dob", out = "ggplot2",time = "month")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_date             <- function(tbl = "airquality", col = "Day", filter = 'c()', negate = FALSE, missing_values = 'c()', time = "day", out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  plot <- paste0(
    tbl_name," %>% "                                                      ,"\n",
    "  mutate(",col," = ",time,"(as.Date(",col,"))) %>%"                ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                     ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  )) %>% "                     ,"\n",
    "  group_by(",col,",",group_by,") %>%"                                    ,"\n",
    "  tally "                                                                     )

  if(stringr::str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                              ,"\n",
      "  ggplot(aes(x = ",col,", y = n", ifelse(!is.null(group_by),paste0(", color = ",group_by),""),")) +" ,"\n",
      "  geom_segment("                                                         ,"\n",
      "  aes(x = ",col,", xend = ",col,", y = 0, yend = n), color = 'grey') +"  ,"\n",
      "  geom_point(size=4) +"                                                  ,"\n",
      "  theme("                                                                ,"\n",
      "    panel.grid.major.x = element_blank(),"                               ,"\n",
      "    panel.border = element_blank(),"                                     ,"\n",
      "    axis.ticks.x = element_blank()) +"                                   ,"\n",
      "  xlab('distribution of dates - in ",time,"') +"                         ,"\n",
      "  ylab('Number of participants') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(stringr::str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                                            ,"\n",
      "  ggplot(aes(x = ",col,", y = n", ifelse(!is.null(group_by),paste0(", color = ",group_by),""),")) +" ,"\n",
      "  geom_segment("                                                            ,"\n",
      "  aes(x = ",col,", xend = ",col,", y = 0, yend = n), color = 'grey') +"     ,"\n",
      "  geom_point(size=4) +"                                                     ,"\n",
      "  theme("                                                                   ,"\n",
      "    panel.grid.major.x = element_blank(),"                                  ,"\n",
      "    panel.border = element_blank(),"                                        ,"\n",
      "    axis.ticks.x = element_blank()) +"                                      ,"\n",
      "  xlab('distribution of dates - in ",time,"') +"                            ,"\n",
      "  ylab('Number of participants') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," )"      )
  }

  if(stringr::str_detect(out,"code"))                  { return(plot)
  }else if(stringr::str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw bar plot of one (possibly grouped) column in a tbl
#'
#' This function draws a bar plot of the values of a column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return bar plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_bar()
#'
#' # Example 2: graph of Species
#' plot_bar(tbl = tbl, col = "Species", out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_bar              <- function(tbl = "iris", col = "Species", filter = 'c()', negate = FALSE, missing_values = 'c()',               out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  plot <- paste0(
    tbl_name," %>% "                                                       ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                      ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  )) %>% "                      ,"\n",
    "  mutate(",col," = ",col," %>% as.character ) "                                )

  if(stringr::str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                             ,"\n",
      "  ggplot(aes(x = ",col,", fill =  ",col," )) + "                        ,"\n",
      "  geom_bar() + "                                                        ,"\n",
      "  viridis::scale_fill_viridis(discrete = TRUE) + "                               ,"\n",
      "  theme(legend.position = 'right') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(stringr::str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                        ,"\n",
      "  ggplot(aes(x = ",col,", fill =  ",col," )) + "        ,"\n",
      "  geom_bar() + "                                        ,"\n",
      "  viridis::scale_fill_viridis(discrete = TRUE) + "               ,"\n",
      "  theme(legend.position = 'right')",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," ) ")
  }

  if(stringr::str_detect(out,"code"))                  { return(plot)
  }else if(stringr::str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw density plot of one (possibly grouped) column in a tbl
#'
#' This function draws a density line plot of the values of a column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return density plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_density()
#'
#' # Example 2: graph of Petal.Length
#' plot_density(tbl = tbl, col = "Petal.Length", out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_density          <- function(tbl = "iris", col = "Sepal.Length", filter = 'c()', negate = FALSE, missing_values = 'c()', out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  plot <- paste0(
    tbl_name," %>% "                                                                                        ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                                                       ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  )) "                                                            )

  if(stringr::str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                                                                ,"\n",
      "  ggplot(aes(x = ", col, ifelse(!is.null(group_by), paste0(", fill = ", group_by),""),")) +"                   ,"\n",
      "  geom_density( color = '#e9ecef'", ifelse(!is.null(group_by),"",", fill = '#69b3a2'"),", alpha = 0.8) + " ,"\n",
      "  theme(legend.position = 'right') " ,
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(stringr::str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                                               ,"\n",
      "  ggplot(aes(x = ", col, ifelse(!is.null(group_by), paste0(", fill = ", group_by),""),")) +"  ,"\n",
      "  geom_density( color = '#e9ecef'", ifelse(!is.null(group_by),"",", fill = '#69b3a2'"),", alpha = 0.8) + " ,"\n",
      "  theme(legend.position = 'right')",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," ) ")

  }

  if(stringr::str_detect(out,"code"))                  { return(plot)
  }else if(stringr::str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw pie chart of one (possibly grouped) column in a tbl
#'
#' This function draws a pie plot of the values of column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return pie plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_pie()
#'
#' # Example 2: graph of Species
#' plot_pie(tbl = tbl, col = "Species", out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_pie              <- function(tbl = "iris", col = "Species", filter = 'c()', negate = FALSE, missing_values = 'c()',               out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  plot <- paste0(
    tbl_name," %>% "                                                   ,"\n",
    "  filter(", negate, "(",col," %in% ", filter, ")) %>% "                  ,"\n",
    "  filter(!(" , col, " %in% ", missing_values,"  )) %>% "                  ,"\n",
    "  mutate(", col, " = ", col, " %>% as.character ) %>%  "                  ,"\n",
    "  mutate(group_by = ",group_by,") %>% "                               ,"\n",
    "  group_by(",col,",group_by) %>% "                                    ,"\n",
    "  tally "                                                                  )

  if(stringr::str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                           ,"\n",
      "  ggplot(aes(x = '', y = n, fill = ",col,")) +"                       ,"\n",
      "  geom_bar(stat='identity', width = 1, position = position_fill()) + "  ,"\n",
      "  coord_polar('y', start=0) + "                                       ,"\n",
      "  theme_void() + "                                                    ,"\n",
      "  viridis::scale_fill_viridis(discrete = TRUE) + "                             ,"\n",
      "  theme(legend.position = 'right') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~group_by)"),""))

  }

  if(stringr::str_detect(out,"plotly")){

    count_category = c(0:(eval(parse(text = stringr::str_squish(plot))) %>% pull(group_by) %>% unique %>% length))

    plot <- paste0(
      "plotly::plot_ly() %>%"                                                    ,"\n",
      paste(
        paste0("plotly::add_pie(data = ",plot," %>% filter(group_by == \nunique(",plot," %>% pull(group_by)) %>% .[",count_category + 1,"])",",
                             labels = ~",col,",
                             text = ~group_by,
                             values = ~n,
                             domain = list(row = 0, column = ",count_category,")) %>%"),
        collapse = "\n")  ,"\n",
      "       plotly::layout(title = 'Pie Plots with Subplots', showlegend = TRUE,
             grid=list(rows=1, columns=",max(count_category)+1,"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) ")
  }

  if(stringr::str_detect(out,"code"))                  { return(plot)
  }else if(stringr::str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw pie chart of one (possibly grouped) column in a tbl (valid, non-valid and missing values)
#'
#' This function draws a pie plot of the values of a column separating valid,
#' non-valid and missing values.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return pie plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#'
#' # Example 2: graph of Species (virginica is associated to missing values for the
#' # purpose of example)
#' plot_pie_valid_value(
#'   tbl = tbl,
#'   col = "Species",
#'   missing_values = "'virginica'" ,
#'   out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_pie_valid_value  <- function(tbl = "iris", col = "Species", filter = 'c()', negate = FALSE, missing_values = "'versicolor'", out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")
  #
  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  plot <- paste0(
    tbl_name," %>% "                                                     ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                    ,"\n",
    "  mutate(",col," = ",col," %>% as.character )  %>% "                    ,"\n",
    "  mutate( "                                                             ,"\n",
    "    ",col," = case_when("                                               ,"\n",
    "      is.na(",col,")                     ~ 'missing value',"            ,"\n",
    "      !(",col," %in% ",missing_values,") ~ 'valid value', "             ,"\n",
    "      TRUE                               ~ 'not valid value')) %>% "    ,"\n",
    "  mutate(group_by = ",group_by,")  %>% "                                ,"\n",
    "  group_by(",col,",group_by)               %>% "                        ,"\n",
    "  tally "                                                                )

  if(stringr::str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                           ,"\n",
      "  ggplot(aes(x = '', y = n, fill = ",col,")) +"                       ,"\n",
      "  geom_bar(stat='identity', width = 1, position = position_fill()) + "  ,"\n",
      "  coord_polar('y', start=0) + "                                       ,"\n",
      "  theme_void() + "                                                    ,"\n",
      "  viridis::scale_fill_viridis(discrete = TRUE) + "                             ,"\n",
      "  theme(legend.position = 'right') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~group_by)"),""))

  }


  if(stringr::str_detect(out,"plotly")){

    count_category = c(0:(eval(parse(text = stringr::str_squish(plot))) %>% pull(group_by) %>% unique %>% length))

    plot <- paste0(
      "plotly::plot_ly() %>%","\n",
      paste(
        paste0("plotly::add_pie(data = ",plot," %>% filter(group_by == \nunique(",plot," %>% pull(group_by)) %>% .[",count_category + 1,"])",",
                             labels = ~",col,",
                             text = ~group_by,
                             values = ~n,
                             domain = list(row = 0, column = ",count_category,")) %>%"),
        collapse = "\n")  ,"\n",
      "       plotly::layout(title = 'Pie Plots with Subplots', showlegend = TRUE,
             grid=list(rows=1, columns=",max(count_category)+1,"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) ")
  }

  if(stringr::str_detect(out,"code"))                  { return(plot)
  }else if(stringr::str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Create summary table of one (possibly grouped) text-type column in a tbl
#'
#' This function creates a datatable of the values of a column with separate valid,
#' non-valid and missing values.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#' The user can download the content of the datatable in csv format.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either 'DT', 'DT-code' and 'DT-cat'.
#' DT renders a datatable using DT library, code gives the code in a string (usable
#' directly with eval/parse functions) and cat provides indented code in the console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return A datatable (editable) object or a R script in a character string to create it.
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' summary_text()
#'
#' # Example 2: summary table of Species
#' summary_text(tbl = iris, col = "Species", out = "DT")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
summary_text          <- function(tbl = "iris", col = "col", filter = 'c()', negate = FALSE, missing_values = 'c()', out = "DT-cat", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  summary <- paste0(
    tbl_name," %>% "                                                   ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                  ,"\n",
    "  filter(!(", col," %in% ", missing_values,")) %>% "                    ,"\n",
    "  mutate(", col," = as.character(",col, ")) %>% "                       ,"\n",
    "  group_by(", col,",", group_by,") %>% count %>% "                      ,"\n",
    "  select(2, 3, 1) %>% mutate(", col, " = replace_na(", col, ", '-')) %>% "   ,"\n",
    "  rename(`Content` = 1, `Number of answers` = 2) %>% "                ,"\n",
    "  arrange(`Content`,desc(`Number of answers`)) %>% "                  ,"\n",
    "  mutate(`Content` = na_if(`Content`,'')) %>%"                        ,"\n",
    "    remove_empty(which = 'cols') %>%"                                 ,"\n",
    "  DT::datatable( "                                                        ,"\n",
    "    class = 'cell-border stripe', rownames = FALSE,"                  ,"\n",
    "    filter = 'top', editable = FALSE, extensions = 'Buttons', "       ,"\n",
    "    options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = c('csv') ))"                  )

  if(stringr::str_detect(out,"code"))     { return(summary)
  }else if(stringr::str_detect(out,"cat")){ return(summary %>% cat)
  }else if(out == "DT")          { return(summary %>% parceval)
  }else                          { return(message("Valide 'out' attributes are 'DT', 'DT-code','DT-cat'"))}

}


#' Create summary table of one (possibly grouped) numerical-type column in a tbl
#'
#' This function creates datatable of the values of a column separating valid,
#' non-valid and missing values.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#' The user can download the content of the datatable in csv format.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#'
#' @param tbl A character string or tibble
#' @param col A character string of a column of interest
#' @param filter A character string to subset the rows, applying the expressions in ...
#' to the column values to determine which rows should be retained. It can be applied
#' to both grouped and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. Those
#' values will not be exclud from counting, but will be separated from valid values.
#' @param out parameter that specifies the output expected: can be either 'DT', 'DT-code' and 'DT-cat'.
#' DT renders a datatable using DT library, code gives the code in a string (usable
#' directly with eval/parse functions) and cat provides indented code in the console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return A datatable (editable) object or a R script in a character string to create it.
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' summary_numerical()
#'
#' # Example 2: summary table of Petal.Length
#' summary_numerical(tbl = iris, col = "Petal.Length", out = "DT")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
summary_numerical     <- function(tbl = "iris", col = "col", filter = 'c()', negate = FALSE, missing_values = 'c()', out = "DT-cat", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  summary <- paste0(
    tbl_name," %>% "                                                   ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                  ,"\n",
    "  group_by(",group_by,") %>% "                                        ,"\n",
    "  select(",col,") %>% "                                               ,"\n",
    "  rename(group = 1) %>% "                                             ,"\n",
    "  mutate(group = ifelse(group == '', ' ', group)) %>% "               ,"\n",
    "  summarise( "                                                        ,"\n",
    "    nbr.val = sum(!is.na(",col,")), "                                 ,"\n",
    "    nbr.na  = sum(is.na(",col,")), "                                  ,"\n",
    "    min = min(",col,", na.rm = TRUE), "                               ,"\n",
    "    max = max(",col,", na.rm = TRUE), "                               ,"\n",
    "    range = max - min, "                                              ,"\n",
    "    median = median(",col,", na.rm = TRUE), "                         ,"\n",
    "    mean = mean(",col,", na.rm = TRUE), "                             ,"\n",
    "    std.dev = sd(",col,", na.rm = TRUE)) %>% "                        ,"\n",
    "  tidyr::pivot_longer(!group) %>% "                                   ,"\n",
    "  tidyr::pivot_wider(names_from = group, "                            ,"\n",
    "  names_glue = '{group}<br>(all answers)') %>%"                       ,"\n",
    " full_join(     "                                                     ,"\n",
    tbl_name," %>% "                                                   ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                  ,"\n",
    "  filter(!(",col," %in% ",missing_values,"| is.na(",col,"))) %>% "    ,"\n",
    "  group_by(",group_by,") %>% "                                        ,"\n",
    "  select(",col,") %>% "                                               ,"\n",
    "  rename(group = 1) %>% "                                             ,"\n",
    "  mutate(group = ifelse(group == '', ' ', group)) %>% "               ,"\n",
    "  summarise( "                                                        ,"\n",
    "    nbr.val = sum(!is.na(",col,")), "                                 ,"\n",
    "    nbr.na  = sum(is.na(",col,")), "                                  ,"\n",
    "    min = min(",col,", na.rm = TRUE), "                               ,"\n",
    "    max = max(",col,", na.rm = TRUE), "                               ,"\n",
    "    range = max - min, "                                              ,"\n",
    "    median = median(",col,", na.rm = TRUE), "                         ,"\n",
    "    mean = mean(",col,", na.rm = TRUE), "                             ,"\n",
    "    std.dev = sd(",col,", na.rm = TRUE)) %>% "                        ,"\n",
    "  tidyr::pivot_longer(!group) %>% "                                   ,"\n",
    "  tidyr::pivot_wider(names_from = group, "                            ,"\n",
    "  names_glue = '{group}<br>(only valid answers)')) %>%"               ,"\n",
    "  select(' ' = name, order(colnames(.))) %>% "                         ,"\n",
    "  mutate_at(.vars = -1, ~ round(., 2)) %>% "                             ,"\n",
    "  DT::datatable( "                                                        ,"\n",
    "    class = 'cell-border stripe', rownames = TRUE,"                   ,"\n",
    "    editable = FALSE, extensions = 'Buttons',"                        ,"\n",
    "    options = list(scrollX = TRUE, dom = 'Brtip', buttons = c('csv')), escape = FALSE )"   )

  if(stringr::str_detect(out,"code"))     { return(summary)
  }else if(stringr::str_detect(out,"cat")){ return(summary %>% cat)
  }else if(out == "DT")          { return(summary %>% parceval)
  }else                          { return(message("Valide 'out' attributes are 'DT', 'DT-code','DT-cat'"))}

}

#' Create summary table of one (possibly grouped) category-type column in a tbl
#'
#' This function creates datatable of the values of a column separating valid,
#' non-valid and missing values.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#' The user can download the content of the datatable in csv format.
#'
#' @param tbl A character string or tibble specifying the input tbl
#' @param col A character string of a column of interest
#' @param filter A character string to subset the rows, applying the expressions in ...
#' to the column values to determine which rows should be retained. It can be applied
#' to both grouped and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. Those
#' values will not be exclud from counting, but will be separated from valid values.
#' @param out parameter that specifies the output expected: can be either 'DT', 'DT-code' and 'DT-cat'.
#' DT renders a datatable using DT library, code gives the code in a string (usable
#' directly with eval/parse functions) and cat provides indented code in the console.
#' @param group_by A character string of one column in the tbl that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return A datatable (editable) object or a R script in a character string to create it.
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' summary_category()
#'
#' # Example 2: summary table of Petal.Length
#' summary_category(tbl = iris, col = "Species", out = "DT")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
summary_category      <- function(tbl = "iris", col = "col", filter = 'c()', negate = FALSE, missing_values = 'c()', out = "DT-cat", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  tbl_name <- if(class(tbl)[1] == "character") {tbl}else{as.character(substitute(tbl)) }
  tbl      <- if(class(tbl)[1] == "character") { parceval(tbl) }else{ tbl}

  summary <- paste0(
    tbl_name," %>% "                                                        ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                       ,"\n",
    "  group_by(",col,", group_by = ",group_by,") %>% count %>% "               ,"\n",
    "  tibble::add_column(prop_no_mis = NA_real_) %>% "                                 ,"\n",
    "  group_by(group_by) %>% "                                                 ,"\n",
    "  select(group_by, everything()) %>% "                                     ,"\n",
    "  mutate(prop_no_mis = paste0(round(n/sum(n), digits = 2)*100,'%')) %>%"   ,"\n",
    "  full_join( "                                                             ,"\n",
    tbl_name," %>% "                                                        ,"\n",
    "    filter(",negate,"(",col," %in% ",filter, ")) %>% "                     ,"\n",
    "    filter(!(",col," %in% ",missing_values," | is.na(",col,"))) %>% "      ,"\n",
    "      group_by(",col,", group_by = ",group_by,") %>% count %>% "           ,"\n",
    "      tibble::add_column(prop_tot = NA_real_) %>% "                                ,"\n",
    "      group_by(group_by) %>% "                                             ,"\n",
    "      select(group_by, everything()) %>% "                                 ,"\n",
    "      mutate(prop_tot = paste0(round(n/sum(n), digits = 2)*100,'%')))%>% " ,"\n",
    "  mutate(prop_tot = replace_na(prop_tot,'-')) %>% "                        ,"\n",
    "  rename(`Grouping variable` = 1, "                                        ,"\n",
    "          `Category code` = 2, "                                           ,"\n",
    "          `Number of answers` = 3 ,"                                       ,"\n",
    "          `Proportion - all` = 4 , "                                       ,"\n",
    "          `Proportion - valid values` = 5) %>% "                           ,"\n",
    "  select(1, 2, 3, 4, 5) %>% "                                                  ,"\n",
    "  mutate(`Grouping variable` = na_if(`Grouping variable`,'')) %>% "        ,"\n",
    "  arrange(`Grouping variable`,`Category code`) %>% "                       ,"\n",
    "    remove_empty('cols') %>% "                                             ,"\n",
    "  DT::datatable( "                                                             ,"\n",
    "    class = 'cell-border stripe', rownames = FALSE,"                       ,"\n",
    "    filter = 'top', editable = FALSE, extensions = 'Buttons', "            ,"\n",
    "    options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = c('csv') ))"       )

  if(stringr::str_detect(out,"code"))     { return(summary)
  }else if(stringr::str_detect(out,"cat")){ return(summary %>% cat)
  }else if(out == "DT")          { return(summary %>% parceval)
  }else                          { return(message("Valide 'out' attributes are 'DT', 'DT-code','DT-cat'"))}

}


#' Create a bookdown template for the visual report
#'
#' This helper function creates a template for the visual report bookdown. This
#' template is taken from the following link:
#' https://github.com/jtr13/bookdown-template/archive/refs/heads/master.zip
#' folder
#'
#' @param to A character string of a path where the bookdown report will be placed
#'
#' @return a folder containing all files (Rmd, yml, docs, ...) to generate bookdown report
#'
#' @examples
#' \dontrun{
#' # Example 1: create a folder containing template
#'
#' template_visual_report("template")
#'
#' }
#'
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
template_visual_report    <- function(to = getwd()){

  try({unlink(paste0(to,"/temp_bookdown_report/"), recursive = TRUE)},silent = TRUE)
  fs::dir_create(paste0(to,"/temp_bookdown_report"))

  utils::download.file("https://github.com/jtr13/bookdown-template/archive/refs/heads/master.zip", paste0(to,"/temp_bookdown_report/file.zip"))
  utils::unzip(paste0(to,"/temp_bookdown_report/file.zip"),exdir = paste0(to,"/temp_bookdown_report/file"))
  file.remove(paste0(to,"/temp_bookdown_report/file.zip"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-master/02-tears.Rmd"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-master/03-race.Rmd"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-master/README.md"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-master/docs/index.html"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-master/docs/the-pool-of-tears.html"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-master/docs/a-caucus-race-and-a-long-tale.html"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-master/docs/search_index.json"))

  paste0(
    'book_filename: "bookdownproj"
output_dir: docs
delete_merged_file: true
edit: https://github.com/YOUR GITHUB USERNAME/YOUR REPO NAME/edit/master/%s
view: https://github.com/YOUR GITHUB USERNAME/YOUR REPO NAME/blob/master/%s
language:
  ui:
    chapter_name: ""

') %>% readr::write_lines(file = paste0(to,"/temp_bookdown_report/file/bookdown-template-master/_bookdown.yml"), append = FALSE)


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

') %>% readr::write_lines(file = paste0(to,"/temp_bookdown_report/file/bookdown-template-master/style.css"), append = FALSE)

  paste0(
    '---
title: "XXX"
author: "xxx"
date: "`r Sys.Date()`"
site: bookdown::bookdown_site
---

') %>% readr::write_lines(file = paste0(to,"/temp_bookdown_report/file/bookdown-template-master/index.Rmd"), append = FALSE)

}
