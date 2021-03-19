#' @title Turn a column into an ordered factor
#' @description Converts a column into an ordered factor.
#' @param data a data frame or tibble
#' @param column the column to convert to an ordered factor
#' @param by the column used to order `column`
#' @param ... expressions passed to `dplyr::filter`. Only rows satisfying these expressions are used to order `column`,
#' @return The dataframe with `column` converted to an ordered factor that is ordered according to `by`
#' @details `column`, `by`, and `...` use tidyselect (e.g. column names do not need to be quoted)
#' @examples 
#' library(tidyr)
#' mtcars$Model <- rownames(mtcars)
#' ordfactby(mtcars, Model, gear)  #turns column Model into an ordered factor ordered by gear
#' @export
ordfactby <- function(data, column, by, ...){
  specorder <- data %>%
    dplyr::ungroup() %>% #don't want grouping in the ordered factor
    dplyr::filter(...) %>%
    dplyr::select({{ column }}, {{ by }}) %>% 
    dplyr::distinct({{ column }}, {{ by }}) %>% #remove duplicates in column only when by is also duplicated
    dplyr::arrange({{ by }}) %>% 
    dplyr::select({{ column }}) %>%
    unlist()
  data <- dplyr::mutate(data, {{column}} := factor({{ column }}, levels = specorder, ordered = TRUE))
  return(data)
}
