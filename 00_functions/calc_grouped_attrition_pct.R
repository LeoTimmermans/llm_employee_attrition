#' Calculate Grouped Attrition Percentage
#'
#' This function calculates the percentage of attrition for a specified target variable
#' grouped by a specified grouping variable. It counts the occurrences of each combination
#' of the target and grouping variables, then computes the percentage of those counts
#' that correspond to the "Yes" category in the target variable.
#'
#' @param data A data frame containing the data you want to analyze.
#' @param target_var A symbol or a character string representing the target variable
#' (e.g., Attrition) whose values are to be evaluated. This variable should contain
#' at least the values "Yes" and "No". Defaults to Attrition
#' @param group_var A symbol or a character string representing the grouping variable
#' (e.g., Department) by which the data should be grouped. This variable should have
#' categorical data.
#'
#' @return A data frame with two columns:
#' \item{group_var}{The distinct values of the grouping variable.}
#' \item{pct}{The percentage of attrition for each grouping variable value,
#' calculated as the number of "Yes" responses divided by the total responses in that group.}
#'
#' @example
#' # Assuming `train_tbl` is your dataset with variables 'Attrition' and 'Department':
#' result_tbl <- calc_grouped_attrition_pct(train_tbl, Attrition, Department)
#'
#' # The result_tbl will contain the percentage of employees who left
#' # in each department.
#'
#' @import dplyr
#' @export
calc_grouped_attrition_pct <- function(
  data,
  target_var = Attrition,
  group_var
) {
  box::use(
    dplyr[count, filter, group_by, mutate, select, ungroup]
  )

  result <- data |>
    count({{ target_var }}, {{ group_var }}) |>
    group_by({{ group_var }}) |>
    mutate(pct = n / sum(n)) |>
    ungroup() |>
    filter({{ target_var }} == "Yes") |>
    select({{ group_var }}, pct)

  return(result)
}
