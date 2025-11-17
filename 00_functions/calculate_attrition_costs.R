calculate_attrition_costs <- function(
  n = 1,
  salary = 80000,
  separation_costs = 500,
  vacancy_costs = 10000,
  acquisition_costs = 4900,
  placement_costs = 3500,
  nett_revenue_per_employee = 250000,
  workdays_per_year = 240,
  workdays_position_open = 40,
  workdays_onboarding = 60,
  onboarding_effectiveness = 0.5
) {
  # direct costs
  direct_costs <- sum(
    separation_costs + vacancy_costs + acquisition_costs + placement_costs
  )

  # lost productivity costs
  productivity_costs <- nett_revenue_per_employee /
    workdays_per_year *
    (workdays_position_open + workdays_onboarding * onboarding_effectiveness)

  # savings of salary & benefits (cost reduction)
  salary_benefit_rediction <- salary /
    workdays_per_year *
    workdays_position_open

  # estimated turnover per employee
  cost_per_employee <- direct_costs +
    productivity_costs -
    salary_benefit_rediction

  # total cost of employee turnover (per year)
  total_cost <- n * cost_per_employee

  return(total_cost)
}
