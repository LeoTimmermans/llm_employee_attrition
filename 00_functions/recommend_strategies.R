recommend_strategies <- function(data, employee_number) {
  data %>%
    filter(EmployeeNumber == employee_number) %>%
    mutate_if(is.factor, as.numeric) %>%

    # Personal Development Strategy
    mutate(
      personal_development_strategy = case_when(
        # (Worst Case) Create Personal Development Plan: JobInvolvement, JobSatisfaction, PerfomanceRating
        PerformanceRating == 1 |
          JobSatisfaction == 1 |
          JobInvolvement <= 2 ~
          "Create Personal Development Plan",

        # (Better Case) Promote Training and Formation: YearsAtCompany, TotalWorkingYears
        YearsAtCompany < 3 |
          TotalWorkingYears < 6 ~
          "Promote Training and Formation",

        # (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction
        (YearsInCurrentRole > 3 | YearsAtCompany >= 5) &
          PerformanceRating >= 3 &
          JobSatisfaction == 4 ~
          "Seek Mentorship Role",

        # (Best Case 2) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating
        JobInvolvement >= 3 &
          JobSatisfaction >= 3 &
          PerformanceRating >= 3 ~
          "Seek Leadership Role",

        # Catch All
        TRUE ~ "Retain and Maintain"
      )
    ) %>%
    # select(EmployeeNumber, personal_development_strategy)

    # Professional Development Strategy
    mutate(
      professional_development_strategy = case_when(
        # Ready For Rotation: YearsInCurrentRole, JobSatisfaction (LOW)
        YearsInCurrentRole >= 2 &
          JobSatisfaction <= 2 ~
          "Ready for Rotation",

        # Ready For Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 1 &
          YearsInCurrentRole >= 2 &
          JobInvolvement >= 3 &
          PerformanceRating >= 3 ~
          "Ready for Promotion",

        # Ready For Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 2 &
          YearsInCurrentRole >= 2 &
          JobInvolvement >= 4 &
          PerformanceRating >= 3 ~
          "Ready for Promotion",

        # Ready For Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 3 &
          YearsInCurrentRole >= 3 &
          JobInvolvement >= 4 &
          PerformanceRating >= 3 ~
          "Ready for Promotion",

        # Ready For Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 4 &
          YearsInCurrentRole >= 4 &
          JobInvolvement >= 4 &
          PerformanceRating >= 3 ~
          "Ready for Promotion",

        # Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating
        YearsInCurrentRole >= 4 &
          JobSatisfaction >= 4 &
          PerformanceRating >= 3 ~
          "Incentivize Specialization",

        # Catch All
        TRUE ~ "Retain and Maintain"
      )
    ) %>%
    # select(EmployeeNumber, personal_development_strategy, professional_development_strategy)

    # Work Environment Strategy
    mutate(
      work_environment_strategy = case_when(
        # Improve Work-Life Balance: OverTime, WorkLifeBalance
        OverTime == 2 |
          WorkLifeBalance == 1 ~
          "Improve Work-Life Balance",

        # Monitor Business Travel: BusinessTravel, DistanceFromHome, WorkLifeBalance
        (BusinessTravel == 3 |
          DistanceFromHome >= 10) &
          WorkLifeBalance == 2 ~
          "Monitor Business Travel",

        # Review Job Assignment: EnvironmentSatisfaction, YearsInCurrentRole
        EnvironmentSatisfaction == 1 &
          YearsInCurrentRole >= 2 ~
          "Review Job Assignment",

        # Promote Job Engagement: JobInvolvement
        JobInvolvement <= 2 ~ "Promote Job Engagement",

        # Catch All
        TRUE ~ "Retain and Maintain"
      )
    ) %>%
    select(
      EmployeeNumber,
      personal_development_strategy,
      professional_development_strategy,
      work_environment_strategy
    )
}
