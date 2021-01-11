library(tidymodels)
library(magrittr)
library(dplyr)
library(sjmisc)
library(magrittr)
library(haven)
library(sjlabelled)

employee_attrition_tbl <- read.csv("C:/Users/rueta/Desktop/Data_Science_ML/data/HR-Employee-Attrition.csv")

dept_job_role_tbl <- employee_attrition_tbl %>%
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

dept_job_role_tbl %>%
  
  group_by(Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n))

# Attrition by department

dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2: Caution: It's easy to inadvertently miss grouping when creating counts & percents within groups
  group_by(Department) %>%
  mutate(pct = n / sum(n))


# Attrition by job role

dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes")



##Develop KPI

dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  )


# Function to calculate attrition cost
calculate_attrition_cost <- function(
  
  # Employee
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
  
) {
  
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year *
    (workdays_position_open + workdays_onboarding * onboarding_efficiency)
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
}

calculate_attrition_cost()
## [1] 78483.33
calculate_attrition_cost(200)
## [1] 15696667


dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  
  # Block 4. Set salaray to 80000 for now
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  )

dept_job_role_tbl %>%
  count(Department, JobRole, Attrition)


dept_job_role_tbl %>%
  
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  )



assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
  
  attrition_col_expr <- enquo(attrition_col)
  
  data %>%
    
    # Use parenthesis () to give tidy eval evaluation priority
    filter((!! attrition_col_expr) %in% attrition_value) %>%
    arrange(desc(pct)) %>%
    mutate(
      # Function inputs in numeric format (e.g. baseline_pct = 0.088 don't require tidy eval)
      above_industry_avg = case_when(
        pct > baseline_pct ~ "Yes",
        TRUE ~ "No"
      )
    )
  
}