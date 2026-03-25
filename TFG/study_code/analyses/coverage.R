# I will start working with one campaign (a_2023)

# Denominator eligibility conditioned by the number of prior doses 
# (e.g., individuals with 2 doses won't be eligible for the 1 dosis)
cdm$campaign1 <- cdm$campaign1 |>
  mutate(eligibility_3_dose = if_else(
    prior_dose == 2L, 1L, 0L))

result <- summariseResult(
  table = cdm$campaign1, 
  group = "cohort_name",
  strata = combineStrata(c("region", "imd", "sex", "ethnicity")), 
  variables = list(c("vaccinated", "dose", "prior_dose"), c("cohort_start_date")),
  estimates = list(c("count", "percentage"), c("min", "max", "median", "q25", "q75")))

#tidy(result)

#visOmopResults::visOmopTable(result|>filter(result_id==1), header = c("cohort_name"))
