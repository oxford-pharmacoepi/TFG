# All of the vaccinated people stratified for all campaigs, where the 2 dosis 
# filter isn't considered
characterisation <- cdm$vaccinated_within_campaigns |>
  summariseCharacteristics(
    strata=list("vaccination_campaign"),
    cohortIntersectCount = list(
      "Number of prior vaccines" = list(
        targetCohortTable="vaccine_90",
        window = list("number_of_prior_vaccines" = c(-Inf, -1))
      )
    ),
    tableIntersectCount = list(
      "Number visits in the prior year" = list(
        tableName = "visit_occurrence",
        window = c(-365, -1)
      )
    ),
    otherVariables = c("region", "ethnicity", "sex", "imd",
                                 "immunosuppressed", "age_eligibility")
)


