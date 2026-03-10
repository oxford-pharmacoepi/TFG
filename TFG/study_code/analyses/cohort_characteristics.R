characterisation <- df |>
  summariseCharacteristics(
    strata=list("vaccination_campaign"),
    cohortIntersectCount = list(
      "Number of prior vaccines" = list(
        targetCohortTable="vaccine_90",
        window = list("number_of_prior_vaccines" = c(-Inf, -1))
      )
    ),

  #ageGroup = list("kids" = c(0, 17), adults = c(18, Inf)),
    tableIntersectCount = list(
      "Number visits in the prior year" = list(
        tableName = "visit_occurrence",
        window = c(-365, -1)
      )
    ),
  cohortIntersectFlag = list(
    eligibles_age = list(
      targetCohortTable = "vaccine_age",
      window = c(0, 0)
    ),
    eligibles_immunosupressed = list(
      targetCohortTable = "vaccine_camp_imm_coh",
      window = c(0, 0)
    )
  ),
    otherVariables = c("region", "ethnicity", "sex", "deprivation_index")
  )

# characterisationRegCamp <- cdm$vaccine_camp |>
#   mutate(
#     campaign_region = paste(vaccination_campaign, region, sep = "_")
#   ) |>
#   summariseCharacteristics(
#     strata=list("campaign_region"),
#     cohortIntersectCount = list(
#       "Number of prior vaccines" = list(
#         targetCohortTable="vaccine_90",
#         window = list("number_of_prior_vaccines" = c(-Inf, -1))
#       )
#     )
#   )

largeScaleCharacteristics <- summariseLargeScaleCharacteristics(
  cdm$vaccine_camp_fin,
  strata = "vaccination_campaign",
  window = list(c(-Inf, -366), c(-365, 0), c(1, 365),
                c(366, Inf)),
  eventInWindow = "condition_occurrence",
  indexDate = "cohort_start_date",
  censorDate = NULL,
  includeSource = FALSE,
  minimumFrequency = 0.005,
  excludedCodes = c(0)
)

