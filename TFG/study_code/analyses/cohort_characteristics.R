characterisationRegion <- cdm$vaccine_camp |>
  summariseCharacteristics(
    strata=list("vaccination_campaign"),
    cohortIntersectCount = list(
      "Number of prior vaccines" = list(
        targetCohortTable="vaccine_90",
        window = list("number_of_prior_vaccines" = c(-Inf, -1))
      )
    ),
    ageGroup = list("kids" = c(0, 17), adults = c(18, Inf)),
    tableIntersectCount = list(
      "Number visits in the prior year" = list(
        tableName = "visit_occurrence",
        window = c(-365, -1)
      )
    ),
    otherVariables = "region"
  )

characterisationRegCamp <- cdm$vaccine_camp |>
  mutate(
    campaign_region = paste(vaccination_campaign, region, sep = "_")
  ) |>
  summariseCharacteristics(
    strata=list("campaign_region"),
    cohortIntersectCount = list(
      "Number of prior vaccines" = list(
        targetCohortTable="vaccine_90",
        window = list("number_of_prior_vaccines" = c(-Inf, -1))
      )
    )
  )

LargeScaleCharacteristics <- summariseLargeScaleCharacteristics(
  cdm$vaccine_camp,
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

x <- cdm$vaccine_camp |>
  group_by(vaccination_campaign, cohort_start_date) |>
  tally() |>
  collect() |>
  select(vaccination_campaign, cohort_start_date, n) |>
  mutate(n = dplyr::if_else(n < 5, 5L, as.integer(n))
  ) |>
  collect(name=x)
  
                
