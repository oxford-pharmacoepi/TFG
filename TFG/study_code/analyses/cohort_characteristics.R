characterisationRegion <- cdm$vaccine_camp |>
  summariseCharacteristics(
    strata=list("vaccination_campaign", "region"),
    cohortIntersectCount = list(
      "Number of prior vaccines" = list(
        targetCohortTable="vaccine_90",
        window = list("number_of_prior_vaccines" = c(-Inf, -1))
      )
    )
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
