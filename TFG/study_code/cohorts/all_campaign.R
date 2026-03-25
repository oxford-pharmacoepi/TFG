# Select the individuals to be included for the coverage assessment
cdm$demo <- demographicsCohort(cdm, name = "demo")

campaign <- "a_2023"
cdm$campaign1 <- cdm$demo |>
  copyCohorts(n = 1, name = "campaign1") |>
  trimDatesIntoCampaign(campaign) |>
  addVaccinatedInCampaign() |>
  addImmunosuppressed() |>
  addAge(name = "campaign1") |>
  filter(age >= 65L | immunosuppressed == 1L) |>
  compute(name = "campaign1")|>
  recordCohortAttrition(reason = "Eligible for vaccination") |>
  # to consider to add this with demo cohort
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  
  addDoseCampaign() |>
  addDosePriorCampaign() |>
  filter(prior_dose>=2L) |>
  compute(name = "campaign1")|>
  recordCohortAttrition(reason = "At least 2 doses at campaign start") |>
  addSex(name = "campaign1")

