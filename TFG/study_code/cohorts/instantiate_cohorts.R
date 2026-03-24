# Preparing vaccinated people's cohort

# Vaccine records
cdm$vaccine <- conceptCohort(cdm = cdm,
                             conceptSet = list(
                               "vaccine_record" =
                                 vac),
                             name = "vaccine"
)

# Vaccine record with 90 days washout  +
# Addition of the number of dose from the received vaccines for each subject +
# Addition of the campaign the vaccine corresponds to 
cdm$vaccine_90 <- cdm$vaccine |>
  requireCohortIntersect(
    targetCohortTable = "vaccine", 
    window = c(-90, -1), #CAREFUL!! The cohort can't be windowed
    intersections = 0,
    name="vaccine_90"
  ) |>
  group_by(subject_id) |>
  arrange(cohort_start_date) |>
  mutate(dose = row_number()) |>
  ungroup() |>
  addCampaigns()
  
