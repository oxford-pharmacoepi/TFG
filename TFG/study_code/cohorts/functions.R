addRegion <- function(cohort) {
  cohort |>
    left_join(
      cdm$location |>
        select(location_source_value, location_id) |>
        inner_join(
          cdm$care_site |>
            select(location_id, care_site_id),
          by = "location_id"
        ) |>
        left_join(
          cdm$person |>
            select(person_id, care_site_id),
          by = "care_site_id"
        ) |>
        select(location_source_value, person_id) |>
        rename(subject_id = person_id, region = location_source_value)
    )
  # MC add compute + acocunt for missing region
}

addIMD <- function(cohort) {
  cohort |>
    left_join(
      cdm$measurement |>
        filter(measurement_concept_id == "715996") |>
        select(person_id, value_as_number) |>
        rename(subject_id = person_id, imd = value_as_number) |>
        mutate(imd = case_when(
          imd %in% c(1, 2) ~ "Q1",
          imd %in% c(3, 4) ~ "Q2",
          imd %in% c(5, 6) ~ "Q3",
          imd %in% c(7, 8) ~ "Q4",
          imd %in% c(9, 10) ~ "Q5"
        )),
      by = "subject_id"
    )
  # MC add compute + account for missing IMD
}

addEthnicity <- function(cohort) {
  cohort |>
    left_join(
      cdm$person |>
        rename(
          subject_id = person_id,
          ethnicity = race_source_value
        ) |>
        select(subject_id, ethnicity),
      by = "subject_id"
    )
  # MC add compute + account for missing values
}

addImmunosuppressed <- function(cohort) {
  cohort |>
    addConceptIntersectFlag(
      conceptSet = list(
        # MC equivalent a: conceptSet = codelist["syst_corticosteriods"]
        "immuno_condsyst" =
          codelist$syst_corticosteriods
      ),
      window = list(
        "last_1_2year" = c(-183, 0)
      )
    ) |>
    addConceptIntersectFlag(
      conceptSet = list(
        "immuno_agsyst" =
          codelist$transplant
      ),
      window = list(
        "last_year" = c(-365, 0)
      )
    ) |>
    addConceptIntersectFlag(
      conceptSet = list(
        "immuno_agent" =
          c(
            codelist$inmmunos_antineo,
            codelist$immunos_antineo_exclude
          )
      ),
      window = list(
        "last_1_2year" = c(-183, 0)
      )
    ) |>
    addConceptIntersectFlag(
      conceptSet = list(
        "immuno_cond" =
          c(
            codelist$hiv_aids,
            codelist$intrinsec_immune,
            codelist$scid,
            codelist$cancerexcludnonmelaskincancer
          )
      ),
      window = list(
        "last_year" = c(-365, 0)
      )
    ) |>
    mutate(
      immunosuppressed = if_else(
        (immuno_condsyst_last_1_2year == 1 & immuno_agsyst_last_year == 1) |
          immuno_agent_last_1_2year == 1 |
          immuno_cond_last_year == 1,
        1L,
        0L
      )
    ) |>
    select(-immuno_agent_last_1_2year, -immuno_cond_last_year, -immuno_agsyst_last_year, -immuno_condsyst_last_1_2year)
  # compute(name=tableName(cohort))
  # MC add compute
}

addCampaigns <- function(cohort) {
  cohort |>
    # filter(cohort_start_date>as.Date("2023-10-02") & cohort_start_date<as.Date("2026-01-31"))|>
    mutate(vaccination_campaign = case_when(
      cohort_start_date >= as.Date("2023-10-02") & cohort_start_date <= as.Date("2024-01-31") ~ "a_2023",
      cohort_start_date >= as.Date("2024-04-15") & cohort_start_date <= as.Date("2024-06-30") ~ "s_2024",
      cohort_start_date >= as.Date("2024-10-03") & cohort_start_date <= as.Date("2025-01-31") ~ "a_2024",
      cohort_start_date >= as.Date("2025-04-01") & cohort_start_date <= as.Date("2025-06-17") ~ "s_2025",
      cohort_start_date >= as.Date("2025-09-01") & cohort_start_date <= as.Date("2026-01-31") ~ "a_2025",
      TRUE ~ NA_character_
    ))
  # |> compute(name=tableName(cohort))
  #   |> filter(!is.na(vaccination_campaign))
  # MC add compute
}

# MC quite sure this function dont do waht you want
addDose <- function(cohort, vaccine_cohort) {
  vaccine_cohort_dose <- vaccine_cohort |>
    group_by(subject_id) |>
    arrange(cohort_start_date) |>
    mutate(dose = paste(row_number(), "dose")) |>
    ungroup() |>
    group_by(cohort_start_date, dose) |>
    add_tally(name = "n_dose") |>
    ungroup()

  if (is.null(cohort)) {
    return(vaccine_cohort_dose)
  }

  cohort |>
    left_join(
      vaccine_cohort_dose |> select(subject_id, n_dose, dose),
      by = "subject_id"
    )
  # |> if_else(
  #     is.na(n_dose), 0, n_dose)|>
  #   if_else(
  #     is.na(n_dose), "0 dose", dose)
}

requireCampaign <- function(vaccine_cohort, campaign) {
  vaccine_cohort |>
    filter(vaccination_campaign == campaign) |>
    mutate(vaccine_start_date = cohort_start_date) |>
    # MC record reason?
    compute(name = "vaccine_90_{campaign}")
}

# MC not sure this function does what you want
addVaccinated <- function(cohort, vaccine_cohort_s) {
  cohort |>
    addCohortIntersectFlag(
      targetCohortTable = tableName(vaccine_cohort_s),
      indexDate = "cohort_start_date",
      targetStartDate = "vaccine_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-Inf, Inf)),
      nameStyle = "vaccinated",
      name = tableName(cohort)
    ) |>
    left_join(
      vaccine_cohort_s |> select(vaccine_start_date, subject_id),
      by = "subject_id"
    ) |>
    mutate(cohort_start_date = vaccine_start_date) |>
    mutate(cohort_end_date = cohort_start_date) |>
    select(-vaccine_start_date)
}

requireObs <- function(cohort, campaign) {
  # MC to use switch
  start <- case_when(
    campaign == "a_2023" ~ as.Date("2023-01-31"),
    campaign == "s_2024" ~ as.Date("2023-06-30"),
    campaign == "a_2024" ~ as.Date("2024-01-31"),
    campaign == "s_2025" ~ as.Date("2024-06-17"),
    campaign == "a_2025" ~ as.Date("2025-01-31")
  )

  end <- case_when(
    campaign == "a_2023" ~ as.Date("2023-10-02"),
    campaign == "s_2024" ~ as.Date("2024-04-15"),
    campaign == "a_2024" ~ as.Date("2024-10-03"),
    campaign == "s_2025" ~ as.Date("2025-04-01"),
    campaign == "a_2025" ~ as.Date("2025-09-01")
  )

  # MC not sure this does what you want
  cohort |>
    requireInDateRange(
      dateRange = as.Date(c(NA, start)),
      indexDate = "cohort_start_date"
    ) |>
    requireInDateRange(
      dateRange = as.Date(c(end, NA)),
      indexDate = "cohort_end_date"
    ) |>
    requireDuration(
      daysInCohort = c(365, Inf)
    )
}

# MC not sure what is the purpose of this function
addDatesCampaignAge <- function(cohort, campaign) {
  date <- case_when(
    campaign == "a_2023" ~ as.Date("2023-10-02"),
    campaign == "s_2024" ~ as.Date("2024-04-15"),
    campaign == "a_2024" ~ as.Date("2024-10-03"),
    campaign == "s_2025" ~ as.Date("2025-04-01"),
    campaign == "a_2025" ~ as.Date("2025-09-01")
  )
  # cohort_c <- cohort
  # non_vaccinated<-cohort_c|>filter(vaccinated == 0L)|>
  #   trimToDateRange(
  #   dateRange=as.Date(c(date, date)),
  #   cohortId = NULL,
  #   startDate = "cohort_start_date",
  #   endDate = "cohort_end_date"
  # )
  cohort |>
    mutate(cohort_start_date = if_else(
      is.na(cohort_start_date),
      date,
      cohort_start_date
    )) |>
    mutate(cohort_end_date = cohort_start_date)
}
