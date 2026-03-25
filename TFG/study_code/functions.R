#functions to be used for the cohort creation
addIMD <- function(cohort, name = tableName(cohort)){
  cohort|>
    left_join(cdm$measurement |>     
  filter(measurement_concept_id== "715996")|>
  select(person_id, value_as_number)|>
  rename(subject_id=person_id, imd=value_as_number)|>
  mutate(imd = case_when(
    imd %in% c(1,2) ~ "Q1",
    imd %in% c(3,4) ~ "Q2",
    imd %in% c(5,6) ~ "Q3",
    imd %in% c(7,8) ~ "Q4",
    imd %in% c(9,10) ~ "Q5")
    )|>
    mutate(imd = coalesce(imd, "missing")), by="subject_id") |>
    compute(name = name) 
  }

addEthnicity <- function(cohort, name = tableName(cohort)){
  cohort|>
    left_join(cdm$person|>
                rename(subject_id=person_id, 
                       ethnicity=race_source_value)|>
                select(subject_id, ethnicity) |>
                mutate(ethnicity = coalesce(ethnicity, "missing")), by="subject_id") |>
    compute(name = name)
}

addRegion <- function(cohort,  name = tableName(cohort)) {
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
        rename(subject_id = person_id, region = location_source_value)|>
        mutate(region = coalesce(region, "missing")), by="subject_id") |>
    compute(name = name)
}

addImmunosuppressed <- function(cohort, name = tableName(cohort)) {
  cohort |>
    addConceptIntersectFlag(
      conceptSet = list(
        # MC equivalent a: conceptSet = codelist["syst_corticosteriods"]
        "immuno_condsyst" =
          codelist$syst_corticosteriods
      ),
      window = list(
        "last_1_2year" = c(-183, 0)
      ),
      name = name
    ) |>
    addConceptIntersectFlag(
      conceptSet = list(
        "immuno_agsyst" =
          codelist$transplant
      ),
      window = list(
        "last_year" = c(-365, 0)
      ),
      name = name
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
      ),
      name = name
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
      ),
      name = name
    ) |>
    mutate(
      immunosuppressed = if_else(
        (immuno_condsyst_last_1_2year == 1 & immuno_agsyst_last_year == 1) |
          immuno_agent_last_1_2year == 1 |
          immuno_cond_last_year == 1,
        1L, 0L)
    ) |>
    select(-immuno_agent_last_1_2year,-immuno_cond_last_year, -immuno_agsyst_last_year, -immuno_condsyst_last_1_2year) |>
    compute(name = name)
}

addCampaigns <- function(cohort, name = tableName(cohort)){
  cohort|>
    #filter(cohort_start_date>as.Date("2023-10-02") & cohort_start_date<as.Date("2026-01-31"))|>
    mutate(vaccination_campaign = case_when(
      cohort_start_date>=as.Date("2023-10-02") & cohort_start_date<=as.Date("2024-01-31") ~ "a_2023",
      cohort_start_date>=as.Date("2024-04-15") & cohort_start_date<=as.Date("2024-06-30") ~ "s_2024",
      cohort_start_date>=as.Date("2024-10-03") & cohort_start_date<=as.Date("2025-01-31") ~ "a_2024",
      cohort_start_date>=as.Date("2025-04-01") & cohort_start_date<=as.Date("2025-06-17") ~ "s_2025",
      cohort_start_date>=as.Date("2025-09-01") & cohort_start_date<=as.Date("2026-01-31") ~ "a_2025",
      TRUE ~ NA_character_)
    ) |>
    mutate(vaccination_campaign = coalesce(vaccination_campaign, "None")) |>
  compute(name = name)
 #   |> filter(!is.na(vaccination_campaign)) 
  
}

addDoseCampaign <- function(cohort, name = tableName(cohort)) {
  cohort |>
    addCohortIntersectField(
      targetCohortTable = "vaccine_90",
      field = "dose",
      indexDate = "cohort_start_date", # start of vaccination campaign
      censorDate = "cohort_end_date", # end of vaccination campaign
      order = "first",
      window = list(c(0, Inf)),
      nameStyle = "vaccine_dose",
      name = name
    ) 
}

addDosePriorCampaign <- function(cohort, name = tableName(cohort)) {
  cohort|>
    addCohortIntersectField(
      targetCohortTable = "vaccine_90",
      field = "dose",
      indexDate = "cohort_start_date", # start of vaccination campaign
      censorDate = "cohort_end_date", # end of vaccination campaign
      order = "last",
      window = list(c(-Inf, -1)),
      nameStyle = "prior_dose",
      name = name
    )|>
    # time since last dose
    addCohortIntersectDays(
      targetCohortTable = "vaccine_90",
      window = c(-Inf, -1),
      order = "last",
      nameStyle = "last_dose_days",
      name = name
    )
    
}

requireCampaign <- function(vaccine_cohort, campaign, name = "{vaccine_cohort}_{campaign}"){
  vaccine_cohort |>
    filter(vaccination_campaign == campaign) |>
    mutate(vaccine_start_date=cohort_start_date)|>
    compute(name = name)
}

addVaccinatedInCampaign <- function(cohort, name = tableName(cohort)){
  cohort |> 
    addCohortIntersectFlag(
    targetCohortTable = "vaccine_90",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    window = list(c(0, Inf)),
    nameStyle = "vaccinated",
    name = name
   )
}

trimDatesIntoCampaign <- function(cohort, campaign) {
  start <- switch(campaign,
     "a_2023" = as.Date("2023-10-02"),
     "s_2024" = as.Date("2024-04-15"),
     "a_2024" = as.Date("2024-10-03"),
     "s_2025" = as.Date("2025-04-01"),
     "a_2025" = as.Date("2025-09-01")
     )
  end <- switch(campaign,
     "a_2023" = as.Date("2024-01-31"),
     "s_2024" = as.Date("2024-06-30"),
     "a_2024" = as.Date("2025-01-31"),
     "s_2025" = as.Date("2025-06-17"),
     "a_2025" = as.Date("2026-01-31")
     )

  cohort|>
    trimToDateRange(c(start, end)) |>
    requirePriorObservation(minPriorObservation = 365)
}

addAgeEligibility <- function(cohort, name = tableName(cohort), campaign ="all") {
  cohort|>
    mutate(age_eligibility=case_when(
      (vaccination_campaign == "a_2023" | campaign == "a_2023") & age >= 65 ~ 1L, 
      (vaccination_campaign == "s_2024" | campaign == "s_2024") & age >= 75 ~ 1L,
      (vaccination_campaign == "a_2024" | campaign == "a_2024") & age >= 75 ~ 1L,
      (vaccination_campaign == "s_2025" | campaign == "s_2025") & age >= 75 ~ 1L,
      (vaccination_campaign == "a_2025" | campaign == "a_2025") & age >= 75 ~ 1L,
  TRUE ~ 0L) 
    ) |> 
    compute(name = name)
}

