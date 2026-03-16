addRegion <- function(cohort){
  cohort|>
  left_join(cdm$location |>
    select(location_source_value, location_id)|>
    inner_join( 
      cdm$care_site |>
        select(location_id, care_site_id),
      by = "location_id"
    ) |>
    left_join(
      cdm$person |>
        select(person_id, care_site_id),
      by = "care_site_id" 
    )|>
    select(location_source_value, person_id)|>
    rename(subject_id=person_id, region=location_source_value)
  )
}

addIMD <- function(cohort){
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
    imd %in% c(9,10) ~ "Q5"), by="subject_id")
    )
}

addEthnicity <- function(cohort){
  cohort|>
    left_join(cdm$person|>
                rename(subject_id=person_id, 
                       ethnicity=race_source_value)|>
                select(subject_id, ethnicity), by="subject_id")
}


addImmunosuppresed <- function(cohort) {
  cohort |>
    addConceptIntersectFlag(conceptSet = list("immuno_condsyst"=
                                                codelist$syst_corticosteriods
    ),
    window=c (-Inf,0)
    ) |> 
    addConceptIntersectFlag(conceptSet=list("immuno_agsyst"=
                                              codelist$transplant),
                            window = list(
                              "last_year" = c(-365, 0)
                            )
    )|>
    addConceptIntersectFlag(conceptSet = list("immuno_agent"=
                                                c(codelist$intrinsec_immune,
                                                  codelist$intrinsec_antineo,
                                                  codelist$intrinsec_antineo_exclude
                                                )
    ),
    window = list(
      "last_1_2year" = c(-183, 0)
    )
    ) |>
    addConceptIntersectFlag(
      conceptSet = list( "immuno_cond"=
                           c(codelist$hiv_aids, 
                             codelist$intrinsec_immune,
                             codelist$scid,
                             codelist$cancerexcludnonmelaskincancer
                           )
      ),
      window = list(
        "last_year" = c(-365, 0)
      )
    ) |> 
    mutate("immunosupressed"= if_else(
      immuno_condsyst_minf_to_0 !="1" & immuno_agsyst_last_year!="1" |
        immuno_agent_last_1_2year !="1" | immuno_cond_last_year !="1", 1L, 0L)
    ) |>
    select(-immuno_agent_last_1_2year,-immuno_cond_last_year, -immuno_agsyst_last_year, -immuno_condsyst_minf_to_0)
}

vaccinated <- function(cohort, vaccine_camp_fin){
  cohort|>
    addCohortIntersectFlag(targetCohortTable=vaccine_camp_fin,
                           window = list(c(0, 120), 
                           nameStyle = "{cohort_name}_{window_name}"))|>
    filter("{cohort_name}_{window_name}"==1L)|>
    select(-"{cohort_name}_{window_name}")
}

addCampaigns <- function(cohort){
  cohort|>
    filter(cohort_start_date>as.Date("2023-10-02") & cohort_start_date<as.Date("2026-01-31"))|>
    mutate(vaccination_campaign = case_when(
      cohort_start_date>=as.Date("2023-10-02") & cohort_start_date<=as.Date("2024-01-31") ~ "A_2023",
      cohort_start_date>=as.Date("2024-04-15") & cohort_start_date<=as.Date("2024-06-30") ~ "S_2024",
      cohort_start_date>=as.Date("2024-10-03") & cohort_start_date<=as.Date("2025-01-31") ~ "A_2024",
      cohort_start_date>=as.Date("2025-04-01") & cohort_start_date<=as.Date("2025-06-17") ~ "S_2025",
      cohort_start_date>=as.Date("2025-09-01") & cohort_start_date<=as.Date("2026-01-31") ~ "A_2025")
    )|>
    filter(!is.na(vaccination_campaign)) 
}

addDose <- function(cohort){
  cohort|> 
    group_by(subject_id) |>
  arrange(cohort_start_date) |> 
  mutate( 
    "n_dose_subject" = paste(row_number(), "dosis")
  ) |>
  ungroup() |>
  group_by(cohort_start_date, n_dose_subject) |>
  add_tally()|>
  ungroup()|> 
  arrange(cohort_start_date)|>
  rename(n_dose=n)
}

addVaccinated <- function(cohort, cohort_vaccinated){
  cohort|> left_join(
    cohort_vaccinated,
    by="subject_id"
  ) |>
    mutate(vaccinated=if_else(
      !is.na(vaccinated_campaign, 1L, 0L)
    ))
    # mutate(vaccination_campaign=case_when(
    #   cohort_start_date==as.Date("2023-10-02"), ~ "A_2023",
    #   cohort_start_date==as.Date("2024-04-15"), ~ "S_2024",
    #   cohort_start_date==as.Date("2024-10-03"), ~ "A_2024",
    #   cohort_start_date==as.Date("2025-04-01"), ~ "S_2025",
    #   cohort_start_date==as.Date("2025-09-01"), ~ "A_2025"
    # ))|>
    # filter(!is.na(vaccination_campaign))|>
    # select(-cohort_start_date, -cohort_end_date)|> #trim dates?
    # left_join(cohort_vaccinated, by=c("subject_id", "vaccination_campaign")
    #           )|>
    # addCohortIntersectFlag(
    #   targetCohortTable = cohort_vaccinated |>
    #     select(vaccination_campaign),  
    #   window = c(0, 0), 
    #   nameStyle = "{cohort_name}_{window_name}"
    # ) 
}
