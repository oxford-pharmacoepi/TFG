#library(here)
#source(here("codelist/codelist_creation.R"))

cdm$vaccine <- conceptCohort(cdm = cdm,
                             conceptSet = list(
                             "vaccine_record" =
                             concepts),
                             name = "vaccine"
)

cdm$vaccine_90 <- cdm$vaccine |>
  requireCohortIntersect(
    targetCohortTable = "vaccine", 
    window = c(-90, -1), #si habiamos windeado el cohort: CUIDADO!!!!
    intersections = 0,
    name="vaccine_90"
  )

get_regions <- cdm$location |>
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
  rename(subject_id=person_id, region=location_source_value) |>
  compute(name="get_regions")

cdm$vaccine_camp <- cdm$vaccine_90 |> 
  filter(cohort_start_date>as.Date("2023-08-02") & cohort_start_date<as.Date("2026-01-01"))|>
  mutate(vaccination_campaign = case_when(
    cohort_start_date>=as.Date("2023-08-02") & cohort_start_date<=as.Date("2024-01-31") ~ "A_2023",
    cohort_start_date>=as.Date("2024-04-15") & cohort_start_date<=as.Date("2024-06-03") ~ "S_2024",
    cohort_start_date>=as.Date("2024-08-03") & cohort_start_date<=as.Date("2024-12-20") ~ "A_2024",
    cohort_start_date>=as.Date("2025-04-01") & cohort_start_date<=as.Date("2025-06-01") ~ "S_2025",
    cohort_start_date>=as.Date("2025-07-01") & cohort_start_date<=as.Date("2026-01-01") ~ "A_2025"),
  )|>
  filter(!is.na(vaccination_campaign))|>
  left_join(
    get_regions, 
    by= "subject_id")|>
  recordCohortAttrition(reason="vaccine_campaigns") |>
  compute(name = "vaccine_camp") 


