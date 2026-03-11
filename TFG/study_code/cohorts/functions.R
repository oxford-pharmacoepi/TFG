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

eth <-cdm$person|>rename(subject_id=person_id, 
                   ethnicity=race_source_value)|>
  select(subject_id, ethnicity) |>
  compute(name="eth")

imd <-cdm$measurement |>     
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
  compute(name="imd")
