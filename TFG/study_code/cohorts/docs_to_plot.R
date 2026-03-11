x <- cdm$vaccine_camp |>
  group_by(vaccination_campaign, cohort_start_date) |>
  tally() |>
  collect() |>
  select(vaccination_campaign, cohort_start_date, n) |>
  mutate(n = dplyr::if_else(n < 5, 5L, as.integer(n))
  ) |>
  collect(name=x)

x_dose<-cdm$vaccine_90_dose|>
 select(cohort_start_date, dose, n) |>
 rename(n_dose=n) |>
 distinct(cohort_start_date, dose, n_dose) |>
 group_by(cohort_start_date) |>
 mutate(n=sum(n_dose))|>
 mutate(n = dplyr::if_else(n < 5, 5L, as.integer(n))
 ) |>
 collect(name=x_dose)