x_90 <- cdm$vaccine_90 |>
  group_by(cohort_start_date) |>
  tally() |>
  collect() |>
  select(cohort_start_date, n) |>
  mutate(n = dplyr::if_else(n < 5, 5L, as.integer(n))
  ) |>
  collect(name=x_90)

x_dose<-cdm$vaccine_90_dose|>
  distinct(cohort_start_date, n_dose, n_dose_subject) |>
  group_by(cohort_start_date) |>
  mutate(n=sum(n_dose))|>
  mutate(n = dplyr::if_else(n < 5, 5L, as.integer(n))
 ) |>
  collect(name=x_dose)

# Groups:   cohort_start_date [1,327] què quadra