
cdm$vaccine_90_dose <- cdm$vaccine_90_dose |>
  mutate(
    n_dose_subject = case_when(
      n_dose_subject %in% c("1 dosis", "2 dosis", "3 dosis") ~ n_dose_subject,
      .default = NA_character_
    )
  )
cdm<-bind(cdm$vaccine_camp |>renameCohort("vaccine_camp"),
                     cdm$vaccine_90_dose, 
                     name = "prueba")

result <- summariseResult(
       table = cdm$prueba,
       strata = (c("vaccination_campaign")),
       variables = list(c("vaccination_campaign", "n_dose_subject")),
       estimates = c("count", "percentage"))
  
  tidy(result)