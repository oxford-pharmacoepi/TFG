cdm$vaccine_90 <- conceptCohort(cdm = cdm,
                             conceptSet = list(
                               "vaccine_record" =
                                 vac),
                             name = "vaccine"
)|>
  requireCohortIntersect(
    targetCohortTable = "vaccine", 
    window = c(-90, -1), #si habiamos windeado el cohort: CUIDADO!!!!
    intersections = 0,
    name="vaccine_90"
  )

cdm$vaccine_camp <- cdm$vaccine_90 |> 
  addCampaigns()|>
  #recordCohortAttrition(reason="vaccine_campaigns") |>
  compute(name = "vaccine_camp") 

cdm$vaccine_90_dose <-cdm$vaccine_90 |>
  addDose()|>
  compute(name="vaccine_90_dose")|>
  addCampaigns()|>
  compute(name="vaccine_90_dose")

cdm$vaccine_camp_imm <- cdm$vaccine_camp|>
  addImmunosuppressed()|>
  compute(name="vaccine_camp_imm") |> #sense aquesta linea no funciona? NO
  filter(immunosuppressed == 1L)|>
  compute(name="vaccine_camp_imm")

cdm$vaccine_age <- cdm$vaccine_camp |>
  requireAge(
    ageRange=c(75, Inf),
    cohortId = NULL,
    name = "vaccine_age_75"
  ) |>
  addAge(name="vaccine_age_75") |>
  full_join(cdm$vaccine_camp |>
              filter(vaccination_campaign=="A_2023") |>
              addAge() |>
              filter(age>="65")
  ) |>
  compute(name="vaccine_age")

cdm$vaccine_eligible <- full_join(cdm$vaccine_camp_imm, 
                                  cdm$vaccine_age)|>
  mutate(immunosuppressed=if_else(is.na(immunosuppressed), 0L, 1L)) |>
  compute(name="vaccine_eligible")

cdm$vaccine_camp_fin <- inner_join(cdm$vaccine_camp 
                                   , cdm$vaccine_eligible) |>
  compute(name="vaccine_camp_fin")  |>
  recordCohortAttrition(reason="eligibles") 

cdm$vaccine_camp_d <- cdm$vaccine_camp |>
  addEthnicity() |> 
  addSex()|> 
  addIMD()|>
  compute(name="vaccine_camp_d")

