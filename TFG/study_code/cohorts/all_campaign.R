cdm$demo <- demographicsCohort(
    cdm, 
    ageRange = NULL, 
    sex = NULL,
    minPriorObservation = NULL, # cohort_start_date will start one year later if we put 365
    name = "demo"
)

campaign<- "A_2023"
cdm$campaign1 <- cdm$demo |>
  requireObs(campaign)|>
  addImmunosuppressed() |>
  addAge() |>
  compute(name = "campaign1") |> 
  filter(age >= 65L | immunosuppressed == 1L) |>
  recordCohortAttrition(reason = "Inclusion criteria") |>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addSex(name = "campaign1") |>
  addVaccinated(cdm$vaccine_camp_fin, campaign) |>
  compute(name = "campaign1")|>
  addDateNonVaccinated(campaign) |>
  compute(name = "campaign1")

campaign<- "S_2024"
cdm$campaign1 <- cdm$demo |>
  requireObs(campaign)|>
  addImmunosuppressed() |>
  addAge() |>
  compute(name = "campaign2") |> 
  filter(age >= 75L | immunosuppressed == 1L) |>
  recordCohortAttrition(reason = "Inclusion criteria") |>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addSex(name = "campaign2") |>
  addVaccinated(cdm$vaccine_camp_fin, campaign) |>
  compute(name = "campaign2")|>
  addDateNonVaccinated(campaign) |>
  compute(name = "campaign2")

campaign<- "A_2024"
cdm$campaign1 <- cdm$demo |>
  requireObs(campaign)|>
  addImmunosuppressed() |>
  addAge() |>
  compute(name = "campaign3") |> 
  filter(age >= 75L | immunosuppressed == 1L) |>
  recordCohortAttrition(reason = "Inclusion criteria") |>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addSex(name = "campaign3") |>
  addVaccinated(cdm$vaccine_camp_fin, campaign) |>
  compute(name = "campaign3")|>
  addDateNonVaccinated(campaign) |>
  compute(name = "campaign3")

campaign<- "S_2025"
cdm$campaign1 <- cdm$demo |>
  requireObs(campaign)|>
  addImmunosuppressed() |>
  addAge() |>
  compute(name = "campaign4") |> 
  filter(age >= 75L | immunosuppressed == 1L) |>
  recordCohortAttrition(reason = "Inclusion criteria") |>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addSex(name = "campaign4") |>
  addVaccinated(cdm$vaccine_camp_fin, campaign) |>
  compute(name = "campaign4")|>
  addDateNonVaccinated(campaign) |>
  compute(name = "campaign4")

cdm$cdm <- bind(cdm$campaign1, 
            cdm$campaign2, 
            cdm$campaign3,
            cdm$campaign4,
            name = "all_campaigns")

cdm$all_campaigns <- cdm$all_campaigns |>
  addCohortName()

result <- summariseResult(
  table = cdm$campaign1, 
  group = "cohort_name",
  strata = combineStrata(c("region", "imd", "sex", "ethnicity")), 
  variables = "vaccinated", 
  estimates = c("count", "percentage")
)

tidy(result)

  
  requireInDateRange(
    dateRange=as.Date(c("2022-10-02", NA)),
    cohortId = NULL,
    indexDate = "cohort_start_date",
    atFirst = FALSE,
    name = "all"
  )


cdm$all_d <- cdm$all |> 
  left_join(imd, by="subject_id")|> 
  left_join(
    get_regions, by="subject_id"  
  ) |>
  addSex()|>
  left_join(
  eth, by="subject_id"  
  )
compute(name="all_d")

cdm$all_di <- cdm$all_d |>
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
  compute(name="all_di")

cdm$all_immuno <- cdm$all_di |>
  filter(immuno_condsyst_minf_to_0 !="0" & immuno_agsyst_last_year!="0" |
           immuno_agent_last_1_2year !="0" | immuno_cond_last_year !="0"
  ) |>
  select(-immuno_agent_last_1_2year,-immuno_cond_last_year, -immuno_agsyst_last_year, -immuno_condsyst_minf_to_0)|>
  compute(name="all_immuno")|>
  recordCohortAttrition(reason="immunosuppressed")

  
cdm$all_a2023 <- cdm$all_immuno|>
    requireAge(
    ageRange=c(75, Inf),
    cohortId = NULL
  ) |>
  addAge() |>
  full_join(cdm$all_d |>
              filter(cohort_start_date>=as.Date("2023-10-02") & cohort_start_date<=as.Date("2024-01-31")) |>
              addAge() |>
              filter(age>="65")
  ) |>
    compute(name="all_denom")
  
