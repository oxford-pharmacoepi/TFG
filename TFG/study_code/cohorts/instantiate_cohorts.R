cdm$vaccine <- conceptCohort(cdm = cdm,
                             conceptSet = list(
                             "vaccine_record" =
                             vac),
                             name = "vaccine"
)

cdm$vaccine_90 <- cdm$vaccine |>
  requireCohortIntersect(
    targetCohortTable = "vaccine", 
    window = c(-90, -1), #si habiamos windeado el cohort: CUIDADO!!!!
    intersections = 0,
    name="vaccine_90"
  )


cdm$vaccine_camp <- cdm$vaccine_90 |> 
  filter(cohort_start_date>as.Date("2023-10-02") & cohort_start_date<as.Date("2026-01-31"))|>
  mutate(vaccination_campaign = case_when(
    cohort_start_date>=as.Date("2023-10-02") & cohort_start_date<=as.Date("2024-01-31") ~ "A_2023",
    cohort_start_date>=as.Date("2024-04-15") & cohort_start_date<=as.Date("2024-06-30") ~ "S_2024",
    cohort_start_date>=as.Date("2024-10-03") & cohort_start_date<=as.Date("2025-01-31") ~ "A_2024",
    cohort_start_date>=as.Date("2025-04-01") & cohort_start_date<=as.Date("2025-06-17") ~ "S_2025",
    cohort_start_date>=as.Date("2025-09-01") & cohort_start_date<=as.Date("2026-01-31") ~ "A_2025")
  )|>
  filter(!is.na(vaccination_campaign))|>
  left_join(
    get_regions, 
    by= "subject_id")|>
  recordCohortAttrition(reason="vaccine_campaigns") |> # MC separar en diferent steps?
  compute(name = "vaccine_camp") 

cdm$vaccine_90_dose <-cdm$vaccine_90 |>
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
  rename(n_dose=n)|>
  compute(name="vaccine_90_dose")

# MC to use function?
cdm$vaccine_camp_imm <- cdm$vaccine_camp |>
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
                 ),
    name="vaccine_camp_imm"
               )

cdm$vaccine_camp_imm_coh <- cdm$vaccine_camp_imm |> 
  filter(immuno_condsyst_minf_to_0 !="0" & immuno_agsyst_last_year!="0" |
         immuno_agent_last_1_2year !="0" | immuno_cond_last_year !="0"
        ) |>
  #recordCohortAttrition(reason="vaccine_immuno") |>
  compute(name="vaccine_camp_imm_coh")

cdm$vaccine_age_75 <- cdm$vaccine_camp |>
  requireAge(
    ageRange=c(75, Inf),
    cohortId = NULL,
    name = "vaccine_age_75"
  ) |>
  addAge() 

cdm$vaccine_age <- cdm$vaccine_age_75|>
  full_join(cdm$vaccine_camp |>
              filter(vaccination_campaign=="A_2023") |>
              addAge() |>
              filter(age>="65")
  ) |>
  compute(name="vaccine_age")
  #recordCohortAttrition(reason="vaccine_age") 

cdm$vaccine_eligible <- full_join(cdm$vaccine_camp_imm_coh, 
                                  cdm$vaccine_age)|>
  compute(name="vaccine_eligible")
  
cdm$vaccine_camp_fin <- inner_join(cdm$vaccine_camp, cdm$vaccine_eligible) |>
  compute(name="vaccine_camp_fin")  |>
  recordCohortAttrition(reason="eligibles") 

cdm$vaccine_camp_d <- cdm$vaccine_camp |>
  left_join(eth,
            by="subject_id") |> 
  addSex()|> left_join(
    imd, by="subject_id")|>
  compute(name="vaccine_camp_d")

# cdm$vaccine_camp_imm_coh|>tally(): 128
# cdm$vaccine_age|>tally(): 4596
# cdm$vaccine_camp_fin|>tally(): 4639
# We should expect 43 that are only immuno. 

# cdm$vaccine_camp_imm_coh |> 
#   addCohortIntersectFlag(
#     targetCohortTable = "vaccine_age",
#     window = c(0,0)
#   ) |>
#   filter(vaccine_record_0_to_0>0) |>
#   tally()
# we get 85 (85+43 =128) GOOD

# cdm$vaccine_camp_d <- cdm$vaccine_camp |> left_join(
#   imd, by="subject_id")|>
#     compute(name="vaccine_camp_d")

