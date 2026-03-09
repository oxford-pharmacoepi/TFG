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

cdm$vaccine_90_dose <-cdm$vaccine_90 |>
  group_by(subject_id) |>
  arrange(cohort_start_date) |> 
  mutate( 
    "dose" = paste(row_number(),"dose")
        ) |>
  ungroup() |>
  group_by(cohort_start_date, dose) |>
  add_tally()|>
  ungroup()|> arrange(cohort_start_date)|>
  compute(name="vaccine_90_dose")

# x <- cdm$vaccine_camp |>
#   group_by(vaccination_campaign, cohort_start_date) |>
#   tally() |>
#   collect() |>
#   select(vaccination_campaign, cohort_start_date, n) |>
#   mutate(n = dplyr::if_else(n < 5, 5L, as.integer(n))
#   ) |>
#   collect(name=x)
# 
# x_dose<-cdm$vaccine_90_dose|>
#  select(cohort_start_date, dose, n) |>
#  rename(n_dose=n) |>
#  distinct(cohort_start_date, dose, n_dose) |>
#  group_by(cohort_start_date) |>
#  mutate(n=sum(n_dose))|>
#  mutate(n = dplyr::if_else(n < 5, 5L, as.integer(n))
#  ) |>
#  collect(name=x_dose)

cdm$immun_cond <- conceptCohort(cdm = cdm,
                           conceptSet = list(
                             "immunosupressed" =
                               immun),
                           name = "immun"
)

# cdm$vaccine_camp_immc <- cdm$vaccine_camp |>
#   requireConceptIntersect(
#     conceptSet = list( "immuno_cond"=
#        c(codelist$hiv_aids, 
#          codelist$intrinsec_immune,
#          codelist$scid,
#          codelist$cancerexcludnonmelaskincancer
#       )
#     ),
#     window = list(
#       "last_year" = c(-365, 0)
#     ),
#     intersections = c(1, Inf),
#     name="vaccine_camp_immc"
#   )
# 
# cdm$vaccine_camp_imma <- cdm$vaccine_camp |>
#   requireConceptIntersect(conceptSet = list("immuno_agent"=
#                                               c(codelist$intrinsec_immune,
#                                                 codelist$intrinsec_antineo,
#                                                 codelist$intrinsec_antineo_exclude
#                                               )
#                           ),
#                           window = list(
#                             "last_1_2year" = c(-183, 0)
#                           ),
#                           intersections = c(1, Inf),
#                           name="vaccine_camp_imma"
#   )
# 
# cdm$vaccine_camp_syst <- cdm$vaccine_camp |>
#   requireConceptIntersect(conceptSet = list("immuno_condsyst"=
#                                               codelist$syst_corticosteriods
#                                             ),
#                           window=c (-Inf,0)
#   ) |> 
#   requireConceptIntersect(conceptSet=list("immuno_agsyst"=
#                                             codelist$transplant),
#                           window = list(
#                             "last_year" = c(-365, 0)
#                           ),
#                           intersections = c(1, Inf),
#                           name="vaccine_camp_syst"
#   )

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

cdm$vaccine_elligible <- full_join(cdm$vaccine_camp_imm_coh, 
                                  cdm$vaccine_age)|>
  compute(name="vaccine_elligible")
  
cdm$vaccine_camp_fin <- inner_join(cdm$vaccine_camp, cdm$vaccine_elligible) |>
  compute(name="vaccine_camp_fin")  |>
  recordCohortAttrition(reason="elligibles") 
  
