# Select the individuals to be included for the coverage assessment
cdm$demo <- demographicsCohort(cdm, name = "demo")

campaign <- "a_2023"
cdm$campaign1 <- cdm$demo |>
  copyCohorts(n = 1, name = "campaign1") |>
  trimDatesIntoCampaign(campaign) |>
  recordCohortAttrition(reason = "In observation")|>
  compute(name = "campaign1") |>
  addVaccinatedInCampaign() |>
  addImmunosuppressed() |>
  addAge() |>
  compute(name = "campaign1") |> 
  filter(age >= 65L | immunosuppressed == 1L) |>
  recordCohortAttrition(reason = "Inclusion criteria") |>
  compute(name = "campaign1")|>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addDoseCampaign() |>
  addDosePriorCampaign() |>
  addSex(name = "campaign1")

campaign<- "s_2024"
cdm$vaccine_camp_2024<-cdm$vaccine_camp|>
  requireCampaign(campaign)|>
  compute(name = "vaccine_camp_s_2024")

cdm$campaign2 <- cdm$demo |>
  requireObs(campaign)|>
  recordCohortAttrition(reason = "In observation")|>
  compute(name = "campaign2")|>
  addVaccinated(cdm$vaccine_camp_s_2024) |>
  addDatesCampaignAge(campaign)|>
  addImmunosuppressed() |>
  addAge() |>
  compute(name = "campaign2") |> 
  filter(age >= 75L | immunosuppressed == 1L) |>
  recordCohortAttrition(reason = "Inclusion criteria") |>
  compute(name = "campaign2")|>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addDose(cdm$vaccine_90)|>
  addSex(name = "campaign2") 

campaign<- "a_2024"
cdm$vaccine_camp_a_2024<-cdm$vaccine_camp|>
  requireCampaign(campaign)|>
  compute(name = "vaccine_camp_a_2024")

cdm$campaign3 <- cdm$demo2 |>
  requireObs(campaign)|>
  compute(name = "campaign3")|>
  recordCohortAttrition(reason = "In observation")|>
  compute(name = "campaign3")|>
  addVaccinated(cdm$vaccine_camp_a_2024) |>
  addDatesCampaignAge(campaign)|>
  addImmunosuppressed() |>
  addAge() |>
  compute(name = "campaign3") |> 
  filter(age >= 75L | immunosuppressed == 1L) |>
  recordCohortAttrition(reason = "Inclusion criteria") |>
  compute(name = "campaign3")|>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addDose(cdm$vaccine_90)|>
  addSex(name = "campaign3") 

campaign<- "s_2025"
cdm$vaccine_90_s_2025<-cdm$vaccine_camp|>
  requireCampaign(campaign)|>
  compute(name = "vaccine_90_s_2025")

cdm$campaign4 <- cdm$demo3 |>
  requireObs(campaign)|>
  recordCohortAttrition(reason = "In observation")|>
  compute(name = "campaign4")|>
  addVaccinated(cdm$vaccine_90_s_2025) |>
  addDatesCampaignAge(campaign)|>
  addImmunosuppressed() |>
  addAge() |>
  compute(name = "campaign4") |> 
  filter(age >= 75L | immunosuppressed == 1L) |>
  recordCohortAttrition(reason = "Inclusion criteria") |>
  compute(name = "campaign4")|>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addDose(cdm$vaccine_90)|>
  addSex(name = "campaign4") 

cdm <- bind(cdm$campaign1 |> renameCohort("campaign1"), 
            cdm$campaign2 |> renameCohort("campaign2"), 
            cdm$campaign3 |> renameCohort("campaign3"),
            cdm$campaign4 |> renameCohort("campaign4"),
            name = "all_campaigns")

cdm$all_campaigns <- cdm$all_campaigns|>
  addCohortName() |>
  select(-n_dose) |>
  #mutate(dose= if_else(is.na(dose), "O dose", dose)) |>
  mutate(status = if_else(
    is.na(dose),
    "UV",
    paste0("V: ", dose)
  ))|>
  compute(name= "all_campaigns") 

result <- summariseResult(
  table = cdm$all_campaigns, 
  group = "cohort_name",
  strata = combineStrata(c("region", "imd", "sex", "ethnicity")), 
  variables = list(c("vaccinated", "status"), c("cohort_start_date")),
  estimates = list(c("count", "percentage"), c("min", "max", "median", "q25", "q75")))

tidy(result)
exportSummarisedResult(result,   
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("Results"))

result <- importSummarisedResult(path="Results/results_cdm_gold_202507_2026_03_23.csv")
  
tidy(result|>filter(result_id==1))
visOmopResults::visOmopTable(result|>filter(result_id==1), header = c("cohort_name"))
