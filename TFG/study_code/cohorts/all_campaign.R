cdm$demo <- demographicsCohort(
    cdm, 
    ageRange = NULL, 
    sex = NULL,
    minPriorObservation = NULL, # cohort_start_date will start one year later if we put 365
    name = "demo"
)

cdm$demo0 <- cdm$demo |>compute(name="demo0")

campaign<- "a_2023"
cdm$vaccine_90_a_2023<-cdm$vaccine_camp|>
  requireCampaign(campaign)|>
  compute(name = "vaccine_90_a_2023")

cdm$campaign1 <- cdm$demo |>
  requireObs(campaign)|>
  compute(name = "campaign1")|>
  recordCohortAttrition(reason = "In observation")|>
  addVaccinated(cdm$vaccine_90_a_2023) |>
  addDatesCampaignAge(campaign)|>
  addImmunosuppressed() |>
  addAge() |>
  compute(name = "campaign1") |> 
  filter(age >= 65L | immunosuppressed == 1L) |>
  recordCohortAttrition(reason = "Inclusion criteria") |>
  compute(name = "campaign1")|>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addDose(cdm$vaccine_90)|>
  addSex(name = "campaign1") 

campaign<- "s_2024"
cdm$vaccine_90_s_2024<-cdm$vaccine_camp|>
  requireCampaign(campaign)|>
  compute(name = "vaccine_90_s_2024")

cdm$campaign2 <- cdm$demo |>
  requireObs(campaign)|>
  recordCohortAttrition(reason = "In observation")|>
  compute(name = "campaign2")|>
  addVaccinated(cdm$vaccine_90_s_2024) |>
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
requireCampaign(campaign)|>
  compute(name = "vaccine_90_a_2024")

cdm$campaign3 <- cdm$demo |>
  requireObs(campaign)|>
  compute(name = "campaign3")|>
  recordCohortAttrition(reason = "In observation")|>
  compute(name = "campaign3")|>
  addVaccinated(cdm$vaccine_90_a_2024) |>
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

cdm$campaign4 <- cdm$demo |>
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

cdm <- bind(cdm$campaign1, 
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


  
