# Vaccinated people within the vaccination campaigns of interest -either for being immunosuppressed or by age- 
#stratified by age, ethnicity, IMD, sex and region. Will be used for overall attrition
cdm$vaccinated_within_campaigns <-cdm$vaccine_90 |>
  #for sensitivity analysis
  #requireInDateRange(dateRange =c(NA, "2021-01-01"), name = "vaccinated_within_campaigns")|>
  addCampaigns(name = "vaccinated_within_campaigns") |>
  filter(vaccination_campaign != "None")|>
  compute(name = "vaccinated_within_campaigns")|>
  recordCohortAttrition(reason = "Vaccinated within campaigns of interest") |>
  compute(name = "vaccinated_within_campaigns")|>
  addImmunosuppressed() |>
  addAge() |> 
  filter(if_else(vaccination_campaign == "a_2023", 
                 age >= 75L | immunosuppressed == 1L, 
                 age >= 65L | immunosuppressed == 1L)) |>
  compute(name = "vaccinated_within_campaigns")|>
  recordCohortAttrition(reason = "Inclusion criteria by age and immunosuppresion") |>
  compute(name = "vaccinated_within_campaigns")|>
  addRegion() |>
  addIMD() |>
  addEthnicity() |>
  addSex(name = "vaccinated_within_campaigns")
