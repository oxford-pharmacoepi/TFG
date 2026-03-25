omopgenerics::assertNumeric(min_cell_count)

# Create a log file ----
createLogFile(logFile = here("Results", "log_{date}_{time}"))
logMessage("LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date(NA), as.Date(NA))

# Initialise list to store results as we go -----
results <- list()

# CDM modifications -----

# CDM summary -----
logMessage("Extract CDM snapshot") 
results[["snapshot"]] <- summariseOmopSnapshot(cdm)

logMessage("Extract observation period summary") 
results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)

# Instantiate study cohorts ----
logMessage("Instantiating study cohorts")
codelist <- importCodelist("codelist", type = "csv")
source(here("functions.R"))
logMessage("Codelists and functions to be used imported")

source(here("cohorts", "instantiate_cohorts.R")) 
logMessage("Vaccinated people identified by campaign")

source(here("cohorts", "vaccine_cohorts.R"))
logMessage("Vaccinated people within the vaccination campaigns of interest -either for being immunosuppressed or by age- 
           stratified by age, ethnicity, IMD, sex and region. Will be used for overall attrition") 

source(here("cohorts", "all_campaign.R")) 
logMessage("Eligibles for each of the vaccination campaigns -either for being immunosuppressed or by age- 
           stratified by age, ethnicity, IMD, sex and region. Will be used for coverage")  
logMessage("Study cohorts instantiated")

# Cohort counts and attrition ----
results[["attrition_vaccinated"]] <- summariseCohortAttrition(cdm$vaccinated_within_campaigns)
results[["attrition_campaign1"]] <- summariseCohortAttrition(cdm$campaign1)

# Run analyses ----
logMessage("Run study analyses")
source(here("analyses", "vaccine_characteristics.R"))
logMessage("Analyses for the vaccinated people stratified for all campaigs, 
           where the 2 dosis filter isn't considered DONE")

logMessage("Analyses finished")

# Capture log file ----
results[["log"]] <- summariseLogFile(cdmName = omopgenerics::cdmName(cdm))

# Finish ----
results$characterisation <- characterisation


results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("Results"))

# Results to save as csv and plot ----
# Save data for the local plots of the vaccination chronology 
#(see "vaccination_chronology" for more info). Should be computed once
#source(here("analyses", "vaccination_chronology.R"))
write.csv(x_dose, "Results/plot_dose.csv", row.names = FALSE)

cli::cli_alert_success("Study finished")

