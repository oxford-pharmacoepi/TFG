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
source(here("codelist", "codelist_reading.R")) 
source(here("cohorts", "functions.R"))
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
results[["attrition"]] <- summariseCohortAttrition(cdm$vaccinated_within_campaigns)

# Run analyses ----
logMessage("Run study analyses")
source(here("analyses", "vaccine_characteristics.R"))
logMessage("Analyses finished")

# Capture log file ----
results[["log"]] <- summariseLogFile(cdmName = omopgenerics::cdmName(cdm))

# Finish ----
results$largeScale <- LargeScaleCharacteristics
results$characterisation <- characterisation
results$characterisation_a <- characterisation_a

results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("Results"))

# Results to save as csv and plot ----
#source(here("analyses", "docs_to_plot.R"))
#write.csv(x_90, "Results/plot_90.csv", row.names = FALSE)
#write.csv(x_dose, "Results/plot_dose.csv", row.names = FALSE)

cli::cli_alert_success("Study finished")

