omopgenerics::assertNumeric(min_cell_count)

# Create a log file ----
createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))
logMessage("LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date(NA), as.Date(NA))

# Initialise list to store results as we go -----
results <- list()

# CDM modifications -----

# CDM summary -----
results[["snapshot"]] <- summariseOmopSnapshot(cdm)
results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)

# Instantiate study cohorts ----
logMessage("Instantiating study cohorts")
source(here("codelist", "codelist_creation.R"))
source(here("cohorts", "instantiate_cohorts.R"))
logMessage("Study cohorts instantiated")

# Cohort counts and attrition ----
# results[["counts"]] <- summariseCohortCount("...")
results[["attrition"]] <- summariseCohortAttrition(cdm$vaccine_camp_fin)

# Run analyses ----
logMessage("Run study analyses")
source(here("analyses", "cohort_characteristics.R"))
logMessage("Analyses finished")

# Capture log file ----
results[["log"]] <- summariseLogFile(cdmName = omopgenerics::cdmName(cdm))

# Finish ----
results$largeScale <- LargeScaleCharacteristics
results$characterisation <- characterisation

results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("Results"))
#write.csv(x_dose, "Results/plot_dose.csv", row.names = FALSE)
cli::cli_alert_success("Study finished")

