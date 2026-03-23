omopgenerics::assertNumeric(min_cell_count)

# Create a log file ----
createLogFile(logFile = here("Results", "log_{date}_{time}")))
logMessage("LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date(NA), as.Date(NA))

# Initialise list to store results as we go -----
results <- list()

# CDM modifications -----

# CDM summary -----
logMessage("Extract CDM snapshot") # MC
results[["snapshot"]] <- summariseOmopSnapshot(cdm)

logMessage("Extract observation period summary") # MC
results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)

# Instantiate study cohorts ----
logMessage("Instantiating study cohorts")
source(here("codelist", "codelist_creation.R")) # MC the code seems more to read the codelists than to create them no?
source(here("cohorts", "functions.R"))
source(here("cohorts", "instantiate_cohorts.R"))
source(here("cohorts", "all.R")) # MC does not exist
logMessage("Study cohorts instantiated")

# Cohort counts and attrition ----
results[["attrition"]] <- summariseCohortAttrition(cdm$vaccine_camp_fin)
results[["attrition_a"]] <- summariseCohortAttrition(cdm$all_denom)

# Run analyses ----
logMessage("Run study analyses")
source(here("analyses", "vaccine_characteristics.R"))
source(here("analyses", "all_characteristics.R"))
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

