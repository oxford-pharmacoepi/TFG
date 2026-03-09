# create logger ----
resultsFolder <- here("results")
if(!dir.exists(resultsFolder)){
  dir.create(resultsFolder)
}

createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))
logMessage("LOG CREATED")
library(ParallelLogger)
# run ----
source(here("codelist", "codelist_creation.R"))
source(here("cohorts", "instantiate_cohorts.R"))
logInfo("- Running PhenotypeDiagnostics")
diagnostics <- phenotypeDiagnostics(cdm$all_cohorts,
                          survival = FALSE)

exportSummarisedResult(diagnostics,
                       minCellCount = minCellCount,
                       fileName = "phenotyper_results_{cdm_name}_{date}.csv",
                       path = here("results")
                       )
shinyDiagnostics(result = diagnostics, directory = here(".."), open = TRUE)
runApp()
logMessage("Finished")
