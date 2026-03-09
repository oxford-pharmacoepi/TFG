# renv::activate()
# renv::restore()

library(DBI)
library(dplyr)
library(here)
library(CDMConnector)
library(omopgenerics)
library(OmopSketch)
library(CodelistGenerator)
library(CohortConstructor)
library(PatientProfiles)
library(CohortCharacteristics)
library(DrugUtilisation)
library(IncidencePrevalence)
library(CohortSurvival)
library(odbc)
library(RPostgres)
library(modeltools)

# database metadata and connection details
# The name/ acronym for the database
dbName <- Sys.getenv("DB_NAME")

# Database connection details
# db <- dbConnect(
#   RPostgres::Postgres(),
#   dbname = server_dbi,
#   port = port,
#   host = host,
#   user = user,
#   password = password
# )

db <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = dbName,
                      host = Sys.getenv("DB_HOST"),
                      user = Sys.getenv("DB_USER"),
                      password = Sys.getenv("DB_PASSWORD"))

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_schema <- Sys.getenv("cdm_schema")

# A prefix for all permanent tables in the database
write_prefix <- Sys.getenv("write_schema")

# The name of the schema where results tables will be created
write_schema <- Sys.getenv("write_schema")
achilles_schema <- Sys.getenv("achilles_schema")

# minimum counts that can be displayed according to data governance
min_cell_count <- 5

# Create cdm object ----
cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdm_schema,
  writeSchema = write_schema,
  writePrefix = write_prefix,
  cdmName = dbName,
  achillesSchema = achilles_schema
)

# Run the study
source(here("run_study.R"))

