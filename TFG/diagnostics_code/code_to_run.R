
# Run lines below to use renv
# renv::activate()
# renv::restore()

library(CDMConnector)
library(DBI)
library(dplyr)
library(here)
library(OmopSketch)
library(omopgenerics)
library(CohortConstructor)
library(PhenotypeR)
library(CohortCharacteristics)
library(PatientProfiles)
library(stringr)
library(CodelistGenerator)
library(odbc)
library(RPostgres)
library(readr)
library(purrr)
library(modeltools)

# database metadata and connection details
# The name/ acronym for the database
dbName <- Sys.getenv("DB_NAME")

# Database connection details
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html
# for more details.
# you may need to install another package for this
# eg for postgres
# db <- dbConnect(
#   RPostgres::Postgres(),
#   dbname = server_dbi,i
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

# The name of the schema where the achilles tables are
achilles_schema <- Sys.getenv("achilles_schema")

# minimum counts that can be displayed according to data governance
minCellCount <- 5

# Create cdm object ----
cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdm_schema,
  writeSchema = write_schema,
  writePrefix = write_prefix,
  cdmName = dbName,
  achillesSchema = achilles_schema
)

# Run study ----
source(here("run_study.R"))


