cdm$covid <- conceptCohort(cdm = cdm,
                           conceptSet = list("covid" = diagnosis),
                           name = "covid"
                           )
cdm$measurement |> 
  filter(measurement_concept_id %in% c(test, 4326835L)) |>
  distinct(value_as_number, value_as_concept_id)|>head(10)
cdm$concept|>
  filter(concept_id %in% c(9190, 4126681)) |>
  select(concept_id, concept_name, domain_id, vocabulary_id)|>head(10)

cdm$test_positive <- measurementCohort(cdm = cdm,
                                       conceptSet = list("test" = test),
                                       name = "test_positive", 
                                       valueAsConcept = list("test_positive" = c(4126681))
                                      )

cdm$vaccine <- conceptCohort(cdm = cdm,
                             conceptSet = list(
                               "vaccine_record" =
                                 vac),
                             name = "vaccine"
)

cdm$immun <- conceptCohort(cdm = cdm,
                             conceptSet = list(
                               "immunosupressed" =
                                 immun),
                             name = "immun"
)

summariseOrphanCodes(
  codelist$immun,
  cdm,
  domain = c("condition", "device", "drug", "measurement", "observation", "procedure",
             "visit")
            )
# Result:A tibble: 0 × 13

cdm<- bind(
  cdm$covid,
  cdm$vaccine,
  cdm$immun,
  cdm$test_positive,
  name = "all_cohorts"
)


