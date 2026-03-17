cdm$demo <- demographicsCohort(
  cdm, 
  ageRange = NULL, 
  sex = NULL,
  minPriorObservation = NULL, # cohort_start_date will start one year later if we put 365
  name = "demo"
)

#probes
cdm$demo2<-cdm$demo|>
  addRegion()|>
  compute(name="demo2") |>
  recordCohortAttrition(reason ="demo")|>
  filter(region=="Wales")|>
  recordCohortAttrition(reason ="Wales")|>filter(cohort_end_date=="2025-05-15")|>
  recordCohortAttrition(reason ="date")|>
  compute(name="demo2")