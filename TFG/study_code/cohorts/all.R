cdm$all <- demographicsCohort(
    cdm, 
    ageRange = NULL, 
    sex = NULL,
    minPriorObservation = 365,
    name = "all"
) 


cdm$all_d <- cdm$all |> 
  left_join(imd, by="subject_id")|> 
  left_join(
    get_regions, by="subject_id"  
  ) |>
  addSex()|>
  left_join(
  eth, by="subject_id"  
  )
compute(name="all_d")
