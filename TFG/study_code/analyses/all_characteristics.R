characterisation_a <- cdm$all_denom |>
  summariseCharacteristics(
    tableIntersectCount = list(
      "Number visits in the prior year" = list(
        tableName = "visit_occurrence",
        window = c(-365, -1)
      )
    ),
    otherVariables = c("region", "ethnicity", "sex", "imd")
  )
