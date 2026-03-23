# codes <- omopgenerics::importConceptSetExpression(path = "new_codelists", type = "csv")
# codelists <- CodelistGenerator::asCodelist(codes, cdm = cdm)
# exportCodelist(
#   codelists, "codelist",
#   type = "csv"
# )

codelist <- importCodelist("codelist", type = "csv")
diagnosis <- unique(codelist$covid_diagnosis_broad)
test <- unique(c(codelist$covid_test_antibody, codelist$covid_test))
immun <- unique(c(codelist$hiv_aids, 
                  codelist$cancerexcludnonmelaskincancer,
                  codelist$scid,
                  codelist$intrinsec_immune,
                  
                  codelist$inmmunos_antineo,
                  codelist$immunos_antineo_exclude,
                
                  codelist$syst_corticosteriods,
                  codelist$transplant
                  )
               )
vac <- unique(codelist$covid_vaccine)

