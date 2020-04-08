

forecast_validator <- function(eml){

  meta <- EML::read_eml(eml)

  ##
  usethis::ui_todo("Checking Validity of EML file...")

  valid <- EML::eml_validate(eml)

  if(valid) usethis::ui_done("EML is valid")
  else usethis::ui_stop(paste("EML is not valid",
                              "found the following problems:\n",
                              attr(valid, "errors")))


  ## Check for additional required elements:



  valid
}


forecast_validator("forecast-eml.xml")

