lexists <- function(list,name){
  name %in% names(list)
}

forecast_validator <- function(eml){

  meta <- EML::read_eml(eml)

  ##
  usethis::ui_todo("Checking Validity of EML file...")

  valid <- EML::eml_validate(eml)

  if(valid) usethis::ui_done("EML is valid")
  else usethis::ui_oops(paste("EML is not valid",                    ## revert to ui_stop once sort out additionalMetadata$describes
                              "found the following problems:\n",
                              attr(valid, "errors")))


  ## Check that additonalMetadata exists
  if(lexists(meta,"additionalMetadata")) usethis::ui_done("EML contains additionalMetadata")
  else usethis::ui_stop("file missing additionalMetadata")

  if(lexists(meta$additionalMetadata,"metadata")) usethis::ui_done("additionalMetadata contains `metadata`")
  else usethis::ui_stop("file missing additionalMetadata$metadata")

  AM <- meta$additionalMetadata$metadata

  ## Check REQUIRED elements


  ## Check OPTIONAL elements

  # timestep parsable (considering this flag optional until we sort out whether it can be stored in the main metadata)
  if(lexists(AM,"timestep")){
    if(udunits2::ud.is.parseable(AM$timestep)) usethis::ui_done("timestep parsable")
    else usethis::ui_stop("timestep not parsable")
  }

  valid
}


forecast_validator("forecast-eml.xml")
