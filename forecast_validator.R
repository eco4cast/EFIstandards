lexists <- function(list,name){
  name %in% names(list)
}

check_exists <- function(list,name){
  if(lexists(list,name)) usethis::ui_done(paste(name,"found"))
  else usethis::ui_stop(paste("file missing",name))
}

check_parsable <- function(list,name,required=TRUE){
  if(lexists(list,name)){
    if(udunits2::ud.is.parseable(list[[name]])){
      usethis::ui_done(paste(name,"parsable"))
    } else {
      usethis::ui_stop(paste(name,"not parsable"))
    }
  } else if(required) usethis::ui_stop(paste("file missing",name))
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

  check_exists(meta,"additionalMetadata")
  check_exists(meta$additionalMetadata,"metadata")
  check_exists(meta$additionalMetadata$metadata,"forecast")
  AM <- meta$additionalMetadata$metadata$forecast

  ## Check REQUIRED CORE EML elements


  ## Check REQUIRED FORECAST elements

  check_parsable(AM,"forecast_horizon")

  # check uncertainty classes exist
  check_exists(AM,"uncertainty")
  UQ <- AM$uncertainty
  UQclass <- c("initial_conditions","parameters","drivers","process_error","random_effects")
  for(i in seq_along(UQclass)){
    check_exists(UQ,UQclass[i])
  }
  UQbool <- tolower(names(UQ)) %in% UQclass
  if(!all(UQbool)) usethis::ui_stop(paste("invalid uncertainty class",names(UQ[!UQbool])))

  # check uncertainty class attribute values are valid
  UQ <- tolower(as.character(unlist(UQ)))
  names(UQ) <- names(AM$uncertainty)
  UQoptions <- c("no","contains","data_driven","propagates","assimilates")
  UQbool <- UQ %in% UQoptions
  if(any(is.na(UQbool))|any(is.null(UQbool))) usethis::ui_stop(paste("NA or NULL uncertainty value",names(UQ[!UQbool])))
  if(all(UQbool)) usethis::ui_done("All uncertainty class attributes valid")
  else usethis::ui_stop(paste("invalid uncertainty class attribute",names(UQ[!UQbool])))

  UQnum <- match(UQ,UQoptions)

  ## Check CONDITIONALLY REQUIRED FORECAST elements

  #complexity
  if(any(UQnum>1)){
    check_exists(AM,"complexity")
    UQname <- sort(names(UQ[UQnum>1]))
    Cname  <- sort(names(AM[["complexity"]]))
    if(all(UQname == Cname)) usethis::ui_done("All complexity class matched")
    else usethis::ui_stop(paste0("invalid complexity classes [",paste(Cname,collapse = ", "),
                                "] do not match uncertainty classes: ",paste(UQname,collapse=", ")))
## add is.integer and positive check
  }

  #propagation_method
  if(any(UQnum>3)){
    check_exists(AM,"propagation_method")
    check_exists(AM[["propagation_method"]],"type")
## add check for valid type
    if(tolower(AM[["propagation_method"]][["type"]]) == "ensemble"){
      check_exists(AM[["propagation_method"]],"size")
## add is.integer and positivecheck
    }
  }

  #assimilation_method
  if(any(UQnum>4)){
    check_exists(AM,"assimilation_method")
  }


  ## Check OPTIONAL FORECAST elements

  # timestep parsable (considering this flag optional until we sort out whether it can be stored in the main metadata)
  check_parsable(AM,"timestep",required = FALSE)

  valid
}


forecast_validator("forecast-eml.xml")
