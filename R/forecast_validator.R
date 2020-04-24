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

check_whole <- function(list,name,required=TRUE){
  is.whole <- function(x){
    x = as.numeric(x)
    is.numeric(x) && floor(x)==x
  }
  if(lexists(list,name)){
    if(is.whole(list[[name]])){
      usethis::ui_done(paste(name,"valid"))
    } else {
      usethis::ui_stop(paste(name,list[[name]],"not valid, must be a whole number"))
    }
  } else if(required) usethis::ui_stop(paste("file missing",name))
}


#' EFI forecast standard EML metadata validator
#'
#' @param eml
#'
#' @return
#' @export
#'
#' @examples
#'
#' forecast_validator("forecast-eml.xml")
#'
forecast_validator <- function(eml){

  meta <- EML::read_eml(eml)

  ##
  usethis::ui_todo("Checking Validity of EML file...")

  valid <- EML::eml_validate(eml)

  if(valid) {
    usethis::ui_done("EML is valid")
  }else{
    usethis::ui_stop(paste("EML is not valid",
                           "found the following problems:\n",
                           attr(valid, "errors")))
  }

  ## Check that additonalMetadata exists

  check_exists(meta,"additionalMetadata")
  check_exists(meta$additionalMetadata,"metadata")
  check_exists(meta$additionalMetadata$metadata,"forecast")
  AM <- meta$additionalMetadata$metadata$forecast

  ## Check REQUIRED CORE EML elements


  ## Check REQUIRED FORECAST elements

  check_parsable(AM,"forecast_horizon")

  UQclass <- c(
    "initial_conditions",
    "parameters",
    "drivers",
    "process_error",
    "random_effects"
  )

  ## Check UNCERTAINTY CLASS elements
  validate_uqclass <- function(parent, element) {
    check_exists(parent, element)
    uqlist <- parent[[element]]

    ## Check UNCERTAINTY tag
    check_exists(uqlist, "uncertainty")
    uqunc <- uqlist[["uncertainty"]]
    UQoptions <- c("no", "contains", "data_driven", "propagates", "assimilates")
    if (!uqunc %in% UQoptions) {
      usethis::ui_stop(sprintf(
        "Invalid uncertainty class '%s'",
        uqlist[["uncertainty"]]
      ))
    } else{
      usethis::ui_done(paste0(element," uncertainty class valid: ",uqlist[["uncertainty"]]))
    }

    ## Check CONDITIONALLY DEPENDENT tags
    uqunc_f <- factor(uqunc, UQoptions, ordered = TRUE)

    if (uqunc_f >= "contains") {
      # Check complexity
      check_whole(uqlist,"complexity")

      ## ADD special cases for process_error
        # covariance
        # localization
    }
    if (uqunc_f >= "propagates") {
      # Check propagation method
      check_exists(uqlist,"propagation")
      plist <- uqlist[["propagation"]]

      ## type
      check_exists(plist,"type")
      if (!tolower(plist[["type"]]) %in% c("ensemble","analytic")) {
        usethis::ui_stop(sprintf(
          "'%s' Invalid uncertainty <propagation> <type> '%s'",
          element,plist[["type"]]
        ))
      } else{
        usethis::ui_done(paste0(element," propagation type valid: ",plist[["type"]]))
      }

      ## ensemble size
      if(tolower(plist[["type"]]) == "ensemble"){
        check_whole(plist,"size")
      } else {

      ## ADD check on analytic <method>
      }
    }

    if (uqunc_f >= "assimilates") {
      # Check assimilation method
      check_exists(uqlist,"assimilation")

      ## ADD DETAIL HERE

    }
  }

  for (UQc in UQclass) validate_uqclass(AM, UQc)

  ## Check OPTIONAL FORECAST elements

  # timestep parsable (considering this flag optional until we sort out whether it can be stored in the main metadata)
  check_parsable(AM,"timestep",required = FALSE)

  valid
}


forecast_validator("forecast-eml.xml")
