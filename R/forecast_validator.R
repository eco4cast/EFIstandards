#' EFI forecast standard EML metadata validator
#'
#' @param eml EML file path
#'
#' @return boolean
#' @export
#' @importFrom usethis ui_todo ui_done ui_stop
#' @importFrom EML read_eml eml_validate
#' @examples
#'\donttest{
#' forecast_validator(system.file("extdata", "forecast-eml.xml", package="EFIstandards"))
#' forecast_validator(system.file("extdata", "beetles-eml.xml", package="EFIstandards"))
#'}
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

  ## Check that forecast metadata exists. Can be in any additionalMetadata element
  check_exists(meta,"additionalMetadata")
  AM <- extract_forecast_metadata(meta)

  ## Check REQUIRED CORE EML elements

  ## Check REQUIRED FORECAST elements

  check_parsable(AM,"timestep")
  check_parsable(AM,"forecast_horizon")
  check_exists(AM,"forecast_issue_time")  ## need add check that this is a valid ISO datatime
  check_exists(AM,"forecast_iteration_id")
  check_exists(AM,"forecast_project_id")
  check_exists(AM,"metadata_standard_version")
  check_exists(AM,"model_description")
  check_exists(AM$model_description,"forecast_model_id")
  check_exists(AM$model_description,"name")
  check_exists(AM$model_description,"type")
  check_exists(AM$model_description,"repository")


  UQclass <- c(
    "initial_conditions",
    "parameters",
    "drivers",
    "random_effects",
    "process_error",
    "obs_error"
  )

  ## Check STATUS & UNCERTAINTY CLASS elements
  validate_uqclass <- function(parent, element) {
    check_exists(parent, element)
    uqlist <- parent[[element]]

    ## Check UNCERTAINTY tag
    check_exists(uqlist, "status")
    uqunc <- uqlist[["status"]]
    UQoptions <- c("absent", "present", "data_driven", "propagates", "assimilates")
    if (!uqunc %in% UQoptions) {
      usethis::ui_stop(sprintf(
        "Invalid status/uncertainty class '%s'",
        uqlist[["status"]]
      ))
    } else{
      usethis::ui_done(paste0(element," status/uncertainty class valid: ",uqlist[["status"]]))
    }

    ## Check CONDITIONALLY DEPENDENT tags
    uqunc_f <- factor(uqunc, UQoptions, ordered = TRUE)

    if (uqunc_f >= "present") {
      # Check complexity
      check_whole(uqlist,"complexity",required = FALSE)


      ## ADD special cases for process_error  [[ CURRENTLY SET TO RECOMMENDED]]
       if(element == "process_error"){
      #   check_exists(uqlist,"covariance")
         if(lexists(uqlist,"covariance")){
           if(uqlist[["covariance"]]){     ## if TRUE (full cov matrix), check for localization
      #       check_exists(uqlist,"localization")
           }
         }
       }
    }
    if (uqunc_f >= "propagates") {
      # Check propagation method
      #check_exists(uqlist,"propagation")  ## RECOMMENDED
      if(lexists(uqlist,"propagation")){
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
      } ## end propagation
    } ## end propagates

    if (uqunc_f >= "assimilates") {
      # Check assimilation method
#      check_exists(uqlist,"assimilation")  ## RECOMMENDED
      if(lexists(uqlist,"assimilation")){
        alist <- uqlist[["assimilation"]]

        ## type
        check_exists(alist,"type")

        ## reference
        check_exists(alist,"reference")

        ## complexity
        check_whole(alist,"complexity",required = FALSE)

        ## attributeName (optional)
        check_whole(alist,"attributeName",required = FALSE)
      }
    }
  }

  for (UQc in UQclass) validate_uqclass(AM, UQc)

  ## Check OPTIONAL FORECAST elements


  ## return status
  valid
}


extract_forecast_metadata <- function(meta){

  if(!is.null(names(meta$additionalMetadata)))
    return(meta$additionalMetadata$metadata$forecast)

  i  <- vapply(meta$additionalMetadata,
               function(x) names(x$metadata)[[1]] == "forecast", logical(1L))
  AM <- meta$additionalMetadata[i]
  AM[[1]]$metadata$forecast
}



#forecast_validator("forecast-eml.xml")
