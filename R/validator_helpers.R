#' @importFrom udunits2 ud.is.parseable

lexists <- function(list,name){
  name %in% names(list)
}

#' Check that element exists in a list
#'
#' @param list list to check
#' @param name element
check_exists <- function(list,name){
  if(lexists(list,name)) usethis::ui_done(paste(name,"found"))
  else usethis::ui_stop(paste("file missing",name))
}

#' Check that an element exists and is UDUNITS2 parsable
#'
#' @param list list to check
#' @param name element
#' @param required whether the element is required or not
check_parsable <- function(list,name,required=TRUE){
  if(lexists(list,name)){
    if(udunits2::ud.is.parseable(list[[name]])){
      usethis::ui_done(paste(name,"parsable"))
    } else {
      usethis::ui_stop(paste(name,"not parsable"))
    }
  } else if(required) usethis::ui_stop(paste("file missing",name))
}

#' Check that an element exists and is a whole number
#'
#' @param list list to check
#' @param name element
#' @param required whether the element is required or not
check_whole <- function(list,name,required=TRUE){
  if(lexists(list,name)){
    if(is.whole(list[[name]])){
      usethis::ui_done(paste(name,"valid"))
    } else {
      usethis::ui_stop(paste(name,list[[name]],"not valid, must be a whole number"))
    }
  } else if(required) usethis::ui_stop(paste("file missing",name))
}


#' Check that a number is whole
#'
#' @param x variable
is.whole <- function(x){
  x = as.numeric(x)
  is.numeric(x) && floor(x)==x
}