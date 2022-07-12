
#' @title check_uploads
#' @description Function to check uploaded files
#' @details Functions that returns TRUE if both data and metadata have been 
#' uplooaded, otherwise FALSE.
#' @param App_settings passed to the functions to access values stored in the 
#' App_settings object.
#' @return TRUE or FALSE
#'
#' @noRd

check_uploads <- function(App_settings){
  if(is.null(App_settings$dataList) == FALSE && is.null(App_settings$metadata) == FALSE){
    check <- TRUE
  } else {check <- FALSE}
  return(check)
}

#' checkPlots
#' 
#' @description function to check if Plot TabItem is already present, and if
#'     not it renders the Plots tab.
#' @param App_settings environment
#'
#' @return
#' @export
#'

checkPlots <- function(App_settings){
    App_settings$plotTab <- TRUE
  } 
# }
## add check functions to generate tables
