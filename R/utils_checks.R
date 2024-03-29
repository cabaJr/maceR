
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
#' @export
#'

checkPlots <- function(App_settings){
    App_settings$plotTab$tab <- TRUE
  } 

#' check_plot_tab
#'
#' @param App_setting env App_settings
#' @param activate tab to activate
#'
#' @export
#'
check_plot_tab <- function(App_setting, activate){
  activate <- TRUE
}


#' subset_input_check
#'
#' @param idlist list of selected animals for subsetting
#'
#' @return an error notification
#' @export
#'
subset_input_check <- function(idlist){
  input_error <- dplyr::case_when(
    !is.null(idlist) ~ "You need to select some animals",
    TRUE ~ ""
  )
  if (input_error != "") {
    showModal(modalDialog(
      title = "input_error",
      input_error,
      easyClose = TRUE
    ))
    return() # exit the function here
  }
}

## add check functions to generate tables
