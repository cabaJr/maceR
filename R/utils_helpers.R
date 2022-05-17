#' show_help 
#' @param id id of the helper message to show
#' @description function to show helper messages
#'
#' @return the text of the helper message
#'
#' @noRd

show_help <- function(App_settings, id){
  showModal(modalDialog(App_settings$env_msg$Messages$help[id], title = "Help", easyClose = TRUE))
}