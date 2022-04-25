#' raw_mouse_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_raw_mouse_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' raw_mouse_data Server Functions
#'
#' @noRd 
mod_raw_mouse_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_raw_mouse_data_ui("raw_mouse_data_ui_1")
    
## To be copied in the server
# mod_raw_mouse_data_server("raw_mouse_data_ui_1")
