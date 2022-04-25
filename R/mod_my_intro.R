#' my_intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_intro_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' my_intro Server Functions
#'
#' @noRd 
mod_my_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_my_intro_ui("my_intro_ui_1")
    
## To be copied in the server
# mod_my_intro_server("my_intro_ui_1")
