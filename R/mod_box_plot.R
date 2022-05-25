#' box_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_box_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    
 shinydashboardPlus::box(
   title = "Closable Box with dropdown", 
   closable = TRUE, 
   width = NULL,
   status = "warning", 
   solidHeader = FALSE, 
   collapsible = TRUE,
   enable_dropdown = TRUE,
   dropdown_icon = "wrench",
   # dropdown_menu = dropdownItemList(
   #   dropdownItem(url = "http://www.google.com", name = "Link to google"),
   #   dropdownItem(url = "#", name = "item 2"),
   #   dropdownDivider(),
   #   dropdownItem(url = "#", name = "item 3")
   # ),
   div("plot")
 )
 
  )
}
    
#' box_plot Server Functions
#'
#' @noRd 
mod_box_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_box_plot_ui("box_plot_ui_1")
    
## To be copied in the server
# mod_box_plot_server("box_plot_ui_1")
