#' new_experiment_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_new_experiment_ui_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboardPlus::dashboardPage(
      options = list(sidebarExpandOnHover = TRUE),
      header = shinydashboardPlus::dashboardHeader(title = "MACE", 
                                                   leftUi = tagList(
                                                     shinydashboardPlus::dropdownBlock(id = "mydropdown",
                                                                                       title = "Drop_1",
                                                                                       sliderInput(inputId = "numb", label = "input a number", min = 1, max = 20, value = 5),
                                                                                       # icon = shiny::icon("gears"), 
                                                                                       sliderInput(inputId = "numb2", label = "input a number", min = 10, max = 200, value = 50),
                                                                                       actionButton(inputId = "none", label = "trythis", icon = NULL)
                                                     )
                                                   ),
                                                   controlbarIcon = shiny::icon("gears")),#img(src = "hex-MACE.png")),
      sidebar = shinydashboardPlus::dashboardSidebar(minified = TRUE, collapsed = FALSE,
                                                     shinydashboard::sidebarMenu(#id = "mainmenu",
                                                       shinydashboard::menuItem("Input", tabName = "inputDF", icon = icon("upload")),#, text = "Input", tabName = "inputDF"),
                                                       shinydashboard::menuItem("Data Structure", tabName = "DataStructure", icon = icon("filter")),# text = "Data structure", tabName = "DataStructure"),
                                                       shinydashboard::menuItem(icon = icon("database"), text = "Your Data", tabName = "YourData"),
                                                       shinydashboard::menuItem(icon = icon("sliders-h"), text = "Analysis", tabName = "Analysis"),
                                                       shinydashboard::menuItem(icon = icon("chart-pie"), text = "Plots", tabName = "Plots")
                                                     )
      ),
      body =  shinydashboard::dashboardBody( ),
    )
  )
}
    
#' new_experiment_ui Server Functions
#'
#' @noRd 
mod_new_experiment_ui_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_new_experiment_ui_ui("new_experiment_ui_ui_1")
    
## To be copied in the server
# mod_new_experiment_ui_server("new_experiment_ui_ui_1")
