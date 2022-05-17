#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    
    shinydashboardPlus::dashboardPage(
      options = list(sidebarExpandOnHover = TRUE),
      header = shinydashboardPlus::dashboardHeader(title = "MACE", 
                                                   disable = App_settings$public_fields$dashboardHeader_appearance),
      title = "MACE",
      sidebar = shinydashboardPlus::dashboardSidebar(disable = FALSE, 
                                                     minified = TRUE, 
                                                     collapsed = TRUE,
                     shinydashboard::sidebarMenu(id = "mainmenu",
                                                 shinydashboard::menuItemOutput(outputId = "Home"),
                                                 shinydashboard::menuItemOutput(outputId = "InputDF"),
                                                 shinydashboard::menuItemOutput(outputId = "DataStructure"),
                                                 shinydashboard::menuItemOutput(outputId = "YourData"),
                                                 shinydashboard::menuItemOutput(outputId = "Analysis"),
                                                 shinydashboard::menuItemOutput(outputId = "Plots")
                     )
      ),
      body =  shinydashboard::dashboardBody(
        # initialize shinyjs
        shinyjs::useShinyjs(),
        # custom shinyjs code. to be included in a script elsewhere
        shinyjs::extendShinyjs(text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }", functions = c("hidehead")),
        # start of tabItems
        shinydashboard::tabItems(
          #Home tab 
          shinydashboard::tabItem(tabName = "Home", ##add CSS to change the appearance of the landing page
                                  mod_landing_page_ui("landing_page_ui_1")
                                  ), #End of Home tab
          #### Input ####
          shinydashboard::tabItem(tabName = "InputDF",
                                  mod_Input_DF_ui("Input_DF_ui_1")
                                  ), #endInputDF
          #### Datastructure ######
          shinydashboard::tabItem(tabName = "DataStructure",
                                  mod_data_structure_ui("data_structure_ui_1")
                                  ), #endDataStructure
          #### YourData ####
          shinydashboard::tabItem(tabName = "YourData",
                                  mod_your_data_ui("your_data_ui_1")
                                  ), #endYourData
          #### Analysis ####
          shinydashboard::tabItem(tabName = "Analysis",
                                  mod_analysis_ui("analysis_ui_1")
                                  ), #endAnalysis
          #### Plots ######
          shinydashboard::tabItem(tabName = "Plots"
                                  # mod_plots_ui("plots_ui_1")
                                  ) #endPlots
        ) #end Tab Items
      ), #end dashboardBody
    )
  ) 
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'MACE'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

