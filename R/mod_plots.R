#' plots UI Function
#'
#' @description Shiny Module to display the plots generated upon user request.
#'     Each plot type is displayed in a separate tab. Tabs for different plots
#'     are made available after the app has loaded the plot.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tabsetPanel(
      id = "plot_panels",
      tabPanel(
        title = "Actogram", #id = ns("Actogram"),
        fluidRow(
          shiny::br(),
          uiOutput(ns("DPacto1")),
          uiOutput(ns("DPacto2")),
          uiOutput(ns("DPacto3")),
          uiOutput(ns("DPacto4")),
          uiOutput(ns("DPacto5"))
        )
      ),#end DP actogram panel
      tabPanel(
        title = "Single line actogram", #id = ns("Single line actogram"),
        fluidRow(
          shiny::br(),
          uiOutput(ns("acto1")),
          uiOutput(ns("acto2")),
          uiOutput(ns("acto3")),
          uiOutput(ns("acto4"))
        ),
      ),#end actogram panel
      tabPanel(
        title = "Daily activity",# id = ns("Daily activity"),
        fluidRow(
          shiny::br(),
          uiOutput(ns("DAct1")),
          uiOutput(ns("DAct2")),
          uiOutput(ns("DAct3")),
          uiOutput(ns("DAct4")),
          uiOutput(ns("DAct5")),
          uiOutput(ns("DAct6"))
        )
      ),#end Daily Activity panel
      tabPanel(
        title = "Periodogram",# id = ns("Periodogram"),
        fluidRow(
          shiny::br(),
          uiOutput(ns("Per1")),
          uiOutput(ns("Per2")),
          uiOutput(ns("Per3")),
          uiOutput(ns("Per4")),
          uiOutput(ns("Per5"))
        )
      ),#end Periodogram Panel 
      tabPanel(
        title = "Period boxplots",
        fluidRow(
          shiny::br(),
          uiOutput(ns("Per1_boxplot")),
          uiOutput(ns("Per2_boxplot")),
          uiOutput(ns("Per3_boxplot")),
          uiOutput(ns("Per4_boxplot")),
          uiOutput(ns("Per5_boxplot"))
        )
      ),#end Periodogram Panel 
      tabPanel(
        title = "Average day",# id = ns("Average day"),
        fluidRow(
          shiny::br(),
          uiOutput(ns("AvgDay1")),
          uiOutput(ns("AvgDay2")),
          uiOutput(ns("AvgDay3"))
          # uiOutput(ns("AvgDay4")),
          # uiOutput(ns("AvgDay5"))
        )
      )#end Periodogram Panel
    )#end tabsetpanel
    
  )
}

#' plots Server Functions
#'
#' @noRd 
mod_plots_server <- function(id, env, acto_selected, title, module_id, count, pos, session){
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      # hide all tabs
      # observeEvent(sessionInfo(), ignoreNULL = TRUE, once = TRUE, {
      #   browser()
      #   shinyjs::hide(id = ns("Actogram"), anim = FALSE)
      # ::hideTab(inputId = "plot_panels", target = "Actogram")
      # shiny::hideTab(inputId = "plot_panels", target = "Single line actogram")
      # shiny::hideTab(inputId = "plot_panels", target = "Daily activity")
      # shiny::hideTab(inputId = "plot_panels", target = "Periodogram")
      # shiny::hideTab(inputId = "plot_panels", target = "Average day")
      # })
      output[[pos]] <- renderUI(
        {
          mod_box_plot_ui(NS(id, module_id), title)
        }
      )
      mod_box_plot_server(
        module_id = module_id, 
        env = env, 
        acto_selected = acto_selected, 
        count = count, 
        title = title
      )
    }
  )
}

## To be copied in the UI
# mod_plots_ui("plots_ui_1")

## To be copied in the server
# mod_plots_server("plots_ui_1")
