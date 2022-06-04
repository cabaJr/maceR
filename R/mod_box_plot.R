#' box_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_box_plot_ui <- function(id, plot_title){
  ns <- NS(id)
  tagList(
    
 shinydashboardPlus::box(title= plot_title, 
                         id = "boxwplot", 
                         width = 12, 
                         solidHeader = TRUE, 
                         collapsible = TRUE, 
                         status = "primary",
                         enable_dropdown = TRUE,
                         dropdown_icon = "wrench",
                         fluidRow(
                            column(width = 12,
                                   plotOutput(ns("plot_hold"))
                                   )
                            ),
                         fluidRow(
                           column(width = 2, 
                                  offset = 0,
                                  actionButton(inputId = ns("stop"), label = "browser")
                         ),
                            column(width = 2, 
                                   offset = 5,
                                   downloadButton(outputId = "download_1", label = "Download"))
                            )
 )
 
  )
}
    
#' box_plot Server Functions
#'
#' @noRd 
mod_box_plot_server <- function(id, env, session, acto_selected){
   moduleServer( id, function(input, output, session){
      ns <- session$ns
      Annotate <- env$env4$Annotate
      # browser()
      # for(i in seq_len(plot_list)){
      # if(is.null(plot_list) == TRUE){}else{
        plot <- Annotate$Actograms$acto1[[1]]
      # }
         output$plot_hold <- renderPlot(plot)
         # observeEvent(input$stop, {browser()})
      # }
   })
}
    
## To be copied in the UI
# mod_box_plot_ui("box_plot_ui_1")
    
## To be copied in the server
# mod_box_plot_server("box_plot_ui_1")
