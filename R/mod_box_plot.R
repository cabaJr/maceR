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
                                   shinycssloaders::withSpinner(plotOutput(ns("plot_hold")), id = "spin", type = 4, color = "#2E9AFE", size = 0.65)
                                   )
                            ),
                         fluidRow(
                            column(width = 2, 
                                   offset = 7,
                                   downloadButton(outputId = ns("download_1"), label = "Download"))
                            )
 )
 
  )
}
    
#' box_plot Server Functions
#'
#' @noRd 
mod_box_plot_server <- function(module_id, env, acto_selected, count){
   moduleServer(module_id, function(input, output, session){
      ns <- session$ns
      Annotate <- env$env4$Annotate

      ## get object containing the corresponding plot from table in Annotate
      plot_location <- eval(parse(text = unlist(acto_selected[count, 3]), n =1))
      ## print the parsed plot  
      output$plot_hold <- renderPlot(plot_location)
         
      #Download function
         filename_part <- unlist(acto_selected[count, 1])
         output$download_1 <- downloadHandler( #set an option to choose the quality of the output image
           filename = function(){paste("Single_line_acto_", filename_part, "_", Sys.Date(), ".png", sep = "")},
           content = function(file){
             png(file, width = 1820, height = 787, units = "px")
             print(plot_location)
             dev.off()
           }
         )
   })
}
    
## To be copied in the UI
# mod_box_plot_ui("box_plot_ui_1")
    
## To be copied in the server
# mod_box_plot_server("box_plot_ui_1")
