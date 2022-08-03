#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
mod_landing_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(style = "",
             column(width = 3, #left column
                    p(HTML("<h3></h3>
                            <br>
                            This version of the app is still in development, please use caution when using it
                            <ul></ul>
                            <br>
                            'If you encounter bugs along your path, remember: <br>
                            It's not a bug - it's an undocumented feature '  
                            <br>
                                                   ")),
                    shiny::imageOutput(ns("leftImage")),
             ),
             column(width = 5, #landing page central column
                    p(HTML("<h3>maceR</h3>")),
                    p(HTML("
                            <br>
                            <ul>maceR (Mouse Activity Circadian analysER) is a shiny based app designed to 
                            process and analyse locomotor activity data from circadian cabinets
                            <br>
                            with maceR you can generate several plots and tables including:
                            <li>Actograms</li>
                            <li>Periodograms</li>
                            <li>Total daily activity</li>
                            <li>Average circadian activity</li>
                            </ul>
                            <br>
                            To start a new experiment press the button below!
                                                   ")),
                    
                    actionButton(inputId = "new_experiment", label = "Start a new experiment"),
                    br(),
                    p(HTML(
                      "<br>
                      Here is a double plotted actogram!
                      <br>
                      "
                    )),
                    shiny::imageOutput(ns("examplePlot"))
             ),
             column(width = 3, #right side column
                    p(HTML("<h3></h3>
                            <br>
                            <ul>
                            maceR has been developed at Imperial College London @ Brancaccio Lab <br>
                            For info email m.ferrari20@imperial.ac.uk
                            </ul>
                            <br>
                            
                                                   ")),
                    # shiny::imageOutput(ns("maceR"))
             )
    )
  )
}


#' landing_page Server Functions
#'
#' @noRd 
mod_landing_page_server <- function(id, env){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # browser()
    App_settings <- env
    # left column
    output$leftImage <- shiny::renderImage({
      filename <- normalizePath(file.path('./inst/app/www/code.png'))
      list(
        src = filename,
        width = "100%"
      )
    }, deleteFile = FALSE)
    #central column
    output$examplePlot <- shiny::renderImage({
      filename <- normalizePath(file.path('./inst/app/www/DP_acto.png'))
      list(
        src = filename,
        width = "100%"
      )
    }, deleteFile = FALSE)
    #right column
    # output$maceR <- shiny::renderImage({
    #   filename <- normalizePath(file.path('./inst/app/www/maceR.png'))
    #   list(
    #     src = filename,
    #     width = "100%"
    #   )
    # }, deleteFile = FALSE)
    
  })
}
    
## To be copied in the UI
# mod_landing_page_ui("landing_page_ui_1")
    
## To be copied in the server
# mod_landing_page_server("landing_page_ui_1")
