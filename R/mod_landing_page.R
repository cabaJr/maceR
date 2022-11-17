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
                    p(HTML("
                           <h3></h3>
                            <br>
                            <b>maceR</b> is designed to streamline the processing of your data, 
                            creating various <b>plots</b> and <b>tables</b> ready to be exported 
                            for presentations or further analysis in other software.
                            <ul></ul>
                            <br>
                            All tables are exported in <code>.csv</code> format for increased compatibility.
                            <br>
                            <br>
                            <br>
                            This version of the app is still in development
                            <br>
                            'If you encounter bugs along your path, remember: <br>
                            It's not a bug - it's an undocumented feature '  
                            <br>
                            
                           ")),
             ),
             column(width = 5, 
               fluidRow(align = "center", #landing page central column
                    p(HTML("<h3><b>maceR</b></h3>"))),
               fluidRow(
                    p(HTML("
                            <br>
                            <ul><b>maceR</b> (<b>M</b>ouse <b>A</b>ctivity <b>C</b>ircadian analys<b>ER</b>) is a shiny based app designed to 
                            process and analyse locomotor activity data from circadian cabinets
                            <br>
                            <br>
                            with maceR you can generate several plots and tables including:
                            <li><b>Actograms</b></li>
                            <li><b>Periodograms</b></li>
                            <li><b>Total daily activity</b></li>
                            <li><b>Average circadian activity</b></li>
                            </ul>
                            <br>
                            <br>
                            "))
                    ),
                fluidRow(align = "center",           
                            p(HTML("To start a new experiment press the button below!
                            <br>
                                                   ")),
                    
                    actionButton(inputId = "new_experiment",
                                 label = "Start a new experiment",
                                 style="color: #fff; background-color: #1e690c; border-color: #1e530c; width:200px; height: 50px;"),
                    br(),
                    p(HTML(
                      "<br>
                      Here is a double plotted actogram!
                      <br>
                      "
                    )),
                    shiny::imageOutput(ns("examplePlot"))
             )),
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
