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
                    shiny::textOutput(ns("text_left")) 
             ),
             column(width = 6, #landing page central column
                    shiny::plotOutput(ns("text1")),
                    shiny::textOutput(ns("plot1")),
                    fileInput(inputId = "metadataListId", label = "Select metadata File", multiple = FALSE),
                    actionButton(inputId = "new_experiment", label = "Start a new experiment")
             ),
             column(width = 3, #right side column
                    shiny::imageOutput(ns("image1"))
             )
    )
  )
}


#' landing_page Server Functions
#'
#' @noRd 
mod_landing_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # left column
    output$text_left <- shiny::renderText({shinipsum::random_text(nwords = 320)})
    #central column
    output$plot1 <- shiny::renderText({shinipsum::random_text(nwords = 30)})
    output$text1 <- shiny::renderPlot({shinipsum::random_ggplot("random")})
    #right column
    output$image1 <- shiny::renderImage({shinipsum::random_image()}, deleteFile = TRUE)
    
  })
}
    
## To be copied in the UI
# mod_landing_page_ui("landing_page_ui_1")
    
## To be copied in the server
# mod_landing_page_server("landing_page_ui_1")
