#' your_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_your_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidRow(style = "",
             shinydashboardPlus::box(title= "Uploaded metafiles", id = "box2_1", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "danger",
                 fluidRow(
                   column(width = 8, radioButtons(inputId = 'tablemetafilter', label = '', choiceNames = c('Display full metadata table', 'Display metadata of uploaded files only'),
                                                  choiceValues = c('Y', 'N'), inline = TRUE, selected = 'Y')),
                   column(width = 3, offset = 1, actionButton('help2_1', label = 'Help',
                                                              style="color: #fff; background-color: #1e690c; border-color: #1e530c;"))
                 ),
                 fluidRow(
                   column(width = 9, DT::DTOutput('metatable'), DT::DTOutput('metafiltered')))
             )
    ),
    fluidRow(style = "",
             shinydashboardPlus::box(title= "show data", id = "box2_2", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "warning",
                 fluidRow(
                   column(selectInput("chooseM", label = "Select Id", choices = c(App_settings$listMice[,2]), multiple = FALSE, width = 150),
                          width = 4)
                 ),
                 # column(width = 4, dateInput('findPointD', label = 'Insert data to find'),
                 #        textInput('findPointH', label = "", placeholder = 'hh:mm:ss')),
                 # textOutput('findtime')),
                 column(width = 12, DT::DTOutput('showdata')),#, DT::DTOutput('aligned')),
                 column(width = 2, offset = 6,
                        br(),
                        actionButton('help2_2', label = 'Help',
                                     style="color: #fff; background-color: #1e690c; border-color: #1e530c;")),
                 column(width = 2, offset = 1,
                        br(),
                        downloadButton(outputId = "dataTab", label = "Download")
                 )
                 
             )
    )
    
  )
}
    
#' your_data Server Functions
#'
#' @noRd 
mod_your_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_your_data_ui("your_data_ui_1")
    
## To be copied in the server
# mod_your_data_server("your_data_ui_1")
