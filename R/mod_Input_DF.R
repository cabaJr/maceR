#' Input_DF UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Input_DF_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidRow(style="",
             shinydashboard::box(title= "select files", id = "box0_1", width = 12, solidHeader = TRUE, status = "primary",
                                     column(width = 6,
                                            fileInput(inputId = "fileListId", label = "Select files to analyse", multiple = TRUE, div(style = "left" ))),
                                            # fileInput(inputId = "metadataListId", label = "Select metadata File", multiple = FALSE)),
                                     column(width= 5, offset = 1,
                                            # actionButton(inputId = "workthis", label = "browser"),
                                            fileInput(inputId = "fileMetaId", label = "Select metafile", multiple = FALSE)),
                                            # fileInput(inputId = "metadataListId", label = "Select metadata File", multiple = FALSE)),
                                     column(width = 4),
                                     column(width = 1, offset = 3,
                                            actionButton(inputId = "help_0_2", label = "HELP",
                                                         style="color: #fff; background-color: #1e690c; border-color: #1e530c")
                                     )
             )
    ),
    fluidRow(style = "",
             shinydashboardPlus::box(title= "Uploaded files", id = "box0_2", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "danger",
                                     fluidRow(
                                       column(width = 8, tableOutput('list'))
                                     )
             )
    ),
    fluidRow(style = "",
             shinydashboardPlus::box(title= "Uploaded metafiles", id = "box0_3", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "danger",
                                     fluidRow(
                                       column(width = 6, tableOutput('metaList'))) #, column(width = 6, textOutput('fileCheck')))
             )
    )
    
  )
}
    
#' Input_DF Server Functions
#'
#' @param id 
#'
#' @noRd 
#' 

mod_Input_DF_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    })
  
}
    
## To be copied in the UI
# mod_Input_DF_ui("Input_DF_ui_1")
    
## To be copied in the server
# mod_Input_DF_server("Input_DF_ui_1")
