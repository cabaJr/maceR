#' hot_meta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hot_meta_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    shinydashboard::box(title= "Create metadata", id = ns("box0_4"), width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                        fluidRow(
                          shiny::br(),
                          # p(shiny::HTML("<h4 style=font-size:12>Paste your metadata here</h4>")),
                          column(width = 9, 
                                 #Rhandsontable output
                                 rhandsontable::rHandsontableOutput(ns("metaCustom"))
                          ),
                          # column to perform operations on the table
                          # column(width = 2, offset = 1,
                          #        actionButton(inputId = ns("add_column"), label = "Add column"))
                        ),
                        fluidRow(
                          shiny::br(),
                          column(width = 2, offset = 9,
                                 downloadButton(outputId = ns("saveMeta"), label = "Export metadata"))
                        )
    )
  )
}
    
#' hot_meta Server Functions
#'
#' @noRd 
mod_hot_meta_server <- function(id, env){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #initialize metadata obj
    metadata <- NULL
    #create dataframe from Rhandsontable
    startvalue <- reactiveVal(value = data.frame(Identifier = 1:12,
                                                 Cabinet = "1",
                                                 Sex = c("F", "M"),
                                                 Genotype = "wt",
                                                 LightsOn = "7am"))
    col_heads <- c("Identifier", "Cabinet", "Sex", "Genotype", "LightsOn")
    
    output$metaCustom <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(startvalue(),
                                   colHeaders = col_heads,
                                   useTypes = FALSE
                                   )
                                                            }
                                                            )
    
    toReturn = reactiveValues(ui = NULL)
    observe({
      if(!is.null(input$metaCustom))
        metadata <- rhandsontable::hot_to_r(input$metaCustom)
    })
    
    output$saveMeta <- download_obj("metadata", rhandsontable::hot_to_r(input$metaCustom), "2.csv") #workaround to hack the extension generator in download_obj
    # observeEvent(output$saveMeta,{
      #import values from rhandsontable
      # metadata <- rhandsontable::hot_to_r(input$metaCustom)
      # browser()
      #create csv file with generated metadata
      # download_obj("metadata", metadata, "csv")
      # write.csv2(metadata, file, quote = FALSE, row.names = FALSE)
      #call App_settings$setMeta() using the stored csv file
      # env$setMeta(metadata)
      # output$metaList <- renderTable(App_settings$metadata$name)
      # if(check_uploads(env) == TRUE){
      #   toReturn$ui <- TRUE
      #   env$initialize_DS(session)
      #   preload_data(env)
      # }
      #make download metadata button appear
    # })
    # return(toReturn)
  })
}
    
## To be copied in the UI
# mod_hot_meta_ui("hot_meta_ui_1")
    
## To be copied in the server
# mod_hot_meta_server("hot_meta_ui_1")
