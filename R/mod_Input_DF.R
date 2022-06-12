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
             shinydashboard::box(title= "select files", id = ns("box0_1"), width = 12, solidHeader = TRUE, status = "primary",
                                     column(width = 6,
                                            fileInput(inputId = ns("fileListId"), label = "Select files to analyse", multiple = TRUE, div(style = "left" ))),
                                            # fileInput(inputId = "metadataListId", label = "Select metadata File", multiple = FALSE)),
                                     column(width= 5, offset = 1,
                                            # actionButton(inputId = "workthis", label = "browser"),
                                            fileInput(inputId = ns("fileMetaId"), label = "Select metafile", multiple = FALSE)),
                                            # fileInput(inputId = "metadataListId", label = "Select metadata File", multiple = FALSE)),
                                     column(width = 4),
                                     column(width = 1, offset = 3,
                                            actionButton(inputId = ns("help_0_2"), label = "HELP",
                                                         style="color: #fff; background-color: #1e690c; border-color: #1e530c")#,
                                            # actionButton(inputId = "module", label = "module")
                                     )
             )
    ),
    fluidRow(style = "",
             shinydashboardPlus::box(title= "Uploaded files", id = ns("box0_2"), width = 12, solidHeader = TRUE, collapsible = TRUE, status = "danger",
                                     fluidRow(
                                       column(width = 8, tableOutput(ns('list')))
                                     )
             )
    ),
    fluidRow(style = "",
             shinydashboardPlus::box(title= "Uploaded metafiles", id = ns("box0_3"), width = 12, solidHeader = TRUE, collapsible = TRUE, status = "danger",
                                     fluidRow(
                                       column(width = 6, tableOutput(ns('metaList')))) #, column(width = 6, textOutput('fileCheck')))
             )
    ),
    ##try
    fluidRow(style = "",
             div(id = "placeholder")
    )
    
  )
}
    
#' Input_DF Server Functions
#'
#' @param id module id
#' @param env App_settings env
#' @return inputDF_out list containing $ui field with a TRUE/FALSE value to 
#'     render the next sidebar menu option 
#' @noRd 
#' 

mod_Input_DF_server <- function(id, env){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # observeEvent(input$module, {
    # shiny::insertUI(
    #   selector = "#placeholder",
    #   where = "afterEnd",
    #   ui = mod_box_plot_ui(ns("box_plot_ui_1"), title = "title")
    # )
    #   
    # })
    
    # initialize returnable reactiveValue
    toReturn = reactiveValues(ui = NULL)
    App_settings = env
    # show help messages 
    observeEvent(input$help_0_2, {show_help(App_settings, 1)})
  
    # Store data
    observeEvent(input$fileListId, { #add case when user re-uploads different files
      App_settings$setData(input$fileListId)
      output$list <- renderTable(App_settings$dataList$name)
      if(check_uploads(App_settings) == TRUE){
        toReturn$ui <- TRUE
        # initialize elements in DS
        # App_settings$initialize_DS()
        preload_data(App_settings)
      }
    })
    
    # Store metadata
    observeEvent(input$fileMetaId, {
      App_settings$setMeta(input$fileMetaId)
      output$metaList <- renderTable(App_settings$metadata$name)
      if(check_uploads(App_settings) == TRUE){
        toReturn$ui <- TRUE
        App_settings$initialize_DS(session)
        preload_data(App_settings)
      }
    })
    
    return(
      inputDF_out = reactive(toReturn$ui)
    )
    
    })
}
    
## To be copied in the UI
# mod_Input_DF_ui("Input_DF_ui_1")
    
## To be copied in the server
# mod_Input_DF_server("Input_DF_ui_1")
