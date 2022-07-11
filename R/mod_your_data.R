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
             shinydashboardPlus::box(title= "Uploaded metafiles", id = ns("box2_1"), width = 12, solidHeader = TRUE, collapsible = TRUE, status = "danger",
                 fluidRow(
                   column(width = 8, radioButtons(inputId = ns('tablemetafilter'), label = '', choiceNames = c('Display full metadata table', 'Display metadata of uploaded files only'),
                                                  choiceValues = c('Y', 'N'), inline = TRUE, selected = 'Y')),
                   column(width = 3, offset = 1, actionButton(ns('help2_1'), label = 'Help',
                                                              style="color: #fff; background-color: #1e690c; border-color: #1e530c;"))
                 ),
                 fluidRow(
                   column(width = 9, DT::DTOutput(ns('metatable')), DT::DTOutput(ns('metafiltered'))))
             )
    ),
    fluidRow(style = "",
             shinydashboardPlus::box(title= "show data", id = ns("box2_2"), width = 12, solidHeader = TRUE, collapsible = TRUE, status = "warning",
                 fluidRow(
                   column(selectInput(ns("chooseM"), label = "Select Id", choices = c(App_settings$listMice[,2]), multiple = FALSE, width = 150),
                          width = 4)
                 ),
                 # column(width = 4, dateInput('findPointD', label = 'Insert data to find'),
                 #        textInput('findPointH', label = "", placeholder = 'hh:mm:ss')),
                 # textOutput('findtime')),
                 column(width = 12, DT::DTOutput(ns('showdata'))),#, DT::DTOutput('aligned')),
                 column(width = 2, offset = 6,
                        br(),
                        actionButton(ns('help2_2'), label = 'Help',
                                     style="color: #fff; background-color: #1e690c; border-color: #1e530c;")),
                 column(width = 2, offset = 1,
                        br(),
                        downloadButton(outputId = ns("dataTab"), label = "Download")
                 )
                 
             )
    )
    
  )
}
    
#' your_data Server Functions
#'
#' @noRd 
mod_your_data_server <- function(id, env, idList = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 App_settings = env
 # initialize returnable reactiveValue
 toReturn = reactiveValues(var = NULL)
    # show help messages 
    observeEvent(input$help2_1, {show_help(App_settings, 5)})
    observeEvent(input$help2_2, {show_help(App_settings, 6)})
    
    #don't compute when app is started but only after files are uploaded
    if(is.null(idList) == TRUE){}else{
    # show metadata table in Yourdata tab
    output$metafiltered <- DT::renderDT(App_settings$env2$Annotate$metaTable)
    shinyjs::show("metafiltered", anim = FALSE)
    # Upload selector from listMice to choose what data to display in YourData
    idList <- App_settings$listMice[,2]
    updateSelectInput(session, "chooseM", choices = c("choose" = "", idList, "All" = "All"), selected = NULL)
  }
    
    # display data in YourData tab
    observeEvent(input$chooseM, { #when a new id is selected this chunck runs twice (bug)
      #import choosen id and list of ids
      id <- input$chooseM
      listMice <- App_settings$listMice
      if(is.null(App_settings$env2$Annotate) == FALSE){
        App_settings$env2$Annotate$showData(App_settings$env2, id, listMice)
        table <- App_settings$env2$Annotate$actTable
        output$showdata <- DT::renderDT(table, filter = 'top')
        shinyjs::show("dataTab", anim = FALSE)
      }else{}
    }, ignoreInit = TRUE)
    
    #download button for activity data table

    output$dataTab <- download_obj(title = "Behavr_Table_data_",
                                   location = App_settings$env2$Annotate$actTable,
                                   format = "csv")
      
      # downloadHandler( 
      # filename = function(){paste("Behavr_Table_data_", Sys.Date(), ".csv", sep = "")},
      # content = function(file){
      #       write.csv(App_settings$env2$Annotate$actTable, file, quote = FALSE, row.names = FALSE)
      #     }
      #   )
    
    
    return(
      yourData_out <- reactive(toReturn$var)
      
      )
  })
}
    
## To be copied in the UI
# mod_your_data_ui("your_data_ui_1")
    
## To be copied in the server
# mod_your_data_server("your_data_ui_1")
