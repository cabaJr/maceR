#' data_structure UI Function
#'
#' @description Shiny module to set the parameters of your dataset. It allows
#'     a pre-processing step that ensures your data are interpreted correctly by
#'     the loading functions.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_structure_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidRow(div(style = ""),
             shinydashboard::box(
               title = "Data files settings", 
               id = ns("box1_1_1"), 
               solidHeader = TRUE, 
               width = 12, 
               status = "primary", 
               collapsible = TRUE,
                 fluidRow(
                   column(
                     width = 5, #serieType DFsubsetRT
                          numericInput(
                            inputId = ns("discardFirst"),
                                       label = "Insert number of row to discard upfront: ",
                                       min = 0,
                                       value = 4,
                                       step = 1),
                         shinyWidgets::prettyRadioButtons(
                           inputId = ns("serieType"),
                           label = "",
                           inline = TRUE,
                           choices = c("Real time", "Time points"),
                           selected = "Time points"),
                          ######## second time window analysis ######################
                         shinyWidgets::prettyRadioButtons(
                           inputId = ns("DFsubsetRT2"), 
                           label = "Do you want to a second time window?", 
                           inline = TRUE,
                           choices = c("Yes", "No"), 
                           selected = "No"),
                          #Real Time
                          dateInput(
                            inputId = ns("timeFrame3"),
                            label = "Second window"),
                         dateInput(inputId = ns("timeFrame4"),
                                   label = ""),
                          textInput(ns("RTanalysis_starttime2"),
                                    label = "Insert the time when you want to start analyse your data: ",
                                    placeholder = "hh:mm:ss"),
                          textInput(ns("RTanalysis_endtime2"),
                                    label = "Insert the time when you want to end your analysis: ",
                                    placeholder = "hh:mm:ss"),
                          #Time points
                          numericInput(
                            ns("TPanalysis_starttime2"), 
                            label = "Insert the timepoint when you want to start analyse your data: ", 
                            min = 0, 
                            value = 0),
                          numericInput(
                            ns("TPanalysis_endtime2"), 
                            label = "Insert the timepoint when you want to end your analysis: ", 
                            min = 0, 
                            value = 0)
                          # actionButton(inputId = "help_1_1_1", label = "HELP", style="color: #fff; background-color: #1e690c; border-color: #1e530c")
                          
                   ),
                   column(
                     width = 5,
                     offset = 1,
                          #default
                          dateInput(
                            ns("rtStart"), 
                            label = "Insert experiment start day",
                            autoclose = TRUE),
                          textOutput(
                            ns("time")),
                          textInput(
                            ns("rtStarthour"),
                            label = "Insert experiment start hour",
                            value = "00:00:00"),
                          numericInput(
                            inputId = ns("TPduration"), 
                            label = "Insert timepoint duration (sec):", 
                            min = 1, 
                            value = 60),
                          #optional
                         shinyWidgets::prettyRadioButtons(
                           inputId = ns("DFsubsetRT"), 
                           label = "Do you want to analyse a particular time window?", 
                           inline = TRUE,
                           choices = c("Yes", "No"), 
                           selected = "No"),
                          dateInput(
                            inputId = ns("timeFrame1"), 
                            label = "Days to be analysed (default = all): "), 
                     dateInput(
                       inputId = ns("timeFrame2"), 
                       label = ""),
                     textInput(
                       ns("RTanalysis_starttime"), 
                       label = "Insert the time when you want to start analyse your data: ",
                       placeholder = "hh:mm:ss"),
                     textInput(ns("RTanalysis_endtime"), 
                               label = "Insert the time when you want to end your analysis: ",
                               placeholder = "hh:mm:ss"),
                     shinyWidgets::prettyRadioButtons(
                       inputId = ns("DFsubsetTP"), 
                       label = "Do you want to analyse a particular time window?", 
                       inline = TRUE,
                       choices = c("Yes", "No"), 
                       selected = "No"),
                     numericInput(
                       ns("TPanalysis_starttime"), 
                       label = "Insert the timepoint when you want to start analyse your data: ", 
                       min = 0, value = 0),
                     numericInput(
                       ns("TPanalysis_endtime"), 
                       label = "Insert the timepoint when you want to end your analysis: ", 
                       min = 0, 
                       value = 0),
                     actionButton(
                       inputId = ns("help1_1_1"), 
                       label = "HELP",
                       style="color: #fff; background-color: #1e690c; border-color: #1e530c")
                          
                   )
                 )
             ),
    ), #end fluidRow
#### LD settings #####
    fluidRow(
      div(
        style = ""),         
    shinydashboard::box(
      title = "LD cycle - Advanced",
      id = ns("box1_1_4"),
      solidHeader = TRUE,
      width = 12,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      fluidRow(
        column(width = 4, 
               numericInput(
                 inputId = ns("LDcond"),
                 label = "How long is your light period? ",
                 min = 0, 
                 max = 24, 
                 value = 12),
                   )#,
                   # column(width = 4,
                   #        numericInput(inputId = "dayLenght",
                   #                     label = "How long is your day? ",
                   #                     min = 8, max = 36, value = 24),
                   # )
                 ),
      fluidRow(
        column(
          width = 4,
          shinyWidgets::prettyRadioButtons(
            inputId = ns("DDask"),
            label = "Do you want to set a DD period?",
            inline = TRUE, 
            choices = c("Yes", "No"),
            selected = "No"))
        ),
      fluidRow(
        column(width = 4,
               numericInput(
                 inputId = ns("DDcond"),
                 label = "On which day does DD starts? ",
                 min = 1, 
                 value = 1)),
        column(
          width = 4, 
          offset = 2,
          actionButton(inputId = "updateLD", label = "Update"))
        )
      ),
    shinydashboard::box(
      title = "Load data", 
      id = ns("box1_1_5"), 
      solidHeader = TRUE, 
      width = 12, 
      status = "primary", 
      collapsible = TRUE,
      fluidRow(
        column(
          width = 7,
          actionButton(
            inputId = ns("go"), 
            label = "Load into Data frame")
          )
        )
      )
             #                                                     
             #                                            )
    ),#end of FluidRow
  )
}
    
#' data_structure Server Functions
#'
#' @noRd 
mod_data_structure_server <- function(id, env, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
 App_settings = env
 
 # clean interface
 observeEvent(sessionInfo(), ignoreNULL = TRUE, once = TRUE, {
   initialize_datastr_ui(dataStructure_out)
 })
 
 # initialize returnable reactiveValue
 toReturn = reactiveValues(YourDataTab = NULL,
                           AnalysisTab = NULL,
                           idList = NULL
                           )
    
    # show help messages 
    observeEvent(input$help1_1_1, {show_help(App_settings, 2)})
    observeEvent(input$help1_1_2, {show_help(App_settings, 3)})
    observeEvent(input$help1_2_1, {show_help(App_settings, 4)})
    
    #read parameters and enter in App_settings
    observeEvent(input$discardFirst, {
      App_settings$setDiscRow(input$discardFirst)
      Sys.sleep(2)
      preload_data(App_settings, skipRows = input$discardFirst)
      })
    observeEvent(input$serieType, {
      App_settings$setTimeDisp(input$serieType)
      switch(input$serieType,
             "Real time" = {update_DS_ui(case = "6", dataStructure_out)},
             "Time points" = {update_DS_ui(case = "7", dataStructure_out)})
      })
    observeEvent(input$rtStart, {App_settings$setExpstart(input$rtStart)})
    observeEvent(input$TPduration, {App_settings$setTimepointDur(input$TPduration)})
    #add parameters for LD condition to be registered in App_settings
    
    # initialize menu in DataStructure tab
    # update_DS_ui(session, "serietype", "2") #not working
    
    # update LD settings
    observeEvent(input$updateLD, {
      # save LD parameters in App_settings
      # App_settings$saveLDparams(x = App_settings, light = input$LDcond, ddVal = input$DDask, ddStart = input$DDcond)
      # load LD settings into App_settings
      # App_settings$setLD(x = App_settings, light = input$LDcond, ddVal = input$DDask, ddStart = input$DDcond)
    })
    
    # create Clean_mouse_data object
    observeEvent(input$go,{
      # save LD parameters in App_settings Change LD params to be updated later
      App_settings$saveLDparams(x = App_settings, light = input$LDcond, ddVal = input$DDask, ddStart = input$DDcond)
      # load data inside myCleanMice object
      load_data(App_settings)
      # Create table with metadata
      App_settings$env2$Annotate$showMeta(App_settings$env2)
      # load LD settings into App_settings
      App_settings$setLD(x = App_settings, light = input$LDcond, ddVal = input$DDask, ddStart = input$DDcond)
      # shinyjs::show("chooseM", anim = FALSE)
      toReturn$YourDataTab <- TRUE
      toReturn$AnalysisTab <- TRUE
      # initialize menu in DataStructure tab
      # update_DS_ui(session, "serietype", "2") #not working
      # Upload selector from listMice to choose what data to display in YourData
      toReturn$idList <- App_settings$listMice[,2]
    })
    
    observeEvent(input$subsetPlot, { ## open/close subsetting parameters
      case <- input$subsetPlot
      if(is.null(App_settings$env2) == FALSE){ ## change this with a function
        # switch(case, "Yes" = {App_settings$showSubsetting()}, "No" = {App_settings$clearSubsetting()})
        ## add function to populate fields with corresponding info when opening 
        ##     subset menu. see 1726-1753 app3.R 
        
      }
    })

  ## Create list to be returned
  dataStructure_out <- list(YourDataTab = reactive(toReturn$YourDataTab),
                            AnalysisTab = reactive(toReturn$AnalysisTab),
                            idList = reactive(toReturn$idList),
                            serieType = reactive(input$serieType),
                            DFsubsetRT2 = reactive(input$DFsubsetRT2),
                            DFsubsetRT = reactive(input$DFsubsetRT),
                            DFsubsetTP = reactive(input$DFsubsetTP)
                            )
    
  return(dataStructure_out)
    
  })
}
    
## To be copied in the UI
# mod_data_structure_ui("data_structure_ui_1")
    
## To be copied in the server
# mod_data_structure_server("data_structure_ui_1")
