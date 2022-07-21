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
             shinydashboard::box(title = "Data files settings", id = ns("box1_1_1"), solidHeader = TRUE, width = 12, status = "primary", collapsible = TRUE,
                 fluidRow(
                   column(5, #serieType DFsubsetRT
                          numericInput(inputId = ns("discardFirst"),
                                       label = "Insert number of row to discard upfront: ",
                                       min = 0, value = 3),
                         shinyWidgets::prettyRadioButtons(inputId = ns("serieType"), label = "", inline = TRUE,
                                       choices = c("Real time", "Time points"), selected = "Time points"),
                          ######## second time window analysis ######################
                         shinyWidgets::prettyRadioButtons(inputId = ns("DFsubsetRT2"), label = "Do you want to a second time window?", inline = TRUE,
                                       choices = c("Yes", "No"), selected = "No"),
                          #Real Time
                          dateInput(inputId = ns("timeFrame3"), label = "Second window"), dateInput(inputId = "timeFrame4", label = ""),
                          textInput(ns("RTanalysis_starttime2"), label = "Insert the time when you want to start analyse your data: ",
                                    placeholder = "hh:mm:ss"),
                          textInput(ns("RTanalysis_endtime2"), label = "Insert the time when you want to end your analysis: ",
                                    placeholder = "hh:mm:ss"),
                          #Time points
                          numericInput(ns("TPanalysis_starttime2"), label = "Insert the timepoint when you want to start analyse your data: ", min = 0, value = 0),
                          numericInput(ns("TPanalysis_endtime2"), label = "Insert the timepoint when you want to end your analysis: ", min = 0, value = 0)
                          # actionButton(inputId = "help_1_1_1", label = "HELP", style="color: #fff; background-color: #1e690c; border-color: #1e530c")
                          
                   ),
                   column(5, offset = 1,
                          #default
                          dateInput(ns("rtStart"), label = "Insert experiment start day",
                                    autoclose = TRUE),
                          textOutput(ns("time")),
                          textInput(ns("rtStarthour"), label = "Insert experiment start hour", value = "00:00:00"),
                          numericInput(inputId = ns("TPduration"), label = "Insert timepoint duration (sec):", min = 1, value = 60),
                          #optional
                         shinyWidgets::prettyRadioButtons(inputId = ns("DFsubsetRT"), label = "Do you want to analyse a particular time window?", inline = TRUE,
                                       choices = c("Yes", "No"), selected = "No"),
                          dateInput(inputId = ns("timeFrame1"), label = "Days to be analysed (default = all): "), dateInput(inputId = "timeFrame2", label = ""),
                          textInput(ns("RTanalysis_starttime"), label = "Insert the time when you want to start analyse your data: ",
                                    placeholder = "hh:mm:ss"),
                          textInput(ns("RTanalysis_endtime"), label = "Insert the time when you want to end your analysis: ",
                                    placeholder = "hh:mm:ss"),
                         shinyWidgets::prettyRadioButtons(inputId = ns("DFsubsetTP"), label = "Do you want to analyse a particular time window?", inline = TRUE,
                                       choices = c("Yes", "No"), selected = "No"),
                          numericInput(ns("TPanalysis_starttime"), label = "Insert the timepoint when you want to start analyse your data: ", min = 0, value = 0),
                          numericInput(ns("TPanalysis_endtime"), label = "Insert the timepoint when you want to end your analysis: ", min = 0, value = 0),
                          actionButton(inputId = ns("help1_1_1"), label = "HELP",
                                       style="color: #fff; background-color: #1e690c; border-color: #1e530c")
                          
                   )
                 )
             ),
    ), #end fluidRow
             #                                                     # shinydashboardPlus::box1_1_2<- box(title = "Time alignment - data head", solidHeader = TRUE, width = 12, status = "primary", collapsible = TRUE,  collapsed = TRUE,
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 5,
             #                                                     #                        shinyWidgets::prettyRadioButtons('expstartck', label = 'have you started recording all your cabinet at the same time?',
             #                                                     #                                      choices = c('Yes', 'No'), selected = 'Yes', inline = TRUE)
             #                                                     #                  ),
             #                                                     # 
             #                                                     #                  column(width = 4, offset = 1,
             #                                                     #                         textOutput('y112x1'), textOutput('y112x2'))
             #                                                     #                ),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 3, numericInput('delayCab1', 'Cabinet 1', min = 0, width = 100, value = 0)),
             #                                                     #                  column(width = 3, numericInput('delayCab2', 'Cabinet 2', min = 0, width = 100, value = 0)),
             #                                                     #                  column(width = 3, numericInput('delayCab3', 'Cabinet 3', min = 0, width = 100, value = 0)),
             #                                                     #                  column(width = 3, numericInput('delayCab4', 'Cabinet 4', min = 0, width = 100, value = 0))
             #                                                     #                ),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 5,shinyWidgets::prettyRadioButtons('hourFormat', label = "Select the hour format in metadata/light on",
             #                                                     #                                                 choices = c('12h', '24h'), selected = '12h'),#selected = character(0)),
             #                                                     #                         # DT::DTOutput('converted'), DT::DTOutput('sorted')
             #                                                     #                         # textOutput('sorted')
             #                                                     #                  ),
             #                                                     #                  column(width = 2, offset = 1,
             #                                                     #                         actionButton(inputId = "help1_1_2", label = "HELP",
             #                                                     #                                      style="color: #fff; background-color: #1e690c; border-color: #1e530c")),
             #                                                     #                  column(width = 5, offset = 3, textOutput('cabinetAlignmentTXT'), hidden(p(id = "cabinetAlignmentTXT")))
             #                                                     #                ),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 2 , offset = 1,
             #                                                     #                         actionButton(inputId = "alignData", label = "Align")),
             #                                                     # 
             #                                                     #                )
             #                                                     # ),
             #                                                     # shinydashboardPlus::box1_1_3<- box(title = "Time alignment - data tail", solidHeader = TRUE, width = 12, status = "primary", collapsible = TRUE, collapsed = TRUE,
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 7,
             #                                                     #                        shinyWidgets::prettyRadioButtons('dataCrop', label = 'Do you want to crop all dataset to the same length or to have different lengths?',
             #                                                     #                                      choices = c('Same length', 'Different length'), inline = TRUE, selected = 'Different length'))
             #                                                     #                )
             #                                                     # ),
             #                                                     # shinydashboardPlus::box1_1_4<- box(title = "LD cycle - Advanced", solidHeader = TRUE, width = 12, status = "primary", collapsible = TRUE, collapsed = TRUE,
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 7,
             #                                                     #                         numericInput(inputId = "LDcond",
             #                                                     #                                      label = "How many conditions are present in your experiment? ",
             #                                                     #                                      min = 1, max = 3, value = 1),    
             #                                                     #                  )
             #                                                     #                ),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 3, textOutput(outputId = 'y114x1'))),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond1L', label = 'Light', min = 0, max = 24, value = 12)),
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond1D', label = 'Dark', min = 0, max = 24, value = 12))),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond1End', label = 'On what day condition 1 ends?', min = 1, value = 1))
             #                                                     #                ),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 3, textOutput(outputId = 'y114x2'))),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond2L', label = 'Light', min = 0, max = 24, value = 12)),
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond2D', label = 'Dark', min = 0, max = 24, value = 12))
             #                                                     #                  ),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond2Start', label = 'On what day condition 2 starts?', min = 1, value = 1)),
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond2End', label = 'On what day condition 2 ends?', min = 1, value = 1))
             #                                                     #                ),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 3, textOutput(outputId = 'y114x3'))),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond3L', label = 'Light', min = 0, max = 24, value = 12)),
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond3D', label = 'Dark', min = 0, max = 24, value = 12))
             #                                                     #                ),
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond3Start', label = 'On what day condition 3 starts?', min = 1, value = 1)),
             #                                                     #                  column(width = 4,
             #                                                     #                         numericInput(inputId = 'cond3End', label = 'On what day condition 3 ends?', min = 1, value = 1))
             #                                                     #                )
             #                                                     # ),
             #### LD settings #####
    fluidRow(div(style = ""),         
    shinydashboard::box(title = "LD cycle - Advanced",
                        id = ns("box1_1_4"),
                        solidHeader = TRUE,
                        width = 12,
                        status = "primary",
                        collapsible = TRUE,
                        collapsed = TRUE,
                 fluidRow(
                   column(width = 4,
                          numericInput(inputId = ns("LDcond"),
                                       label = "How long is your light period? ",
                                       min = 0, max = 24, value = 12),
                   )#,
                   # column(width = 4,
                   #        numericInput(inputId = "dayLenght",
                   #                     label = "How long is your day? ",
                   #                     min = 8, max = 36, value = 24),
                   # )
                 ),
                 fluidRow(column(width = 4,
                                shinyWidgets::prettyRadioButtons(inputId = ns("DDask"), label = "Do you want to set a DD period?",inline = TRUE, choices = c("Yes", "No"), selected = "No"))
                 ),
                 fluidRow(
                   column(width = 4,
                          numericInput(inputId = ns("DDcond"),label = "On which day does DD starts? ",
                                       min = 1, value = 1))
                 )
             ),
             shinydashboard::box(title = "Load data", id = ns("box1_1_5"), solidHeader = TRUE, width = 12, status = "primary", collapsible = TRUE,
                 fluidRow(
                   column(width = 7,
                          actionButton(inputId = ns("go"), label = "Load into Data frame")
                   )
                 )
             )
             #                                                     
             #                                            )
    ),#end of FluidRow
    #                                   #####metadata panel#####
    #                                   tabPanel("Debug",
    #                                            # fluidRow(div(style = "margin:30px;"),
    #                                            #          shinydashboardPlus::box1_2_1 <- box(title = "Metadata file structure", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary", 
    #                                            #                          fluidRow(column(width = 12, textOutput("notStandard")),
    #                                            #                                   column(width = 12,shinyWidgets::prettyRadioButtons(inputId = "nonStdMeta", label = "", choices = c("Standard", "Custom"), selected = "Standard", inline = TRUE ))
    #                                            #                          ),
    #                                            #                          fluidRow(column(width = 3, selectizeInput(inputId = "idCol", label = "Select Id column", choices = c("choose" = "", "1" = "1", "2" = "2", "3" = "3", "4" = "4"), multiple = FALSE, width = 100)),
    #                                            #                                   column(width = 3, selectizeInput(inputId = "sexCol", label = "Select Sex column", choices = c("choose" = "", "1" = "1", "2" = "2", "3" = "3", "4" = "4"), multiple = FALSE, width = 110)),
    #                                            #                                   column(width = 3, selectizeInput(inputId = "geneCol", label = "Select Genotype column", choices = c("choose" = "", "1" = "1", "2" = "2", "3" = "3", "4" = "4"), multiple = FALSE, width = 110)),
    #                                            #                                   column(width = 3, selectizeInput(inputId = "cabCol", label = "Select Cabinet column", choices = c("choose" = "", "1" = "1", "2" = "2", "3" = "3", "4" = "4"), multiple = FALSE, width = 110))
    #                                            #                                    ),
    #                                            #                          fluidRow(column(width = 3, offset = 9, actionButton(inputId = "help1_2_1", label = "HELP",
    #                                            #                                                                              style="color: #fff; background-color: #1e690c; border-color: #1e530c"))
    #                                            #                          )
    #                                            #                          ),
    #                                            #          
    #                                            # ),
    
  )
}
    
#' data_structure Server Functions
#'
#' @noRd 
mod_data_structure_server <- function(id, env, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
 App_settings = env
 # initialize returnable reactiveValue
 toReturn = reactiveValues(YourDataTab = NULL,
                           AnalysisTab = NULL,
                           idList = NULL
                           )
 
    # initialize_DS <- function(){
        shinyjs::hide(id = "idCol", anim = FALSE)          #hide custom metadata mode
        shinyjs::hide(id = "sexCol", anim = FALSE)
        shinyjs::hide(id = "geneCol", anim = FALSE)
        shinyjs::hide(id = "cabCol", anim = FALSE)
        shinyjs::hide(id = "metafiltered", anim = FALSE)   #hide filtered metadata table
        shinyjs::hide(id = "tablemetafilter", anim = FALSE)
        shinyjs::hide(id = "alignData", anim = FALSE)
        shinyjs::hide(id = "timeFrame3", anim = FALSE)
        shinyjs::hide(id = "RTanalysis_starttime2", anim = FALSE)
        shinyjs::hide(id = "RTanalysis_endtime2", anim = FALSE)
        shinyjs::hide(id = "TPanalysis_starttime2", anim = FALSE)
        shinyjs::hide(id = "TPanalysis_endtime2", anim = FALSE)
        shinyjs::hide(id = "rtStart", anim = FALSE)
        shinyjs::hide(id = "rtStarthour", anim = FALSE)
        shinyjs::hide(id = "DFsubsetRT", anim = FALSE)
        shinyjs::hide(id = "timeFrame1", anim = FALSE)
        shinyjs::hide(id = "RTanalysis_starttime", anim = FALSE)
        shinyjs::hide(id = "RTanalysis_endtime", anim = FALSE)
        shinyjs::hide(id = "TPanalysis_starttime", anim = FALSE)
        shinyjs::hide(id = "TPanalysis_endtime", anim = FALSE)
        shinyjs::hide(id = "timeFrame4", anim = FALSE)
        shinyjs::hide(id = "DFsubsetRT2", anim = FALSE)
        shinyjs::hide(id = "Dl2", anim = FALSE) #hide download button for table2 before it's created
        shinyjs::hide(id = "chooseM", anim = FALSE) #HIDE button to select mouse table to display
        # hide unnecessary buttons in box1_1 #
        shinyjs::hide(id = "serieType", anim = FALSE)
        shinyjs::hide(id = "DFsubsetTP", anim = FALSE)
    # }
    
    # show help messages 
    observeEvent(input$help1_1_1, {show_help(App_settings, 2)})
    observeEvent(input$help1_1_2, {show_help(App_settings, 3)})
    observeEvent(input$help1_2_1, {show_help(App_settings, 4)})
    
    #read parameters and enter in App_settings
    observeEvent(input$discardFirst, {App_settings$setDiscRow(input$discardFirst)})
    observeEvent(input$serieType, {App_settings$setTimeDisp(input$serieType)})
    observeEvent(input$rtStart, {App_settings$setExpstart(input$rtStart)})
    observeEvent(input$TPduration, {App_settings$setTimepointDur(input$TPduration)})
    #add parameters for LD condition to be registered in App_settings
    
    # initialize menu in DataStructure tab
    update_DS_ui(session, "serietype", "2") #not working
    
    # create Clean_mouse_data object
    observeEvent(input$go,{
      # save LD parameters in App_settings
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
      update_DS_ui(session, "serietype", "2") #not working
      # Upload selector from listMice to choose what data to display in YourData
      toReturn$idList <- App_settings$listMice[,2]
    })
    
    ## open/close subsetting parameters in DataStructure tab box 1
    observeEvent(input$serieType, { 
      case <- input$serieType
      switch(case,
             "Real time" = {update_DS_ui(session, "serietype", 1)},
             "Time points" = {update_DS_ui(session, "serietype", 2)})
    })
    
    observeEvent(input$DFsubsetRT, { ## need to add function for closing
      case <- input$DFsubsetRT
      switch(case,
             "Yes" = update_DS_ui(session, "subsetting", "1"),
             "No" = update_DS_ui(session, "subsetting", "1_hide"))
    })
    
    observeEvent(input$DFsubsetTP, {
      case <- input$DFsubsetTP
      switch(case,
             "Yes" = update_DS_ui(session, "subsetting", "2"),
             "No" = update_DS_ui(session, "subsetting", "2_hide"))
    })
    
    observeEvent(input$DFsubsetRT2, {
      case <- input$DFsubsetRT2
      if(case == "Yes"){
        switch(input$serietype,
               "Real time" = {update_DS_ui(session, "subsetting", "3")},
               "Time points" = {update_DS_ui(session, "subsetting", "4")})
      }else{update_DS_ui(session, "subsetting", "5")}
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
                            idList = reactive(toReturn$idList))  
    
  return(dataStructure_out)
    
  })
}
    
## To be copied in the UI
# mod_data_structure_ui("data_structure_ui_1")
    
## To be copied in the server
# mod_data_structure_server("data_structure_ui_1")
