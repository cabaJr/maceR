#' data_structure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_structure_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidRow(div(style = "margin:30px;"),
             shinydashboardPlus::box(title = "Data files settings", id = "box1_1_1", solidHeader = TRUE, width = 12, status = "primary", collapsible = TRUE,
                 fluidRow(
                   column(5, #serieType DFsubsetRT
                          numericInput(inputId = "discardFirst",
                                       label = "Insert number of row to discard upfront: ",
                                       min = 0, value = 3),
                          radioButtons(inputId = "serieType", label = "", inline = TRUE,
                                       choices = c("Real time", "Time points"), selected = "Time points"),
                          ######## second time window analysis ######################
                          radioButtons(inputId = "DFsubsetRT2", label = "Do you want to a second time window?", inline = TRUE,
                                       choices = c("Yes", "No"), selected = "No"),
                          #Real Time
                          dateInput(inputId = "timeFrame3", label = "Second window"), dateInput(inputId = "timeFrame4", label = ""),
                          textInput("RTanalysis_starttime2", label = "Insert the time when you want to start analyse your data: ",
                                    placeholder = "hh:mm:ss"),
                          textInput("RTanalysis_endtime2", label = "Insert the time when you want to end your analysis: ",
                                    placeholder = "hh:mm:ss"),
                          #Time points
                          numericInput("TPanalysis_starttime2", label = "Insert the timepoint when you want to start analyse your data: ", min = 0, value = 0),
                          numericInput("TPanalysis_endtime2", label = "Insert the timepoint when you want to end your analysis: ", min = 0, value = 0)
                          # actionButton(inputId = "help_1_1_1", label = "HELP", style="color: #fff; background-color: #1e690c; border-color: #1e530c")
                          
                   ),
                   column(5, offset = 1,
                          #default
                          dateInput("rtStart", label = "Insert experiment start day",
                                    autoclose = TRUE),
                          textOutput("time"),
                          textInput("rtStarthour", label = "Insert experiment start hour", value = "00:00:00"),
                          numericInput(inputId = "TPduration", label = "Insert timepoint duration (sec):", min = 1, value = 60),
                          #optional
                          radioButtons(inputId = "DFsubsetRT", label = "Do you want to analyse a particular time window?", inline = TRUE,
                                       choices = c("Yes", "No"), selected = "No"),
                          dateInput(inputId = "timeFrame1", label = "Days to be analysed (default = all): "), dateInput(inputId = "timeFrame2", label = ""),
                          textInput("RTanalysis_starttime", label = "Insert the time when you want to start analyse your data: ",
                                    placeholder = "hh:mm:ss"),
                          textInput("RTanalysis_endtime", label = "Insert the time when you want to end your analysis: ",
                                    placeholder = "hh:mm:ss"),
                          radioButtons(inputId = "DFsubsetTP", label = "Do you want to analyse a particular time window?", inline = TRUE,
                                       choices = c("Yes", "No"), selected = "No"),
                          numericInput("TPanalysis_starttime", label = "Insert the timepoint when you want to start analyse your data: ", min = 0, value = 0),
                          numericInput("TPanalysis_endtime", label = "Insert the timepoint when you want to end your analysis: ", min = 0, value = 0),
                          actionButton(inputId = "help1_1_1", label = "HELP",
                                       style="color: #fff; background-color: #1e690c; border-color: #1e530c")
                          
                   )
                 )
             ),
    ), #end fluidRow
    fluidRow(div(style = "margin:30px;"),
             #                                                     # shinydashboardPlus::box1_1_2<- box(title = "Time alignment - data head", solidHeader = TRUE, width = 12, status = "primary", collapsible = TRUE,  collapsed = TRUE,
             #                                                     #                fluidRow(
             #                                                     #                  column(width = 5,
             #                                                     #                         radioButtons('expstartck', label = 'have you started recording all your cabinet at the same time?',
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
             #                                                     #                  column(width = 5, radioButtons('hourFormat', label = "Select the hour format in metadata/light on",
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
             #                                                     #                         radioButtons('dataCrop', label = 'Do you want to crop all dataset to the same length or to have different lengths?',
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
             shinydashboardPlus::box(title = "LD cycle - Advanced", id = "box1_1_4", solidHeader = TRUE, width = 12, status = "primary", collapsible = TRUE, collapsed = TRUE,
                 fluidRow(
                   column(width = 4,
                          numericInput(inputId = "LDcond",
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
                                 radioButtons(inputId = "DDask", label = "Do you want to set a DD period?",inline = TRUE, choices = c("Yes", "No"), selected = "No"))
                 ),
                 fluidRow(
                   column(width = 4,
                          numericInput(inputId = "DDcond",label = "On which day does DD starts? ",
                                       min = 1, value = 1))
                 )
             ),
             shinydashboardPlus::box(title = "Load data", id = "box1_1_5", solidHeader = TRUE, width = 12, status = "primary", collapsible = TRUE,
                 fluidRow(
                   column(width = 7,
                          actionButton(inputId = "go", label = "Load into Data frame")
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
    #                                            #                                   column(width = 12, radioButtons(inputId = "nonStdMeta", label = "", choices = c("Standard", "Custom"), selected = "Standard", inline = TRUE ))
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
    fluidRow(div(style = "margin:30px;"),
             shinydashboardPlus::box(title= " ", id = "box1_2_2", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(
                   column(width = 8, #radioButtons('cabinets', label = "Cabinet settings", inline = TRUE,
                          #choices = c('Automatic', 'Do not align data'), selected = 'Automatic'),# selected = character(0)),
                          DT::DTOutput('cabinetShift'), actionButton('debug', label = "Debug")#, textInput('light_on_record', label = "Add the time when light is switched on in the cabinet", placeholder = "HH:MM" ),
                          # actionButton('addRecord', label = "Add"), actionButton('rmvRecord', label = "Remove"),
                          # DT::DTOutput('lightOnTb')
                   ),
                 )
             )
    )
    
  )
}
    
#' data_structure Server Functions
#'
#' @noRd 
mod_data_structure_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    initialize_DS <- function(){
        hide(id = "idCol", anim = FALSE)          #hide custom metadata mode
        hide(id = "sexCol", anim = FALSE)
        hide(id = "geneCol", anim = FALSE)
        hide(id = "cabCol", anim = FALSE)
        hide(id = "metafiltered", anim = FALSE)   #hide filtered metadata table
        hide(id = "tablemetafilter", anim = FALSE)
        hide(id = "alignData", anim = FALSE)
        hide(id = "timeFrame3", anim = FALSE)
        hide(id = "RTanalysis_starttime2", anim = FALSE)
        hide(id = "RTanalysis_endtime2", anim = FALSE)
        hide(id = "TPanalysis_starttime2", anim = FALSE)
        hide(id = "TPanalysis_endtime2", anim = FALSE)
        hide(id = "rtStart", anim = FALSE)
        hide(id = "rtStarthour", anim = FALSE)
        hide(id = "DFsubsetRT", anim = FALSE)
        hide(id = "timeFrame1", anim = FALSE)
        hide(id = "RTanalysis_starttime", anim = FALSE)
        hide(id = "RTanalysis_endtime", anim = FALSE)
        hide(id = "TPanalysis_starttime", anim = FALSE)
        hide(id = "TPanalysis_endtime", anim = FALSE)
        hide(id = "timeFrame4", anim = FALSE)
        hide(id = "DFsubsetRT2", anim = FALSE)
        hide(id = "Dl2", anim = FALSE) #hide download button for table2 before it's created
        hide(id = "chooseM", anim = FALSE) #HIDE button to select mouse table to display
        # hide unnecessary buttons in box1_1 #
        hide(id = "serieType", anim = FALSE)
        hide(id = "DFsubsetTP", anim = FALSE)
    }
  })
}
    
## To be copied in the UI
# mod_data_structure_ui("data_structure_ui_1")
    
## To be copied in the server
# mod_data_structure_server("data_structure_ui_1")