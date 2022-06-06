#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidRow(div(style = "margin:30px;"),
             
             shinydashboard::box(title= "Subset data to visualize", id = "box3_0", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 8,
                                 shinyWidgets::prettyRadioButtons(inputId = ns("subsetPlot"), label = "Do you want to subset the data to visualize?", choices = c("Yes", "No"), inline = TRUE, selected = "No")
                 ),
                 column(width = 3, offset = 1,
                        actionButton(ns('help3_0'), label = 'Help',
                                     style="color: #fff; background-color: #1e690c; border-color: #1e530c;"))),
                 fluidRow(column(width = 3, selectInput(inputId = ns("idSubsetList"), label = "Select Id", choices = c("choose" = ""), multiple = TRUE, width = 150)),
                          column(width = 3, selectInput(inputId = ns("sexSubsetList"), label = "Select Sex", choices = c("choose" = ""), multiple = TRUE, width = 150)),
                          column(width = 3, selectInput(inputId = ns("geneSubsetList"), label = "Select Genotype", choices = c("choose" = ""), multiple = TRUE, width = 150)),
                          column(width = 3, selectInput(inputId = ns("cabSubsetList"), label = "Select Cabinet", choices = c("choose" = ""), multiple = TRUE, width = 150))
                 ),
                 fluidRow(column(width = 12,
                                 textOutput(ns("text11")), textOutput(ns("metaUniqueO"))
                 )),
                 fluidRow(column(width = 12,
                                 sliderInput(inputId = ns("timeSubset"), label = "Select time window to visualize", min = 0, max = 34, step = 0.125, value = c(0, 10), width = '90%')))
             )
    ),
    fluidRow(div(style = "margin:30px;"),
             
             shinydashboard::box(title= "Single line actogram", id = ns("box3_1"), width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 12,
                                 shinyWidgets::prettyCheckboxGroup(inputId = ns("stdActogram"), label = "Select the desired plots:",
                                                    choiceNames = c("Total Actogram", "Splitted by sex", "Splitted by genotype", "Splitted by cabinet"),
                                                    choiceValues = c("total", "sex", "genotype", "cabinet"),
                                                    inline = TRUE, width = "80%"),
                                 actionButton(inputId = ns("print"), label = "print"), actionButton(inputId = ns("Dl1"), label = "Download")
                 ))
             )
    ),
    fluidRow(div(style = "margin:30px;"),
             shinydashboard::box(title= "Actogram", id = "box3_2", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 12,
                                 shinyWidgets::prettyCheckboxGroup(inputId = "DPActogram", label = "Select the desired plots:",
                                                    choiceNames = c("Cumulative", "Averaged by sex", "Averaged by genotype", "Averaged by cabinet"),# "Individual"),
                                                    choiceValues = c("DAtotal", "DAsex", "DAgenotype", "DAcabinet"),#, "individual"),
                                                    inline = TRUE, width = "80%"), selectizeInput(inputId = ns("chooseId"), label = NULL, choices = c("choose" = "", levels(unique)), width = 85, multiple = FALSE),
                                 actionButton(inputId = ns("printDP"), label = "print")
                 ))
             )
    ),
    fluidRow(div(style = "margin:30px;"),
             shinydashboard::box(title= "Sum of daily activity", id = "box3_3", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 12,
                                 shinyWidgets::prettyCheckboxGroup(inputId = "DAsum", label = "Select the desired plots:",
                                                    choiceNames = c("Averaged by genotype", "Averaged by sex", "Individual + mean", "Averaged by sex, divided by cabinet", "Individual, divided by sex and genotype + mean", "Individual, divided by cabinet and genotype"),
                                                    choiceValues = c("~gen", "~sex", "individual", "gen~sex", "indiv+sex~gen", "indiv+cab~gen"),
                                                    inline = FALSE, width = "80%"),
                                 actionButton(inputId = ns("dayAct"), label = "Daily activity"),
                 )),
                 fluidRow(column(width = 12,
                                 br(),
                                 DT::DTOutput(ns('dailyActivity'))
                                 
                 )),
                 fluidRow(column(width = 2, offset = 9,
                                 br(),
                                 downloadButton(outputId = ns("Dl2"), label = "Download")
                 ))
             )
    ),
    # fluidRow(div(style = "margin:30px;"),
    #          box3_4 <- box(title= "Activity Onset", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
    #                        fluidRow(column(width = 12, offset = 0,
    #                                        actionButton(inputId = "trythis", label = "Calculate - Very very slow"),
    #                                        DT::DTOutput("onsetDT")
    #                        )),
    #                        fluidRow(column(width = 5, offset = 7,
    #                                        actionButton('help3_4', label = 'Help',
    #                                                     style="color: #fff; background-color: #1e690c; border-color: #1e530c;")))
    #          )
    # ),
    fluidRow(div(style = "margin:30px;"),
             shinydashboard::box(title= "Periodogram", id = "box3_5", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 6,
                                 sliderInput(inputId = ns("periodRange"), label = "Select period range", min = 12, max = 34, step = 1, value = c(20, 28), width = '90%'),
                 ),
                 column(width = 5, offset = 1,
                        selectizeInput(inputId = ns("periodFun"), label = "Select function", choices = c("choose" = "", "Chi square" = "chi_sq_periodogram", "Autocorrelated" = "ac_periodogram", "Lomb-Scargle" = "ls_periodogram"), multiple = FALSE, width = '70%')),
                 ),
                 fluidRow(column(width = 8,
                                 shinyWidgets::prettyCheckboxGroup(inputId = ns("periodCho"), label = "Select the desired plot:",
                                                    choiceNames = c("Cumulative", "Individual", "Sex", "Genotype", "Cabinet"),# "Select Id"),
                                                    choiceValues = c("Pertotal", "Perfaceted", "Persex", "Pergenotype", "Percabinet"),# "individual"),
                                                    inline = TRUE, width = "85%")),
                          column(width = 3, offset = 1,
                                 actionButton('help3_5', label = 'Help',
                                              style="color: #fff; background-color: #1e690c; border-color: #1e530c;"))
                 ),
                 fluidRow(column(width = 6,
                                 actionButton(inputId = ns("periodPrint"), label = "Print"), downloadButton(outputId = ns("Dl3"), label = "Download"))
                 )
             )
    ),
    fluidRow(div(style = "margin:30px;"),
             # shiny::uiOutput("plot_box")
    )
    
  )
}
    
#' analysis Server Functions
#'
#' @noRd 
mod_analysis_server <- function(id, App_settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ## show help messages 
    observeEvent(input$help3_0, {show_help(App_settings, 7)})
    observeEvent(input$help3_4, {show_help(App_settings, 8)})
    observeEvent(input$help3_5, {show_help(App_settings, 9)})
    
    ## initialize toReturn object
    toReturn <- reactiveValues(plotTab = NULL,
                               actos = NULL,
                               DPactos = NULL,
                               Dact = NULL,
                               periods = NULL)
    
    observeEvent(input$print,{
      ## get Custom table object through App_settings
      Custom_tables <- App_settings$env2$Custom_tables
      ## check if table1 is already present or calculate it
      Custom_tables$checkIf(App_settings, input$subsetPlot) #call to checker function that then calls behavrTable
      ## get annotate environment
      Annotate <- App_settings$env4$Annotate
      
      ## calculate LD settings --> make the module able to access values outside of it with
      ## myModule("myModule1", reactive(input$checkbox1))
      # App_settings$setLD(App_settings, input$LDcond , input$DDask, input$DDcond)
      
      ## get user plot choices
      plot_choices <- input$stdActogram
      ## call the function to output the plot for all the selected plot types
      purrr::map(plot_choices, ~ Annotate$plot_actogram(x = App_settings, type = .x))
      
      ## assign value to be returned to activate plot tab
      if(App_settings$plotTab == FALSE){
        toReturn$plotTab <- checkPlots(App_settings)
      }
      ## assign value of selected plots to be returned
      toReturn$actos <- plot_choices
    })
    
    observeEvent(input$printDP, {
      ## get Custom table object through App_settings
      Custom_tables <- App_settings$env2$Custom_tables
      ## check if table1 is already present or calculate it
      Custom_tables$checkIf(App_settings, input$subsetPlot) #call to checker function that then calls behavrTable
      ## get annotate environment
      Annotate <- App_settings$env4$Annotate
      ## add LD settings calculation
      
      ## get user plot choices
      plot_choices <- input$DPActogram
      ## call the function to output the plot for all the selected plot types
      purrr::map(plot_choices, ~ Annotate$plot_DPactogram(x = App_settings, type = .x))
      
      ## assign value to be returned to activate plot tab
      if(App_settings$plotTab == FALSE){
        toReturn$plotTab <- checkPlots(App_settings)
      }
      ## assign value of selected plots to be returned
      toReturn$actos <- plot_choices
    })
    # 
    # observeEvent(input$dayAct, {
    #   ## get Custom table object through App_settings
    #   Custom_tables <- App_settings$env2$Custom_tables
    #   ##compute daily activity table
    #   Custom_tables$dailyAct(App_settings)
    #   ## get annotate environment
    #   Annotate <- App_settings$env4$Annotate
    #   ns_id <- length(Annotate$DAct_plots$DAct1)
    #   browser()
    #   output$plot_box <- shiny::renderUI(mod_box_plot_ui(ns_id))
    # })
    
    ## create list with values to return
    analysis_out <- list(
      plotTab = reactive(toReturn$plotTab),
      actos = reactive(toReturn$actos),
      DPactos= reactive(toReturn$DPactos),
      Dact = reactive(toReturn$Dact),
      periods = reactive(toReturn$periods)
    )
    
    return(analysis_out)
  })
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_ui_1")
    
## To be copied in the server
# mod_analysis_server("analysis_ui_1")
