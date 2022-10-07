#' analysis UI Function
#'
#' @description Shiny module to let the user decide what actions to perform
#'     with the data.
#'     
#' @details Each box is responsible for subsetting the data or to submit an
#'     analysis or subset the data. Box 3_0 allows the subsetting of data based 
#'     on factors or individual id. Box 3_1 allows the user to generate single 
#'     line actograms using the ggetho package. If not already present, it calls
#'     the generation of the behavr table containing data to generate actograms.
#'     Box 3_2 allows the selection of which Double plotted actograms to generate.
#'     Box 3_3 calls the generation of line plots showing the sum of daily 
#'     activity. Box 3_4 calls the generation of periodograms. All plots can be 
#'     grouped based on id, sex, genotype or cabinet where the animals are housed.
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
 
    fluidRow(div(style = ""),
             #
             ##
             #### Data subsetting ####
             ##
             #
             shinydashboard::box(
               title= "Subset data to visualize", 
               id = "box3_0", 
               width = 12, 
               solidHeader = TRUE, 
               collapsible = TRUE, 
               status = "primary",
                 fluidRow(
                   column(
                     width = 8,
                     shinyWidgets::prettyRadioButtons(
                       inputId = ns("subsetPlot"), 
                       label = "Do you want to subset the data to visualize?", 
                       choices = c("Yes", "No"), 
                       inline = TRUE, 
                       selected = "No")
                 ),
                 column(
                   width = 3, 
                   offset = 0,
                   actionButton(
                     ns('help3_0'), 
                     label = 'Help',
                     style="color: #fff; background-color: #1e690c; border-color: #1e530c;")
                   )
                 ),
                 fluidRow(
                   column(
                     width = 3, 
                     selectInput(
                       inputId = ns("idSubsetList"), 
                       label = "Select Id", 
                       choices = c("choose" = ""), 
                       multiple = TRUE, 
                       width = 150)
                     ),
                   column(width = 3, 
                          selectInput(
                            inputId = ns("sexSubsetList"), 
                            label = "Select Sex", 
                            choices = c("choose" = ""), 
                            multiple = TRUE, 
                            width = 150)
                          ),
                   column(
                     width = 3, 
                     selectInput(
                       inputId = ns("geneSubsetList"), 
                       label = "Select Genotype", 
                       choices = c("choose" = ""), 
                       multiple = TRUE, width = 150)
                     ),
                   column(
                     width = 3, 
                     selectInput(
                       inputId = ns("cabSubsetList"), 
                       label = "Select Cabinet", 
                       choices = c("choose" = ""), 
                       multiple = TRUE, 
                       width = 150)
                     )
                   ),
               fluidRow(
                 column(
                   width = 12,
                   textOutput(
                     ns("text11")
                     ), 
                   textOutput(ns("metaUniqueO"))
                   )
                 ),
               fluidRow(
                 column(
                   width = 12,
                   sliderInput(
                     inputId = ns("timeSubset"), 
                     label = "Select time window to visualize", 
                     min = 0, 
                     max = 34, 
                     step = 0.125, 
                     value = c(0, 10), 
                     width = '90%')
                   )
                 )
               )
             ),
    fluidRow(
      div(style = ""),
      #
      ##
      #### Actogram Box ####
      ##
      #
      shinydashboard::box(
        title= "Actogram", 
        id = "box3_2", 
        width = 12, 
        solidHeader = TRUE, 
        collapsible = TRUE, 
        status = "primary",
        fluidRow(
          column(
            width = 8,
            shinyWidgets::prettyCheckboxGroup(
              inputId = ns("DPActogram"), 
              label = "Select the desired plots:",
              choiceNames = c("Cumulative", "Averaged by sex", "Averaged by genotype", "Averaged by cabinet"),# "Individual"),
              choiceValues = c("DAtotal", "DAsex", "DAgenotype", "DAcabinet"),#, "individual"),
              inline = TRUE, 
              width = "80%"
              ), 
            # selectizeInput(
            #   inputId = ns("chooseId"), 
            #   label = NULL, 
            #   choices = c("choose" = "", levels(unique)), 
            #   width = 85, 
            #   multiple = FALSE
            #   ),
            actionButton(
              inputId = ns("printDP"), 
              label = "print")
          ),
          column(width = 4,
                 offset = 0,
                 br(),
                 actionButton(
                   ns('help3_2'), 
                   label = 'Help',
                   style="color: #fff; background-color: #1e690c; border-color: #1e530c;")
          )
        )
      )
      ),
    # fluidRow(div(style = ""),
    #          br(),
    #            fluidRow(
    #              column(
    #                width = 6,
    #                shinydashboardPlus::flipBox(
    #                  id = "myflipbox2",
    #                  width = 6,
    #                  front = div(
    #                    class = "text-center",
    #                    h1("Actograms"),
    #                    p("click on the flipbox to select which actogram to generate")
    #                  ),
    #                  back = div(
    #                    class = "text-center",
    #                    height = "300px",
    #                    width = "100%",
    #                    h1("Flip on click"),
    #                    p("please select which actogram you want to generate"),
    #                    fluidRow(column(width = 6,
    #                                    shinyWidgets::prettyCheckboxGroup(inputId = ns("DPActogram"), label = "Select the desired plots:",
    #                                                                      choiceNames = c("Cumulative", "Averaged by sex", "Averaged by genotype", "Averaged by cabinet"),# "Individual"),
    #                                                                      choiceValues = c("DAtotal", "DAsex", "DAgenotype", "DAcabinet"),#, "individual"),
    #                                                                      inline = TRUE, width = "80%"), selectizeInput(inputId = ns("chooseId"), label = NULL, choices = c("choose" = "", levels(unique)), width = 85, multiple = FALSE),
    #                                    actionButton(inputId = ns("printDP"), label = "print")))
    #                  )
    #                )
    #              )
    #            )
    # ),
    fluidRow(
      div(style = ""),
      #
      ##
      #### Single line actogram ####
      ##
      #
      shinydashboard::box(
        title= "Single line actogram", 
        id = ns("box3_1"), 
        width = 12, 
        solidHeader = TRUE, 
        collapsible = TRUE, 
        status = "primary",
        fluidRow(
          column(
            width = 8,
            shinyWidgets::prettyCheckboxGroup(
              inputId = ns("stdActogram"), 
              label = "Select the desired plots:",
              choiceNames = c("Total Actogram", "Splitted by sex", "Splitted by genotype", "Splitted by cabinet"),
              choiceValues = c("total", "sex", "genotype", "cabinet"),
              inline = TRUE, 
              width = "80%"),
            actionButton(
              inputId = ns("print"), 
              label = "print"), 
            downloadButton(
              outputId = ns("Dl1"), 
              label = "Download")
            ),
          column(width = 4,
                 offset = 0,
                 br(),
                 actionButton(
                   ns('help3_3'), 
                   label = 'Help',
                   style="color: #fff; background-color: #1e690c; border-color: #1e530c;")
                 )
          )
        )
      ),
    fluidRow(
      div(style = ""),
      #
      ##
      #### Sum of daily activity ####
      ##
      #
      shinydashboard::box(
        title= "Sum of daily activity",
        id = ns("box3_3"),
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        status = "primary",
        fluidRow(
          column(
            width = 6,
            shinyWidgets::prettyCheckboxGroup(
              inputId = ns("DAsum"), 
              label = "Select the desired plots:",
              choiceNames = c("Averaged by genotype", 
                              "Averaged by sex", 
                              "Individual + mean", 
                              "Averaged by sex, divided by cabinet", 
                              "Individual, divided by sex and genotype + mean", 
                              "Individual, divided by cabinet and genotype"),
              choiceValues = c("~gen", "~sex", "individual", "gen~sex", "indiv+sex~gen", "indiv+cab~gen"),
              inline = FALSE, 
              width = "80%"
              )
            ),
          column(
            width = 4, 
            shinyWidgets::prettyRadioButtons(
              inputId = ns("DAct_error"), 
              label = "Do you want to plot SD or Sem?", 
              choices = c("SD", "Sem"), 
              inline = TRUE, 
              selected = "Sem"
              )
            )
          ),
        fluidRow(
          column(
            width = 4,
            offset = 0,
            actionButton(
              inputId = ns("dayAct"), 
              label = "Daily activity"
            )
          ),
          column(
            width = 2, 
            offset = 1,
            br(),
            downloadButton(
              outputId = ns("Dl2"), 
              label = "Download"
              ),
            actionButton(
              ns('help3_4'), 
              label = 'Help',
              style="color: #fff; background-color: #1e690c; border-color: #1e530c;")
            )
          )
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
    fluidRow(
      div(style = ""),
      #
      ##
      #### Periodogram ####
      ##
      #
      shinydashboard::box(
        title = "Periodogram", 
        id = ns("box3_5"), 
        width = 12, 
        solidHeader = TRUE, 
        collapsible = TRUE, 
        status = "primary",
        fluidRow(
          column(
            width = 6,
            sliderInput(
              inputId = ns("periodRange"), 
              label = "Select period range", 
              min = 12, 
              max = 34, 
              step = 1, 
              value = c(20, 28), 
              width = '90%'
              ),
            ),
          column(
            width = 5, 
            offset = 1,
            selectizeInput(
              inputId = ns("periodFun"), 
              label = "Select function", 
              choices = c("choose" = "",
                          "Chi square" = "chi_sq_periodogram",
                          "Autocorrelated" = "ac_periodogram",
                          "Fourier" = "fourier_periodogram",
                          "Wavelet" = "cwt_periodogram",
                          "Lomb-Scargle" = "ls_periodogram"
                          ),
              multiple = FALSE, 
              width = '70%'
              )
            ),
          ),
        fluidRow(
          column(
            width = 8,
            shinyWidgets::prettyCheckboxGroup(
              inputId = ns("periodCho"), 
              label = "Select the desired plot:",
              choiceNames = c(
                "Cumulative", 
                "Individual", 
                "Sex", 
                "Genotype", 
                "Cabinet"
                ),# "Select Id"),
              choiceValues = c(
                "Pertotal", 
                "Perfaceted", 
                "Persex", 
                "Pergenotype", 
                "Percabinet"
                ),# "individual"),
              inline = TRUE, 
              width = "85%")
            ),
          column(
            width = 3, 
            offset = 1,
            actionButton(
              ns('help3_5'), 
              label = 'Help',
              style="color: #fff; background-color: #1e690c; border-color: #1e530c;")
            )
          ),
        fluidRow(
          column(
            width = 6,
            actionButton(
              inputId = ns("periodPrint"), 
              label = "Print"), 
            downloadButton(
              outputId = ns("Dl3"), 
              label = "Download"
              )
            )
          )
        )
      ),
    fluidRow(
      div(style = ""),
      #
      ##
      #### Average day ####
      ##
      #
      shinydashboard::box(
        title= "Average day", 
        id = ns("box3_6"), 
        width = 12, 
        solidHeader = TRUE, 
        collapsible = TRUE, 
        status = "primary",
        fluidRow(
          column(width = 6,
                 shinyWidgets::prettyCheckboxGroup(
                   inputId = ns("Avg_day_cho"), 
                   label = "Select the desired plots:",
                   choiceNames = c(
                     "Individual", 
                     "Averaged by sex", 
                     "Averaged by genotype"
                     ),#, "Individual, divided by sex and genotype + mean", "Individual, divided by cabinet and genotype"),
                   choiceValues = c(
                     "individualAvgD", 
                     "sexAvgD", 
                     "genotypeAvgD"
                     ),#, "gen~sex", "indiv+sex~gen", "indiv+cab~gen"),
                   width = "80%"
                   ),
                 ),
          column(
            width = 4, 
            shinyWidgets::prettyRadioButtons(
              inputId = ns("AvgDay_error"), 
              label = "Do you want to plot SD or Sem?", 
              choices = c(
                "SD", 
                "Sem"
                ), 
              inline = TRUE, 
              selected = "Sem"
              )
            )
          ),
        fluidRow(
          column(
            width = 3,
            offset = 2, 
            actionButton(
              inputId = ns("AvgDayPrint"), 
              label = "Print"
              )
            ),
          column(width = 2,
                 offset = 2, 
                 actionButton(
                   ns('help3_6'), 
                   label = 'Help',
                   style="color: #fff; background-color: #1e690c; border-color: #1e530c;")
                 ),
          column(
            width = 2,
            downloadButton(
              outputId = ns("Dl4"), 
              label = "Download"
              )
            )
          )
        )
      )
  )
  }
    
#' analysis Server Functions
#'
#' @noRd 
mod_analysis_server <- function(id, App_settings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # clean interface
    observeEvent(
      sessionInfo(), 
      ignoreNULL = TRUE, 
      once = TRUE, 
      {
      clearSubsetting()
        }
      )
    
    ## show help messages 
    observeEvent(input$help3_0, {show_help(App_settings, 9)}) #data subsetting
    observeEvent(input$help3_2, {show_help(App_settings, 10)}) #Double plotted actogram
    observeEvent(input$help3_3, {show_help(App_settings, 11)}) #single line acto
    observeEvent(input$help3_4, {show_help(App_settings, 12)}) #Sum of daily activity
    observeEvent(input$help3_5, {show_help(App_settings, 13)}) #Periodogram
    observeEvent(input$help3_6, {show_help(App_settings, 14)}) #Average day
    
    ## initialize toReturn object
    toReturn <- reactiveValues(
      plotTab = NULL,
      actos = NULL,
      DPactos = NULL,
      Dact = NULL,
      periods = NULL
      )
    
    #### Data Subsetting ####
    
    observeEvent(
      input$subsetPlot, 
      { #open/close subsetting parameters
        case <- input$subsetPlot
        if(is.null(App_settings$env2) == FALSE){
          switch(
            case,
            "Yes" = {showSubsetting()},
            "No" = {clearSubsetting()}
          )
          }
        }
      )
    ## update selectors
    observeEvent(
      input$subsetPlot, 
      {
        if(input$subsetPlot == "Yes")
          {
          upload_subsetting(
            funEnv = App_settings,
            session = session,
            input_result = input_result
          )
          }
        }
      )

    #compile vector with list of mice objects
    observeEvent(c(
      input$idSubsetList,
      input$sexSubsetList,
      input$geneSubsetList,
      input$cabSubsetList
      ), {
        App_settings$setListMiceFiltered(
          App_settings$env2,
          input$idSubsetList,
          input$sexSubsetList, 
          input$geneSubsetList,
          input$cabSubsetList
          )
        idList <- as.character(
          App_settings$subsetting$miceListFiltered$id
          )
        output$text11 <- renderText(paste("Selected Id: "))
        output$metaUniqueO <- renderText(paste(idList))
        }
      )
    #update min max values
    observeEvent(
      input$timeSubset,{
      Sys.sleep(1)
      App_settings$updateTimeRange(input$timeSubset, App_settings)
      }
      )
    
    #### SINGLE LINE PERIODOGRAM ####
    observeEvent(
      input$print,
      {
      ## get Custom table object through App_settings
      Custom_tables <- App_settings$env2$Custom_tables
      ## check if locomotor_act[[1]] is already present or calculate it
      Custom_tables$checkIf(App_settings, input$subsetPlot) #call to checker function that then calls behavrTable
      ## get annotate environment
      Annotate <- App_settings$env4$Annotate
      ## get user plot choices
      plot_choices <- input$stdActogram
      ## call the function to output the plot for all the selected plot types
      purrr::map(plot_choices, ~ Annotate$plot_actogram(env = App_settings, type = .x))
      ## assign value to be returned to activate plot tab
      if(App_settings$plotTab$tab == FALSE){
        toReturn$plotTab <- checkPlots(App_settings)
      }
      ## assign value of selected plots to be returned
      toReturn$actos <- plot_choices
    })
    
    ## download handler for behavr table 
    output$Dl1 <- download_obj(title = "Behavr_table_",
                               location = App_settings$env2$Custom_tables$locomotor_act[[1]],
                               format = "csv")
    
    #### DOUBLE PLOTTED PERIODOGRAM ####
    observeEvent(input$printDP, {
      ## get Custom table object through App_settings
      Custom_tables <- App_settings$env2$Custom_tables
      ## check if locomotor_act[[1]] is already present or calculate it
      Custom_tables$checkIf(App_settings, input$subsetPlot) #call to checker function that then calls behavrTable
      ## get annotate environment
      Annotate <- App_settings$env4$Annotate
      ## add LD settings calculation
      
      ## get user plot choices
      plot_choices <- input$DPActogram
      ## call the function to output the plot for all the selected plot types
      purrr::map(
        plot_choices, ~ Annotate$plot_DPactogram(env = App_settings, type = .x))
      
      ## assign value to be returned to activate plot tab
      if(App_settings$plotTab$tab == FALSE){
        toReturn$plotTab <- checkPlots(App_settings)
      }
      ## assign value of selected plots to be returned
      toReturn$DPactos <- plot_choices
    })
    
    #### SUM OF DAILY ACTIVITY ####
    observeEvent(
      input$dayAct, 
      {
      # browser()
      ## get Custom table object through App_settings
      Custom_tables <- App_settings$env2$Custom_tables
      ##compute daily activity table
      ## to add a check that if parameters are the same there is no need to recompute
      Custom_tables$dailyAct(App_settings, input$subsetPlot)
      ## get annotate environment
      Annotate <- App_settings$env4$Annotate
      ## get user plot choices
      plot_choices <- input$DAsum
      ## call the function to output the plot for all the selected plot types
      purrr::map(
        plot_choices, ~ Annotate$plot_DAct(env = App_settings, type = .x, error = input$DAct_error))
      
      ## assign value to be returned to activate plot tab
      if(App_settings$plotTab$tab == FALSE){
        toReturn$plotTab <- checkPlots(App_settings)
      }
      ## assign value of selected plots to be returned
      toReturn$Dact <- plot_choices
    })
    
    ## download handler for sum of daily activity 
    output$Dl2 <- download_obj(title = "Sum_of_daily_activity_",
                               location = App_settings$env2$Custom_tables$daily_act[[1]],
                               format = "csv")
    
    #### PERIODOGRAMS ####
    observeEvent(
      input$periodPrint, 
      {
      ## get Custom table object through App_settings
      Custom_tables <- App_settings$env2$Custom_tables
      ## check if locomotor_act[[1]] is already present or calculate it
      Custom_tables$checkIf(env = App_settings, subsetPlot =  input$subsetPlot) #call to checker function that then calls behavrTable
      ## get user plot choices 
          plot_choices <- input$periodCho
      ## create plot choices for histogram
          plot_choices_hist <- paste(plot_choices, "_hist", sep = "")
      ## get period range
          periodRange <- input$periodRange
      ## get function to use to compute periodogram
          periodFun <- input$periodFun
      Custom_tables$computePer(method = periodFun, periodRange = periodRange, env = App_settings, subsetVal = input$subsetPlot)
      ## get annotate environment
      Annotate <- App_settings$env4$Annotate
      ## call the function to output the plot for all the selected plot types
      purrr::map(
        plot_choices, ~ Annotate$plot_periodogram(funEnv = App_settings, plotType = .x))
      ## call the function to output the period boxplots for all selected plot types
      purrr::map(
        plot_choices_hist, ~ Annotate$plot_periodogram_hist(funEnv = App_settings, plotType = .x, periodRange = periodRange))
      ## assign value to be returned to activate plot tab
      if(App_settings$plotTab$tab == FALSE){
        toReturn$plotTab <- checkPlots(App_settings)
      }
      plot_choices_tot <- c(plot_choices, plot_choices_hist)
      ## assign value of selected plots to be returned
      toReturn$periods <- plot_choices_tot
    })
    
    ## download handler for periodogram table 
    output$Dl3 <- download_obj(title = "Periodogram_table_",
                               location = App_settings$env2$Custom_tables$periodograms[[2]],
                               format = "csv")
    
    #### AVERAGE DAY OF ACTIVITY ####
    observeEvent(input$AvgDayPrint, {
      ## get Custom table object through App_settings
      Custom_tables <- App_settings$env2$Custom_tables
      ## check if average_day[[1]] is already present or calculate it
      # Custom_tables$checkIf(App_settings, input$subsetPlot) #call to checker function that then calls behavrTable
      Custom_tables$AvgDay(App_settings, per_len = 1440, input$subsetPlot)
      ## get user plot choices 
      plot_choices <- input$Avg_day_cho
      ## get annotate environment
      Annotate <- App_settings$env4$Annotate
      ## call the function to output the plot for all the selected plot types
      purrr::map(
        plot_choices, ~ Annotate$plot_avg_day(funEnv = App_settings, plotType = .x, error = input$AvgDay_error))
      ## assign value to be returned to activate plot tab
      if(App_settings$plotTab$tab == FALSE){
        toReturn$plotTab <- checkPlots(App_settings)
      }
      ## assign value of selected plots to be returned
      toReturn$avgDay <- plot_choices
    })
    
    ## download handler for periodogram table 
    output$Dl4 <- download_obj(title = "Average_day_table_",
                               location = App_settings$env2$Custom_tables$average_day[[1]],
                               format = "csv")
    
    
    ## create list with values to return
    analysis_out <- list(
      plotTab = reactive(toReturn$plotTab),
      actos = reactive(toReturn$actos),
      DPactos= reactive(toReturn$DPactos),
      Dact = reactive(toReturn$Dact),
      periods = reactive(toReturn$periods),
      avgDay = reactive(toReturn$avgDay)
    )
    ## return input values as reactive
    input_result <- list(
      subsetPlot = reactive(input$subsetPlot),
      idSubsetList = reactive(input$idSubsetList),
      sexSubsetList = reactive(input$sexSubsetList),
      geneSubsetList = reactive(input$geneSubsetList),
      cabSubsetList = reactive(input$cabSubsetList),
      timeSubset = reactive(input$timeSubset)
    )
    return(c(analysis_out, input_result))
  })
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_ui_1")
    
## To be copied in the server
# mod_analysis_server("analysis_ui_1")
