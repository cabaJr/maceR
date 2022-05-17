#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidRow(div(style = "margin:30px;"),
             
             shinydashboardPlus::box(title= "Subset data to visualize", id = "box3_0", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 8,
                                 radioButtons(inputId = "subsetPlot", label = "Do you want to subset the data to visualize?", choices = c("Yes", "No"), inline = TRUE, selected = "No")
                 ),
                 column(width = 3, offset = 1,
                        actionButton('help3_0', label = 'Help',
                                     style="color: #fff; background-color: #1e690c; border-color: #1e530c;"))),
                 fluidRow(column(width = 3, selectInput(inputId = "idSubsetList", label = "Select Id", choices = c("choose" = ""), multiple = TRUE, width = 150)),
                          column(width = 3, selectInput(inputId = "sexSubsetList", label = "Select Sex", choices = c("choose" = ""), multiple = TRUE, width = 150)),
                          column(width = 3, selectInput(inputId = "geneSubsetList", label = "Select Genotype", choices = c("choose" = ""), multiple = TRUE, width = 150)),
                          column(width = 3, selectInput(inputId = "cabSubsetList", label = "Select Cabinet", choices = c("choose" = ""), multiple = TRUE, width = 150))
                 ),
                 fluidRow(column(width = 12,
                                 textOutput("text11"), textOutput("metaUniqueO")
                 )),
                 fluidRow(column(width = 12,
                                 sliderInput(inputId = "timeSubset", label = "Select time window to visualize", min = 0, max = 34, step = 0.125, value = c(0, 10), width = '90%')))
             )
    ),
    fluidRow(div(style = "margin:30px;"),
             
             shinydashboardPlus::box(title= "Single line actogram", id = "box3_1", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 12,
                                 checkboxGroupInput(inputId = "stdActogram", label = "Select the desired plots:",
                                                    choiceNames = c("Total Actogram", "Splitted by sex", "Splitted by genotype", "Splitted by cabinet"),
                                                    choiceValues = c("total", "sex", "genotype", "cabinet"),
                                                    inline = TRUE, width = "80%"),
                                 actionButton(inputId = "print", label = "print"), actionButton(inputId = "Dl1", label = "Download")
                 ))
             )
    ),
    fluidRow(div(style = "margin:30px;"),
             shinydashboardPlus::box(title= "Actogram", id = "box3_2", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 12,
                                 checkboxGroupInput(inputId = "DPActogram", label = "Select the desired plots:",
                                                    choiceNames = c("Cumulative", "Averaged by sex", "Averaged by genotype", "Averaged by cabinet"),# "Individual"),
                                                    choiceValues = c("total", "sex", "genotype", "cabinet"),#, "individual"),
                                                    inline = TRUE, width = "80%"), selectizeInput(inputId = "chooseId", label = NULL, choices = c("choose" = "", levels(unique)), width = 85, multiple = FALSE),
                                 actionButton(inputId = "printDP", label = "print")
                 ))
             )
    ),
    fluidRow(div(style = "margin:30px;"),
             shinydashboardPlus::box(title= "Sum of daily activity", id = "box3_3", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 12,
                                 checkboxGroupInput(inputId = "DAsum", label = "Select the desired plots:",
                                                    choiceNames = c("Averaged by genotype", "Averaged by sex", "Individual + mean", "Averaged by sex, divided by cabinet", "Individual, divided by sex and genotype + mean", "Individual, divided by cabinet and genotype"),
                                                    choiceValues = c("~gen", "~sex", "individual", "gen~sex", "indiv+sex~gen", "indiv+cab~gen"),
                                                    inline = FALSE, width = "80%"),
                                 actionButton(inputId = "dayAct", label = "Daily activity"),
                 )),
                 fluidRow(column(width = 12,
                                 br(),
                                 DT::DTOutput('dailyActivity')
                                 
                 )),
                 fluidRow(column(width = 2, offset = 9,
                                 br(),
                                 downloadButton(outputId = "Dl2", label = "Download")
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
             shinydashboardPlus::box(title= "Periodogram", id = "box3_5", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(width = 6,
                                 sliderInput(inputId = "periodRange", label = "Select period range", min = 12, max = 34, step = 1, value = c(20, 28), width = '90%'),
                 ),
                 column(width = 5, offset = 1,
                        selectizeInput(inputId = "periodFun", label = "Select function", choices = c("choose" = "", "Chi square" = "chi_sq_periodogram", "Autocorrelated" = "ac_periodogram", "Lomb-Scargle" = "ls_periodogram"), multiple = FALSE, width = '70%')),
                 ),
                 fluidRow(column(width = 8,
                                 checkboxGroupInput(inputId = "periodCho", label = "Select the desired plot:",
                                                    choiceNames = c("Cumulative", "Individual", "Sex", "Genotype", "Cabinet"),# "Select Id"),
                                                    choiceValues = c("total", "faceted", "sex", "genotype", "cabinet"),# "individual"),
                                                    inline = TRUE, width = "85%")),
                          column(width = 3, offset = 1,
                                 actionButton('help3_5', label = 'Help',
                                              style="color: #fff; background-color: #1e690c; border-color: #1e530c;"))
                 ),
                 fluidRow(column(width = 6,
                                 actionButton(inputId = "periodPrint", label = "Print"), downloadButton(outputId = "Dl3", label = "Download"))
                 )
             )
    )
    
  )
}
    
#' analysis Server Functions
#'
#' @noRd 
mod_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_ui_1")
    
## To be copied in the server
# mod_analysis_server("analysis_ui_1")
