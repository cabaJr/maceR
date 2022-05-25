#' plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    tabsetPanel(
      tabPanel("Single line actogram",
               fluidRow(
                 div(id = "plot_placeholder")
                 # withSpinner(plotOutput('actogram1'), id = "spin1_1", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "actogram_1", label = "Download"),
                 # withSpinner(plotOutput('actogram2'), id = "spin1_2", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "actogram_2", label = "Download"),
                 # withSpinner(plotOutput('actogram3'), id = "spin1_3", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "actogram_3", label = "Download"),
                 # withSpinner(plotOutput('actogram4'), id = "spin1_4", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "actogram_4", label = "Download")
               )
      ),#end actogram panel
      tabPanel("Actogram",
               fluidRow(
                 
                 # withSpinner(plotOutput('DPactogram1'), id = "spin2_1", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DPactogram_1", label = "Download"),
                 # withSpinner(plotOutput('DPactogram2'), id = "spin2_2", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DPactogram_2", label = "Download"),
                 # withSpinner(plotOutput('DPactogram3'), id = "spin2_3", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DPactogram_3", label = "Download"),
                 # withSpinner(plotOutput('DPactogram4'), id = "spin2_4", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DPactogram_4", label = "Download"),
                 # withSpinner(plotOutput('DPactogram5'), id = "spin2_5", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DPactogram_5", label = "Download")
               )
      ),#end DP actogram panel
      tabPanel("Daily activity",
               fluidRow(
                 
                 # withSpinner(plotOutput('DAct1'), id = "spin3_1", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DAct_1", label = "Download"),
                 # withSpinner(plotOutput('DAct2'), id = "spin3_2", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DAct_2", label = "Download"),
                 # withSpinner(plotOutput('DAct3'), id = "spin3_3", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DAct_3", label = "Download"),
                 # withSpinner(plotOutput('DAct4'), id = "spin3_4", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DAct_4", label = "Download"),
                 # withSpinner(plotOutput('DAct5'), id = "spin3_5", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DAct_5", label = "Download"),
                 # withSpinner(plotOutput('DAct6'), id = "spin3_6", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "DAct_6", label = "Download")
               )
      ),#end Daily Activity panel
      tabPanel("Periodogram",
               fluidRow(
                 
                 # withSpinner(plotOutput('Per1'), id = "spin4_1", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "Per_1", label = "Download"),
                 # withSpinner(plotOutput('Per2'), id = "spin4_2", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "Per_2", label = "Download"),
                 # withSpinner(plotOutput('Per3'), id = "spin4_3", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "Per_3", label = "Download"),
                 # withSpinner(plotOutput('Per4'), id = "spin4_4", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "Per_4", label = "Download"),
                 # withSpinner(plotOutput('Per5'), id = "spin4_5", type = 4, color = "#2E9AFE", size = 0.65),
                 # downloadButton(outputId = "Per_5", label = "Download"),#,
                 # plotOutput('Per3'), downloadButton(outputId = "Per_3", label = "Download")
               )
      )#end Periodogram Panel
    )#end tabsetpanel
    
  )
}
    
#' plots Server Functions
#'
#' @noRd 
mod_plots_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}
    
## To be copied in the UI
# mod_plots_ui("plots_ui_1")
    
## To be copied in the server
# mod_plots_server("plots_ui_1")
