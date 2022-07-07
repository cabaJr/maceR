#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # create home tab Item using fct_generate_UI
  output$Home <- render_tabItem_ui(tabname = "Home",
                                   text = "Home",
                                   icon = "calendar")
  # create Messages object
  Messages <- Messages$new()
  # move active tab to Home tab item
  move_tab(session, "Home")
  # server side of landing page module
  mod_landing_page_server("landing_page_ui_1")
  # create necessary R6 class objects (App_settings and Messages)
  App_settings <- App_settings$new()
  # store Messages obj environment in App_settings
  App_settings$env_msg <- pryr::where("Messages")
  
  # activate Input tab and set it as active tab
  observeEvent(input$new_experiment, {
    output$InputDF <- render_tabItem_ui(tabname = "InputDF",
                                        text = "Input",
                                        icon = "upload")
    new_experiment(session, App_settings)
    
  })
  
  # get value from mod_input_DF
  inputDF_out <- mod_Input_DF_server("Input_DF_ui_1", App_settings)
  
 # render Data Structure sidebar button
  observeEvent(inputDF_out(), {
    if(isTRUE(inputDF_out()) == TRUE){ #to be changed with req()
  output$DataStructure <- render_tabItem_ui(tabname = "DataStructure",
                                            text = "Data structure",
                                            icon = "filter")
    }
  })
  
  # get return values from mod_data_structure
  dataStructure_out <- mod_data_structure_server("data_structure_ui_1", App_settings)
  
  # render Your Data sidebar button and prepare for user
  observeEvent(dataStructure_out$YourDataTab(),{
    if(isTRUE(dataStructure_out$YourDataTab()) == TRUE){ #to be changed with req()
      output$YourData <- render_tabItem_ui(tabname = "YourData",
                                           text = "Your data",
                                           icon = "database")
      move_tab(session, "YourData")
      showModal(modalDialog("Your data are ready", title = "Data", easyClose = TRUE))
      # feed idList as a release trigger signal inside module to update table
      mod_your_data_server("your_data_ui_1", App_settings, idList = dataStructure_out$idList())
    }
  })
  # render Analysis sidebar button
  observeEvent(dataStructure_out$AnalysisTab(), {
    if(isTRUE(dataStructure_out$AnalysisTab()) == TRUE){ #to be changed with req()
      output$Analysis <- render_tabItem_ui(tabname = "Analysis",
                                           text = "Analysis",
                                           icon = "sliders-h")
      }
  })
  
  # get return values from mod_your_data
  yourData_out <- mod_your_data_server("your_data_ui_1", App_settings)
  
  # get return values from mod_analysis
  analysis_out <- mod_analysis_server("analysis_ui_1", App_settings)
  
  # render Plots sidebar button
  observeEvent(analysis_out$plotTab(), {
    if(isTRUE(analysis_out$plotTab()) == TRUE){ #to be changed with req()
      output$Plots <- render_tabItem_ui(tabname = "Plots",
                                        text = "Plots", 
                                        icon = "chart-pie")
    }
  }, once = TRUE)
  
  #observe when a new actogram is selected and print it
  observeEvent(analysis_out$actos(), {
  plots_out_actos <- mod_plots_server("plots_ui_1", App_settings, plot_list = analysis_out$actos())
  })
  #observe when a new DP actogram is selected and print it
  observeEvent(analysis_out$DPactos(), {
  plots_out_DPactos <- mod_plots_server("plots_ui_1", App_settings, plot_list = analysis_out$DPactos())
  })
  # observe when a new daily activity is selected and print it
  # use a for cycle to call plot module several times
  # If it works, create a functions that calls the plot module instead of using 
      # a for cycle in the server script 
  observeEvent(analysis_out$Dact(), {
    Annotate <- App_settings$env4$Annotate
    plot_list_static <- isolate(analysis_out$Dact())
    acto_choices <- Annotate$output_list_acto
    
    acto_selected <- acto_choices[acto_choices$handler  %in% plot_list_static, ]
    # browser()
    if(is.null(acto_selected) == FALSE){
      for(i in seq_len(nrow(acto_selected[, 1]))){
        title = unlist(acto_selected[i, 4])
        pos <- unlist(acto_selected[i, 2])
        module_id <- paste("box_plot_ui_", pos, sep = "")
  plots_out_Dact <- mod_plots_server(id = "plots_ui_1", env = App_settings, acto_selected = acto_selected, title = title, module_id = module_id, count = i, pos = pos)
      }}
  })
  #observe when a new periodogram is selected and print it
  observeEvent(analysis_out$periods(), {
  plots_out_periods <- mod_plots_server("plots_ui_1", App_settings, plot_list = analysis_out$periods())
  })
  
  
#### To Debug #########

  
  #### DEBUG ########################################
  
  #DEBUG in app
  observeEvent(input$debug, {
    browser()
  })  
  
  
## add function to display list of ids subset 
  
  ## declare the presence of Plot Tab as rective val
  
  # eventReactive(input$print, {mod_plots_server("plots_ui_1", App_settings, Acto_list)})
  
  
}
