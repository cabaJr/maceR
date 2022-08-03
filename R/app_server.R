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
  # create necessary R6 class objects (App_settings and Messages)
  App_settings <- App_settings$new()
  # store Messages obj environment in App_settings
  App_settings$env_msg <- pryr::where("Messages")
  
  # move active tab to Home tab item
  move_tab(session, "Home")
  # server side of landing page module
  mod_landing_page_server("landing_page_ui_1", env = App_settings)
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
  # call the lad_plots fun to activate the module for each requested plot  
  load_plots(env = App_settings, plot_list = analysis_out$actos(), session = session)
  })
  
  #observe when a new DP actogram is selected and print it
  observeEvent(analysis_out$DPactos(), {
  # call the lad_plots fun to activate the module for each requested plot  
  load_plots(env = App_settings, plot_list = analysis_out$DPactos(), session = session)
  })
  
  # observe when a new daily activity is selected and print it
  observeEvent(analysis_out$Dact(), {
  # call the lad_plots fun to activate the module for each requested plot  
  load_plots(env = App_settings, plot_list = analysis_out$Dact(), session = session)
  })
  
  #observe when a new periodogram is selected and print it
  observeEvent(analysis_out$periods(), {
  # call the lad_plots fun to activate the module for each requested plot  
  load_plots(env = App_settings, plot_list = analysis_out$periods(), session = session)
  })
  
  #observe when a new periodogram is selected and print it
  observeEvent(analysis_out$avgDay(), {
    # call the lad_plots fun to activate the module for each requested plot  
    load_plots(env = App_settings, plot_list = analysis_out$avgDay(), session = session)
  })
  
  
#### To Debug #########

  
  #### DEBUG ########################################
  
  #DEBUG in app
  # observeEvent(input$debug, {
  #   browser()
  # })  
  
  
## add function to display list of ids subset 
  
  ## declare the presence of Plot Tab as rective val
  
  # eventReactive(input$print, {mod_plots_server("plots_ui_1", App_settings, Acto_list)})
  
  
}
