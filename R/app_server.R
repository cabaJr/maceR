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
  # show help messages 
  observeEvent(input$help_0_2, {show_help(App_settings, 1)})
  observeEvent(input$help1_1_1, {show_help(App_settings, 2)})
  observeEvent(input$help1_1_2, {show_help(App_settings, 3)})
  observeEvent(input$help2_1, {show_help(App_settings, 4)})
  observeEvent(input$help2_2, {show_help(App_settings, 5)})
  observeEvent(input$help1_2_1, {show_help(App_settings, 6)})
  observeEvent(input$help3_0, {show_help(App_settings, 7)})
  observeEvent(input$help3_4, {show_help(App_settings, 8)})
  observeEvent(input$help3_5, {show_help(App_settings, 9)})
  
  # activate Input tab and set it as active tab
  observeEvent(input$new_experiment, {
    output$InputDF <- render_tabItem_ui(tabname = "InputDF",
                                        text = "Input",
                                        icon = "upload")
    new_experiment(session, App_settings)
  })
  
  # Store data
  observeEvent(input$fileListId, { #add case when user re-uploads different files
    App_settings$setData(input$fileListId)
    output$list <- renderTable(App_settings$dataList$name)
    if(check_uploads(App_settings) == TRUE){
      output$DataStructure <- render_tabItem_ui(tabname = "DataStructure",
                                                text = "Data structure",
                                                icon = "filter")
      # initialize elements in DS
      # App_settings$initialize_DS()
      preload_data(App_settings)
    }
  })
  # Store metadata
  observeEvent(input$fileMetaId, {
    App_settings$setMeta(input$fileMetaId)
    output$metaList <- renderTable(App_settings$metadata$name)
    if(check_uploads(App_settings) == TRUE){
      output$DataStructure <- render_tabItem_ui(tabname = "DataStructure",
                                                text = "Data structure",
                                                icon = "filter")
      App_settings$initialize_DS(session)
      preload_data(App_settings)
    }
  })
  
  # import parameters into App_settings
  observeEvent(input$discardFirst, {App_settings$setDiscRow(input$discardFirst)})
  observeEvent(input$serieType, {App_settings$setTimeDisp(input$serieType)})
  observeEvent(input$rtStart, {App_settings$setExpstart(input$rtStart)})
  observeEvent(input$TPduration, {App_settings$setTimepointDur(input$TPduration)})
  
  # create Clean_mouse_data object
  observeEvent(input$go,{
    load_data(App_settings)
    # shinyjs::show("chooseM", anim = FALSE)
    output$YourData <- render_tabItem_ui(tabname = "YourData",
                                              text = "Your data",
                                              icon = "database")
    output$Analysis <- render_tabItem_ui(tabname = "Analysis",
                                         text = "Analysis",
                                         icon = "sliders-h")
    # initialize menu in yourData tab
    update_DS_ui(session, "serietype", "2")
    move_tab(session, "YourData")
    showModal(modalDialog("Your data are ready", title = "Data", easyClose = TRUE))
    # Create table with metadata
    App_settings$env2$Annotate$showMeta(App_settings$env2)
    # show metadata table in Yourdata tab
    output$metafiltered <- DT::renderDT(App_settings$env2$Annotate$metaTable)
    shinyjs::show("metafiltered", anim = FALSE)
    # Upload selector from listMice to choose what data to display in YourData
    idList <- App_settings$listMice[,2]
    updateSelectInput(session, "chooseM", choices = c("choose" = "", idList, "All" = "All"), selected = NULL)
  })
  
  # display data in YourData tab
  observeEvent(input$chooseM, {
    #import choosen id and list of ids
    id <- input$chooseM
    listMice <- App_settings$listMice
    if(is.null(App_settings$env2$Annotate) == FALSE){
      App_settings$env2$Annotate$showData(App_settings$env2, id, listMice)
      table <- App_settings$env2$Annotate$actTable
      output$showdata <- DT::renderDT(table, filter = 'top')
      shinyjs::show("dataTab", anim = FALSE)
    }else{}
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

## add function to display list of ids subset 
  
# print call of single actogram plot
  observeEvent(input$print,{
    ## get Custom table object through App_settings
    Custom_tables <- App_settings$env2$Custom_tables
    ## check if table1 is already present or calculate it
    Custom_tables$checkIf(App_settings, input$subsetPlot) #call to checker function that then calls behavrTable
    ## get annotate environment
    Annotate <- App_settings$env4$Annotate
    ## function to clear all plots
    # App_settings$clearActos()
    ## calculate LD settings
    App_settings$setLD(App_settings, input$LDcond , input$DDask, input$DDcond)
    browser()
    
    ## add function to plot graphs
    
    # if ("total" %in% input$stdActogram){
    #   shinyjs::show(id = "spin1_1", anim = FALSE)
    #   # if(Custom_tables$cacheKeys[1,2] == Annotate$cacheKeys[1,2]){
    #   #   output$actogram1 <- renderCachedPlot({plot1}, cacheKeyExpr = Custom_tables$table1)
    #   # }else{
    #   #plot actogram and store it
    #   Annotate$plot_actogram(App_settings, "total")
    #   plot1 <- Annotate$Actograms$acto1[[1]]
    #   output$actogram1 <- renderCachedPlot({plot1}, cacheKeyExpr = Custom_tables$table1)
    # }
    # shinyjs::show(id = "actogram1", anim = FALSE)
    # shinyjs::show(id = "actogram_1", anim = FALSE)
    # # }
    # if ("sex" %in% input$stdActogram){
    #   shinyjs::show(id = "spin1_2", anim = FALSE)
    #   #plot actogram and store it
    #   Annotate$plot_actogram(App_settings, "sex")
    #   plot2 <- Annotate$Actograms$acto2[[1]]
    #   output$actogram2 <- renderCachedPlot({plot2}, cacheKeyExpr = Custom_tables$table1)
    #   shinyjs::show(id = "actogram2", anim = FALSE)
    #   shinyjs::show(id = "actogram_2", anim = FALSE)
    # }
    # if ("genotype" %in% input$stdActogram){
    #   shinyjs::show(id = "spin1_3", anim = FALSE)
    #   #plot actogram and store it
    #   Annotate$plot_actogram(App_settings, "genotype")
    #   plot3 <- Annotate$Actograms$acto3[[1]]
    #   output$actogram3 <- renderCachedPlot({plot3}, cacheKeyExpr = Custom_tables$table1)
    #   shinyjs::show(id = "actogram3", anim = FALSE)
    #   shinyjs::show(id = "actogram_3", anim = FALSE)
    # }
    # if ("cabinet" %in% input$stdActogram){
    #   shinyjs::show(id = "spin1_4", anim = FALSE)
    #   #plot actogram and store it
    #   Annotate$plot_actogram(App_settings, "cabinet")
    #   plot4 <- Annotate$Actograms$acto4[[1]]
    #   output$actogram4 <-renderCachedPlot({plot4}, cacheKeyExpr = Custom_tables$table1)
    #   shinyjs::show(id = "actogram4", anim = FALSE)
    #   shinyjs::show(id = "actogram_4", anim = FALSE)
    # }
    
  })
  
#### DEBUG ########################################
  #DEBUG in app
  observeEvent(input$debug, {
    browser()
  })
}
