#' @title render_tabItem_UI
#' @description function to dynamically render sidebar menu as the user 
#'     progresses through the experiment 
#' @details Call to the function render_tabItem_UI() requires the specification
#'     of params tabname, text, and icon to render a menuItem in the sidebar
#'     of the dahboardSidebar. Calling of the function can be associated to
#'     a call to movetab() to make it the active tab.
#' @param tabname a character string containing the name of the tabItem to
#'     render. It must match one of the menuItemOutput
#' @param text a character string containing the text to be displayed in the 
#'     sidebar
#' @param icon a character string containing the icon to appear on the left  
#' @return A new menuItem is generated in the app sidebar.
#' 
  render_tabItem_ui <- function(tabname, text, icon){
    
    shinydashboard::renderMenu({
      shinydashboard::menuItem(tabName = tabname, text = text, icon = icon(icon))
    })
    
  }
  
 #' @title move_tab
 #' @description activate a specific tabItem.
 #' @details Calling the move_tab function allows to dynamically change the 
 #'     active tab to a desired one. It is used to seamlessly progress through
 #'     the app when the user finishes the tasks in one section of the app.
 #' @param session the current R session
 #' @param tabname the tabItem to be activated
 #' @return a different tabItem is activated
 #'
  move_tab <- function(session, tabname){
    shinydashboard::updateTabItems(session, inputId = "mainmenu", selected = tabname)
  }
  
  #' @title new_experiment
  #' @description set up the app for a new experiment
  #' @details change the app appearance from the landing page to the 
  #'     configuration used to start a new experiment. Restores the dashboard
  #'     header, enables the sidebar and moves the active tab to Input.
  #' @param session the current R session
  #' @param env to access App_settings
  #' @return the app is now ready to receive the files for a new experiment
  #' 
  new_experiment <- function(session, env){
    ## inside here call a function that:
    ## restores the dashboard header (2),
    ## enables the sidebar (3) 
    ## and moves the active tab to inputDF (4).
    
    # call to shinyjs providing a null argument to hidehead
    shinyjs::js$hidehead('') #2
    move_tab(session, tabname = "InputDF") #4
    
  }
  
#' initialize_datastr_ui
#'
#' @return
#' @export
#'
  initialize_datastr_ui <- function(...){
      shinyjs::hide(id = "rtStart", anim = FALSE)
        shinyjs::hide(id = "rtStarthour", anim = FALSE)
        shinyjs::hide(id = "TPduration", anim = FALSE)
        shinyjs::hide(id = "timeFrame1", anim = FALSE)
        shinyjs::hide(id = "timeFrame2", anim = FALSE)
        shinyjs::hide(id = "RTanalysis_starttime", anim = FALSE)
        shinyjs::hide(id = "RTanalysis_endtime", anim = FALSE)
        shinyjs::hide(id = "DFsubsetRT2", anim = FALSE)
        shinyjs::hide(id = "timeFrame3", anim = FALSE)
        shinyjs::hide(id = "timeFrame4", anim = FALSE)
        shinyjs::hide(id = "RTanalysis_starttime2", anim = FALSE)
        shinyjs::hide(id = "RTanalysis_endtime2", anim = FALSE)
        shinyjs::hide(id = "TPanalysis_starttime2", anim = FALSE)
        shinyjs::hide(id = "TPanalysis_endtime2", anim = FALSE)
        shinyjs::hide(id = "TPduration", anim = FALSE)
        shinyjs::hide(id = "DFsubsetRT", anim = FALSE)
        shinyjs::hide(id = "TPanalysis_starttime", anim = FALSE)
        shinyjs::hide(id = "TPanalysis_endtime", anim = FALSE)
        # shinyjs::hide(id = "DFsubsetTP", anim = FALSE)
  }

  #' showall_datastr_ui
  #'
  #' @return
  #' @export
  #'
  showall_datastr_ui <- function(...){
    shinyjs::show(id = "rtStart", anim = FALSE)
    shinyjs::show(id = "rtStarthour", anim = FALSE)
    shinyjs::show(id = "TPduration", anim = FALSE)
    shinyjs::show(id = "timeFrame1", anim = FALSE)
    shinyjs::show(id = "timeFrame2", anim = FALSE)
    shinyjs::show(id = "RTanalysis_starttime", anim = FALSE)
    shinyjs::show(id = "RTanalysis_endtime", anim = FALSE)
    shinyjs::show(id = "DFsubsetRT2", anim = FALSE)
    shinyjs::show(id = "timeFrame3", anim = FALSE)
    shinyjs::show(id = "timeFrame4", anim = FALSE)
    shinyjs::show(id = "RTanalysis_starttime2", anim = FALSE)
    shinyjs::show(id = "RTanalysis_endtime2", anim = FALSE)
    shinyjs::show(id = "TPanalysis_starttime2", anim = FALSE)
    shinyjs::show(id = "TPanalysis_endtime2", anim = FALSE)
    shinyjs::show(id = "TPduration", anim = FALSE)
    shinyjs::show(id = "DFsubsetRT", anim = FALSE)
    shinyjs::show(id = "TPanalysis_starttime", anim = FALSE)
    shinyjs::show(id = "TPanalysis_endtime", anim = FALSE)
    # shinyjs::show(id = "DFsubsetTP", anim = FALSE)
  }
  
#' update_DS_ui
#'
#' @param session env 
#' @param element subsetting or serietype
#' @param case which case to apply
#' @description function to dynalically update the possible choices in the
#'     Data Structure tab box 1
#' @return
#' @export
#'
  update_DS_ui <- function(case, ...){
    # browser()
             switch(case,
                    "6" = {
                      shinyjs::show(id = "rtStart", anim = FALSE)
                      shinyjs::show(id = "rtStartHour", anim = FALSE)
                      shinyjs::show(id = "DFsubsetRT", anim = FALSE)
                      shinyjs::hide(id = "DFsubsetTP", anim = FALSE)
                      shinyjs::hide(id = "tpDuration", anim = FALSE)
                    }, #Timepoints
                    "7" = {
                      shinyjs::show(id = "tpDuration", anim = FALSE)
                      shinyjs::show(id = "DFsubsetTP", anim = FALSE)
                      shinyjs::hide(id = "DFsubsetRT", anim = FALSE)
                      shinyjs::hide(id = "rtStart", anim = FALSE)
                      shinyjs::hide(id = "rtStartHour", anim = FALSE)
                    },  #Realtime
                    "1" = {
                      shinyjs::show(id = "timeframe1", anim = FALSE)
                      shinyjs::show(id = "timeframe2", anim = FALSE)
                      shinyjs::show(id = "RTanalysis_starttime", anim = FALSE)
                      shinyjs::show(id = "RTanalysis_endtime", anim = FALSE)
                      shinyjs::show(id = "DFsubsetRT2", anim = FALSE)
                      shinyjs::hide(id = "TPanalysis_starttime", anim = FALSE)
                      shinyjs::hide(id = "TPanalysis_starttime", anim = FALSE)
                      },
                    "1_hide" = {
                      shinyjs::hide(id = "timeframe1", anim = FALSE)
                      shinyjs::hide(id = "timeframe2", anim = FALSE)
                      shinyjs::hide(id = "RTanalysis_starttime", anim = FALSE)
                      shinyjs::hide(id = "RTanalysis_endtime", anim = FALSE)
                    }, #show realtime subsetting 1
                    "2" = {
                      shinyjs::show(id = "TPanalysis_starttime", anim = FALSE)
                      shinyjs::show(id = "TPanalysis_starttime", anim = FALSE)
                      shinyjs::show(id = "DFsubsetRT2", anim = FALSE)
                      shinyjs::hide(id = "timeframe1", anim = FALSE)
                      shinyjs::hide(id = "timeframe2", anim = FALSE)
                      shinyjs::hide(id = "RTanalysis_starttime", anim = FALSE)
                      shinyjs::hide(id = "RTanalysis_endtime", anim = FALSE)
                    }, #show timepoints subsetting 1
                    "2_hide" = {
                      shinyjs::hide(id = "TPanalysis_starttime", anim = FALSE)
                      shinyjs::hide(id = "TPanalysis_starttime", anim = FALSE)
                      shinyjs::hide(id = "DFsubsetRT2", anim = FALSE)
                    }, #show realtime subsetting 1
                    "3" = {
                      shinyjs::show(id = "timeframe3", anim = FALSE)
                      shinyjs::show(id = "timeframe4", anim = FALSE)
                      shinyjs::show(id = "RTanalysis_starttime2", anim = FALSE)
                      shinyjs::show(id = "RTanalysis_endtime2", anim = FALSE)
                      shinyjs::hide(id = "TPanalysis_starttime2", anim = FALSE)
                      shinyjs::hide(id = "TPanalysis_starttime2", anim = FALSE)
                    }, #show realtime subsetting 2
                    "4" = {
                      shinyjs::show(id = "TPanalysis_starttime2", anim = FALSE)
                      shinyjs::show(id = "TPanalysis_starttime2", anim = FALSE)
                      shinyjs::hide(id = "timeframe3", anim = FALSE)
                      shinyjs::hide(id = "timeframe4", anim = FALSE)
                      shinyjs::hide(id = "RTanalysis_starttime2", anim = FALSE)
                      shinyjs::hide(id = "RTanalysis_endtime2", anim = FALSE)
                    }, #show timepoints subsetting 2
                    "5" = {
                      shinyjs::hide(id = "TPanalysis_starttime2", anim = FALSE)
                      shinyjs::hide(id = "TPanalysis_starttime2", anim = FALSE)
                      shinyjs::hide(id = "timeframe3", anim = FALSE)
                      shinyjs::hide(id = "timeframe4", anim = FALSE)
                      shinyjs::hide(id = "RTanalysis_starttime2", anim = FALSE)
                      shinyjs::hide(id = "RTanalysis_endtime2", anim = FALSE)
                      shinyjs::hide(id = "DFsubsetRT2", anim = FALSE)
                    }  #hide all second window subsetting options
      ) # end of outer switch
  } # update_DS_ui end
  
#' #' insert_box
#' #' @description Generates a new box after the placeholder
#' #' @param placeholder what is the placeholder to use
#' #'
#' #' @return a box 
#' #' @export
#' #'
#'   insert_box <- function(placeholder, title, plotObj, session){
#'     browser()
#'     # plotObj <- plotObj
#'     shiny::insertUI(
#'       selector = placeholder,
#'       where = "afterEnd",
#'       ui = mod_box_plot_ui(NS("box_plot_ui_1"), title)
#'     )
#'     mod_box_plot_server("box_plot_ui_1", plotObj)
#'     # output$plot_holder <- shiny::renderUI({
#'     #                           ns <- session$ns
#'     #                           shiny::renderPlot(plotObj)
#'     #                         })
#'   }
  
#' #' displayPlot
#' #'
#' #'@description aa
#' #' @param env a
#' #' @param plot b
#' #' @param placeholder v
#' #' @param ... d
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples f
#'   displayPlot <- function(env, plotUP, ...){
#'     plot <- env$env4$selected
#'     shiny::insertUI(
#'       selector =  "plot_holder",
#'       where = "afterEnd",
#'       ui = 
#'     )
#'   }
  
#' plot_actogram_sup
#'
#' @param env aa
#' @param plot_type ss
#'
#' @return
#' @export
#'
plot_actogram_sup <- function(env, plot_type){
  Annotate <- env$env3$Annotate
  # browser()
  if ("total" %in% plot_type){
    # shinyjs::show(id = "spin1_1", anim = FALSE)
    # if(Custom_tables$cacheKeys[1,2] == Annotate$cacheKeys[1,2]){
    #   output$actogram1 <- renderCachedPlot({plot1}, cacheKeyExpr = Custom_tables$locomotor_act[[1]])
    # }else{
    #plot actogram and store it
    Annotate$plot_actogram(env, "total")
    plot1 <- Annotate$Actograms$acto1[[1]]
    # renderUI
    # output$actogram1 <- renderCachedPlot({plot1}, cacheKeyExpr = Custom_tables$locomotor_act[[1]])
  }
  # shinyjs::show(id = "actogram1", anim = FALSE)
  # shinyjs::show(id = "actogram_1", anim = FALSE)
  # }
  if ("sex" %in% plot_type){
    # shinyjs::show(id = "spin1_2", anim = FALSE)
    #plot actogram and store it
    Annotate$plot_actogram(env, "sex")
    plot2 <- Annotate$Actograms$acto2[[1]]
    # output$actogram2 <- renderCachedPlot({plot2}, cacheKeyExpr = Custom_tables$locomotor_act[[1]])
    # shinyjs::show(id = "actogram2", anim = FALSE)
    # shinyjs::show(id = "actogram_2", anim = FALSE)
  }
  if ("genotype" %in% plot_type){
    # shinyjs::show(id = "spin1_3", anim = FALSE)
    #plot actogram and store it
    Annotate$plot_actogram(env, "genotype")
    plot3 <- Annotate$Actograms$acto3[[1]]
    # output$actogram3 <- renderCachedPlot({plot3}, cacheKeyExpr = Custom_tables$locomotor_act[[1]])
    # shinyjs::show(id = "actogram3", anim = FALSE)
    # shinyjs::show(id = "actogram_3", anim = FALSE)
  }
  if ("cabinet" %in% plot_type){
    # shinyjs::show(id = "spin1_4", anim = FALSE)
    #plot actogram and store it
    Annotate$plot_actogram(env, "cabinet")
    plot4 <- Annotate$Actograms$acto4[[1]]
    # output$actogram4 <-renderCachedPlot({plot4}, cacheKeyExpr = Custom_tables$locomotor_act[[1]])
    # shinyjs::show(id = "actogram4", anim = FALSE)
    # shinyjs::show(id = "actogram_4", anim = FALSE)
  }
}

showSubsetting <- function(...){
    shinyjs::show(id = "idSubsetList", anim = FALSE)   #hide plot subset settings
    shinyjs::show(id = "sexSubsetList", anim = FALSE)
    shinyjs::show(id = "geneSubsetList", anim = FALSE)
    shinyjs::show(id = "cabSubsetList", anim = FALSE)
    shinyjs::show(id = "text11", anim = FALSE)
    shinyjs::show(id = "metaUniqueO", anim = FALSE)
    shinyjs::show(id = "timeSubset", anim = FALSE)
}

clearSubsetting <- function(...){
  shinyjs::hide(id = "idSubsetList", anim = FALSE)   #hide plot subset settings
    shinyjs::hide(id = "sexSubsetList", anim = FALSE)
    shinyjs::hide(id = "geneSubsetList", anim = FALSE)
    shinyjs::hide(id = "cabSubsetList", anim = FALSE)
    shinyjs::hide(id = "text11", anim = FALSE)
    shinyjs::hide(id = "metaUniqueO", anim = FALSE)
    shinyjs::hide(id = "timeSubset", anim = FALSE)
}
  
#' @noRd
