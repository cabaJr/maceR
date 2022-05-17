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
  
#' hide_dataStr_subset_ui
#'
#' @param session 
#'
#' @return
#' @export
#'
  hide_dataStr_subset_ui <- function(session){
      shinyjs::hide(id = "idSubsetList", anim = FALSE)   #hide plot subset settings
      shinyjs::hide(id = "sexSubsetList", anim = FALSE)
      shinyjs::hide(id = "geneSubsetList", anim = FALSE)
      shinyjs::hide(id = "cabSubsetList", anim = FALSE)
      # shinyjs::hide(id = "text11", anim = FALSE)
      shinyjs::hide(id = "metaUniqueO", anim = FALSE)
      shinyjs::hide(id = "timeSubset", anim = FALSE)
  }
  
#' show_dataStr_subset_ui
#'
#' @param session 
#'
#' @return
#' @export
#'
  show_dataStr_subset_ui <- function(session){
    shinyjs::show(id = "idSubsetList", anim = FALSE)   #hide plot subset settings
      shinyjs::show(id = "sexSubsetList", anim = FALSE)
      shinyjs::show(id = "geneSubsetList", anim = FALSE)
      shinyjs::show(id = "cabSubsetList", anim = FALSE)
      shinyjs::show(id = "text11", anim = FALSE)
      shinyjs::show(id = "metaUniqueO", anim = FALSE)
      shinyjs::show(id = "timeSubset", anim = FALSE)
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
  update_DS_ui <- function(session, element, case){
    switch(element, 
           "serietype" = {
             switch(case, 
                    "1" = {
                      shinyjs::show(id = "rtStart", anim = FALSE)
                      shinyjs::show(id = "rtStartHour", anim = FALSE)
                      shinyjs::show(id = "DFsubsetRT", anim = FALSE)
                      shinyjs::hide(id = "DFsubsetTP", anim = FALSE)
                      shinyjs::hide(id = "tpDuration", anim = FALSE)
                    }, #Timepoints
                    "2" = {
                      shinyjs::show(id = "tpDuration", anim = FALSE)
                      shinyjs::show(id = "DFsubsetTP", anim = FALSE)
                      shinyjs::hide(id = "DFsubsetRT", anim = FALSE)
                      shinyjs::hide(id = "rtStart", anim = FALSE)
                      shinyjs::hide(id = "rtStartHour", anim = FALSE)
                    }  #Realtime
             )
           "subsetting" = {
             switch(case, 
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
             )
           }
      })
    
  }
#' @noRd
