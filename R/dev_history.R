#' Document functions and dependencies
#' attachment::att_to_description()
#' Check the package
#' devtools:check()

  #' 09/05/2022
#' Added tabs as modules (inputDF, your_data, data_structure, analysis, plots)
#' added observer code to populate boxes after uploading data and metadata
#' showing the list of uploaded files

  #' 10/05/2022
#' Add a function that checks if both data and metadata are present to then 
#' unlock the Data structure tab item
#' in utils_checks.R see check_uploads()
#' initialise data structure appearance hiding various sections

  #' 11/05/2022 
#' initialise data inside Raw_mouse_data object, creating myMice
#' function to convert 12/24h format separated from Raw_mouse_data::add()
#' function and added to utils_converters::convert_time()
#' !Problem! magrittr pipe operator doesn't work when used in modules

  #' 12/05/2022
#' separated R6 factory into different fct modules, one for each R6 object
#' help buttons connected 

  #' 13/05/2022
#' tried to make load data work. ATM not working because of setListMice fun


  #' 14/05/2022
#' connected Load data button with loadData function
#' created Clean_mouse_data object
#' created Annotate object
#' created Custom_tables object
#' rendered YourData and Analysis tabs
#' fixed help buttons 


  #' 16/05/2022
#' fixed wrong subsetting and assignment in setListMice, now data display
#' in yourData tab works

  #' 17/05/2022
#' started making dynamic rendering of DataStructure box 1 choices
#' activate observers to change UI in DataStructure

  #' 18/05/2022
#' activating functions in Custom_tables: Behavr_table,
#' Connect ui to functions in Custom_tables
#' 

  #' 19/05/2022
#' finish Behavr_table activation
#' activate HHActivity, dailyAct
#' mod_box_plot_fct_plotter module to generate boxes to show plots

  #' 20/05/2022 
#' connect buttons in Analysis tab to DP actogram, dailyActivity, HHActivity,
#'     and computePer

  #' 22/05/2022
#' connected computePer function

  #' 25/05/2022
#' activate Plot tab when any print button is clicked
#' insertUI() to insert a box with a plot every time a new plot is generated
#' couple the generated box with the plot that is generated: when the printing
#'     function is called there are other functions called:
#'     - generate the plot and store it in Annotate
#'     - render that plot inside the box, where the placeholder "plot_hold" is

  #' 26/05/2022
#' continue to 

  #' 31/05/2022
#' tried to fix the issue with insertUI for a long time. Problem with namespacing
#' inside the nested module, hard to solve because the placeholder struggles
#' to use ns(id) on the placeholder
#' moving to renderUI, with the caveat of generating only one box per type
#' of analysis

  #' 03/06/2022
#' move server actions belonging to modules inside module
#' access values inside modules using return()
#' App not lauching because of problem with output from Datastructure module
#' not able to return multiple values inside the same obj.

  #' 04/06/2022
#' cleaned code using modules properly until Analysis
#' now using output from modules to control action of following mod
#' use pretty buttons
#' render box_plot inside plots with random plot
#' print plots in boxes

  #' 05/06/2022
#' make downloadButton functional
#' managed to generalise plotting for actograms but for loop containing 
#'     renderUI doesn't work well when trying to plot multiple plots at the
#'     same time. It works if only one plot is selected and printed and then
#'     repeated

  #' 06/06/2022
#' change values of checkbox input to make them unique
#' expand table to include all plots
#' expand plotting to other plots -> need to debug the function

  #' 12/06/2022
#' Activate daily activity plotting

  #' 29/06/2022
#' Activate periodograms, still not displaying correctly

  #' 07/07/2022
#'  Fix plot generation
#'  Periodgrams are not generated upon request. Individual periodogram is 
#'      generated but when called inside mod_box_plot.R it appears as table 
#'      and cannot be loaded correctly by renderPlot()

  #' 08/07/2022
#'  Fixed display of periodograms
#'  Fix LD settings

  #' 11/07/2022
#'  make downloadButtons for tables available (data table, behavr Table, sum of 
#'      daily activity, period data)
#'  Create download function download_obj()
#'  donwload function not working for plots, still using old function

  #' 12/07/2022  
#'  Create custom Rhandsontable to write and download metadata

  #' 13/07/2022
#'  Fix double plotted actos
#'  fix DD plotting

  #' 14/07/2022  
#'  fixed data table not displaying data correctly
#'  changed buttons to prettyRadioButtons

  #'  16/07/2022
#'  review HHactivity function

  #'  20/07/2022
#'  add download buttons for data in mod_box_plot

#'  activate HHActivity fun plotting


  #' PRIORITIES
#' - activate exporting of parameters in tables (period power and length)
#' - activate functions to display/hide elements in Datastructure tab
#' - remake analysis tab with more user friendly flipbox
#' - update landing page
#' - update help buttons
#' - RMarkdown document
#' - Filtering for analysis of activity only at specific times of day

  #' CODING
#' - activate buttons in Analysis tab: Subsetting box and various analyses
#'     option selection. See mod_analysis for details.
#' - activate option when plotting individual plots to select for ids
#' - create analysis presets (e.g. 1 periodogram, 1 daily activity, 1 periodogram, etc..)
#' - In plots, hide tabs that do not contain any plot
#' 
  #' UI
#' 
#' - Update landing page
#' - Restructure Data structure box 1
#' - redesign plotting to be more user friendly, use of boxes to display
#'     plots. Box are generated from module call 
#'     (mod_box_plot_ui() + mod_box_plot_server()), which generates a new box each
#'     time is called.
#' 
  #' WRITING
#' 
#' - write documentation for R6 objects
#' - rewrite documentation for all @param, @field and @return 
#' - go through foo and fix when passing env to follow a std way
#'
  #' FIXINGS 
#' 
#' - All boxes display the same title
#' - fix second click on Print that makes Plot tab disappear
#' - fix buttons in DataStructure tab
#' 
#' To do: 
#'     : When plotting Daily activity, let the user decide if to plot SEM or SD
#' 
#'     : create a function to filter data based on ZT (i.e. extract only data 
#'     about the dark phase)
#'     
#'     : Below the data subsetting box, create action buttons to start new 
#'     analyses, like actogram, Daily activity, etc. Use 
#'     shinydashboardPlus set of new ui objects to make it sexy. Each time a 
#'     new Analysis is generated, either flip the card and ask for parameters
#'     settings or open a box where the user can select all the parameters and
#'     see the output.
#'     
#'     : make plot tab as nested moduled with each new horizontal tab is appended 
#'     next to the previous and contain all the analyses submitted to that plot type.   
#'     
#' Questions:
#' 
#'  - Is it possible to write automations that bring the app to a specified point?
#'      Useful for testing (bring the app to a certain state), but the experienced 
#'      user could use it to chain processes and speed up analyses across platforms. 
#'      Feature, generator of code for automations in the app sort of Macro recorder.
#'      
#' Solved questions:
#' 
#' - #' - is it possible to have an observer from inside a module?
#'     Yes, if the object to observe is correctly namespaced in the UI and the
#'     server is related to that UI.
#' - is it possible to generate multiple boxes by reusing the same module?
#'     and how to put the placeholder in the ui file? -> use insertUI() coupled
#'     with a placeholder div(id = "")
#'     