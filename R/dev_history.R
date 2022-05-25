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
#' activate HHActivity, dailyAct, computePer
#' mod_box_plot_fct_plotter module to generate boxes to show plots

#' 20/05/2022 
#' connect buttons in Analysis tab to DP actogram, dailyActivity, HHActivity,
#'     and computePer

#' 22/05/2022
#' connected computePer function

#' 25/05/2022
#' activate Plot tab when any print button is clicked
#' insertUI() to insert a box with a plot every time a new plot is generated
#' 

#'
#' CODING
#' 
#' - activate more fun in Custom_tables and Annotate
#' - activate buttons in Analysis tab: Subsetting box and various analyses
#'     option selection. See mod_analysis for details.
#' - activate plotting and storing functions
#' - activate download buttons
#' - create analysis presets (e.g. 1 periodogram, 1 daily activity, 1 periodogram, etc..)
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
#' - rewrite documentation for all @param and @field
#' - go through foo and fix when passing env to follow a std way
#'
#' FIXINGS 
#' 
#' - fix buttons in DataStructure tab
#' - fix problem with radiobuttons not showing the dot
#' 
#' To do: Below the data subsetting box, create action buttons to start new 
#'     analyses, like actogram, Daily activity, etc. Use 
#'     shinydashboardPlus set of new ui objects to make it sexy. Each time a 
#'     new Analysis is generated, either flip the card and ask for parameters
#'     settings or open a box where the user can select all the parameters and
#'     see the output.
#'     
#' Questions:
#' 
#' - is it possible to have an observer from inside a module?
#' - is it possible to generate multiple boxes by reusing the same module?
#'     and how to put the placeholder in the ui file?