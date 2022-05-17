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


#'
#' CODING
#' 
#' - activate observers to change UI in DataStructure
#' - activate more fun in Custom_tables and Annotate
#' - activate plotting and storing functions
#' - restructure Data structure box 1
#' - activate download buttons
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
#' 
