#' Raw_mouse_data 
#'#' 
#' @description Raw_mouse_data R6 class object
#' @details A R6 class object to contain the uploaded files and info extracted
#'     from the metadata file. performs first round of preprocessing to store
#'     data in a convenient format to be further processed.
#'

Raw_mouse_data <- R6::R6Class("Raw_mouse_data",
                              list(
#' @field id animal's id
#' @field sex animal's sex
#' @field genotype animal's genotype
#' @field cabinet cabined in which the animal is housed
#' @field lightOn hour when lights are switched on
#' @field startdate starting date of the experiment
#' @field data raw data
#' @field length length of data table
#' @field fct_1 additional user defined factor
#' @field fct_2 additional user defined factor
#'                             
                                id = as.character(),
                                sex = as.character(),
                                genotype = as.character(),
                                cabinet = as.character(),
                                lightOn = as.character(),
                                startdate = as.character(),
                                data = as.data.frame(NULL),
                                length = as.numeric(),
                                fct_1 = as.character(),
                                fct_2 = as.character(),
#' initialize
#'
#' @return initializes some field with the required type
#'
                                initialize = function(){
                                  self$id <- as.character()
                                  self$sex <- as.character()
                                  self$genotype <- as.character()
                                  self$cabinet <- as.character()
                                  self$lightOn <- as.character()
                                  self$startdate <- as.character()
                                  self$data <- data.frame()
                                },
#' add
#'
#' @param datapath path where to find the uploaded data filed
#' @param metadatapath path where to find the metadata file
#' @param env App_settings environment
#' @param skipRows numeric, number of initial rows to skip in the imported files
#'
                                add = function(datapath, metadatapath, env, skipRows = 4){ #function to read from csv files and insert data inside the object
                                  ## read from the specified datapath (from input button)
                                  datapath1 <- datapath
                                  ## read from the specified metadatapath (from input button)
                                  metadatapath1 <- metadatapath
                                  #add comment on this: conditional option to skip rows
                                  if(is.null(env$discardRow) == FALSE){
                                    skipRows <- env$discardRow
                                  }
                                  # read data file and store it in data variable
                                  data <- read.csv(datapath1, skip = skipRows, header = FALSE, col.names = c("Day", "Hour", "Minute", "Counts_min", "Lights"), stringsAsFactors = FALSE)
                                  
                      # handle error where file is not a csv with sep = ";"
                                  
                                  # read metadata file and store it in metadata variable
                                  metadata <- read.csv(metadatapath1, header = TRUE, sep = ";")
                                  #get animal id from second row
                                  id <- read.csv(datapath1, skip = 1, nrows = 1, header = FALSE)
                                  # select only relevant portion of the id
                                  id_sel <- sapply(strsplit(id[1,1], "_"), function(x) x[(length(x)-1)])
                                  # id_sel <- substr(id[1,1], 4, 10)
                                  #filter metadata table to extract values only to the appropriate id
                                  filtered <- metadata[metadata$Identifier == id_sel,]
                                  # get startdate of experiment from data file
                                  startdate <- read.csv(datapath1, skip = 2, nrows = 1, header = FALSE)
                                  day <- startdate[1,1]
                                  hour <- startdate[1,2]
                                  fulldate <- paste(day, hour, sep = "")
                                  # startdate <- as.POSIXct.Date(fulldate, format("%Y-%m-%d %H:%M:%S"))
                                  addFct <- (ncol(metadata) - 5)
                                    switch(addFct,
                                      "0" = {
                                        self$fct_1 <- 0
                                        self$fct_2 <- 0
                                        },
                                      "1" = {
                                        self$fct_1 <- filtered[,6]
                                        self$fct_2 <- 0
                                        },
                                      "2" = {
                                        self$fct_1 <- filtered[,6]
                                        self$fct_2 <- filtered[,7]
                                        }
                                    )
                                  # fill data with all fields contained in metadata table from user
                                  self$id <- filtered$Identifier
                                  self$sex <- filtered$Sex
                                  self$genotype <- filtered$Genotype
                                  self$cabinet <- filtered$Cabinet
                                  self$lightOn <- convert_time(time = filtered$LightsOn)
                                  self$startdate <- startdate
                                  self$data <- data
                                  self$length <- {nrow(data)}
                                }
                              )
)
