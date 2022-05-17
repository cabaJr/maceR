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
                                #'                             
                                id = as.character(),
                                sex = as.character(),
                                genotype = as.character(),
                                cabinet = as.character(),
                                lightOn = as.character(),
                                startdate = as.character(),
                                data = as.data.frame(NULL),
                                length = as.numeric(),
                                #' initialize
                                #'
                                #' @return initializes some field with the required type
                                #' @export
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
                                #'
                                #' @return
                                #' @export
                                #'
                                add = function(datapath, metadatapath){ #function to read from csv files and insert data inside the object
                                  datapath1 <- datapath
                                  metadatapath1 <- metadatapath
                                  data <- read.csv(datapath1, skip = 4, header = FALSE, col.names = c("Day", "Hour", "Minute", "Counts_min", "Lights"), stringsAsFactors = FALSE)
                                  metadata <- read.csv(metadatapath1, header = TRUE, sep = ";")
                                  id <- read.csv(datapath1, skip = 1, nrows = 1, header = FALSE)
                                  id <- substr(id[1,1], 4, 10)
                                  filtered <- metadata[metadata$Identifier == id,]
                                  startdate <- read.csv(datapath1, skip = 2, nrows = 1, header = FALSE)
                                  day <- startdate[1,1]
                                  hour <- startdate[1,2]
                                  fulldate <- paste(day, hour, sep = "")
                                  # startdate <- as.POSIXct.Date(fulldate, format("%Y-%m-%d %H:%M:%S"))
                                  self$id <- filtered$Identifier
                                  self$sex <- filtered$Sex
                                  self$genotype <- filtered$Gene
                                  self$cabinet <- filtered$Cabinet
                                  self$lightOn <- convert_time(time = filtered$LightsOn)
                                  self$startdate <- startdate
                                  self$data <- data
                                  self$length <- {nrow(data)}
                                }
                              )
)
