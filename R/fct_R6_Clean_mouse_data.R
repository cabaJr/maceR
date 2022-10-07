#' Clean_mouse_data 
#'
#' @description R6 class object fectory. Each uploaded file by the user is 
#'     combined with metadata to create a Clean_mouse_data object, that can 
#'     be stored in a list with other uploads. 
#'     NOTE to create multiple experiments make lists myCleanMiceX
#'
#' @return A Clean_mouse_data object
#'
#' @noRd

Clean_mouse_data <- R6::R6Class("Clean_mouse_data",
                            list(
#' @field id numeric mouse identifier
#' @field sex Character, Male/Female or M/F
#' @field genotype Character, mouse genetic status
#' @field cabinet numeric if multiple cabinets are present
#' @field lightOn hh:mm time format. when lights are switched on
#' @field startdate from calendar input
#' @field timepoint numeric timekeeping index
#' @field realTime time format of each datapoint
#' @field countsMinute numeric how many spins per minute
#' @field length numeric, how many datapoints
#' 
                              id = as.character(),
                              sex = as.character(),
                              genotype = as.character(),
                              cabinet = as.character(),
                              lightOn = as.character(),
                              startdate = as.character(),
                              timepoint = as.integer(),
                              realTime = as.character(),
                              countsMinute = as.integer(),
                              length = as.numeric(),
#' compile
#'
#' @param x 

                              compile = function(x){
                                self$id <- as.factor(x$id)
                                self$sex <- x$sex
                                self$genotype <- x$genotype
                                self$cabinet <- x$cabinet
                                self$lightOn <- x$lightOn
                                # self$startdate <- as.character()
                                # self$data <- data.frame()
                              },

#' addData
#'
#' @param x myMice data object
#' @param App_settings 
#' 
                              addData = function(x, App_settings){ #more comments on these fun
                                # load day, hour and minute separately
                                # browser()
                                data_day <- x$data[,1]
                                data_hour <- x$data[,2]
                                data_minute <- as.character(x$data[,3] )
                                #make all minutes values two figures
                                addZero <- which(nchar(data_minute) == 1)
                                data_minute[addZero] <- paste("0", data_minute[addZero], sep = "")
                                date <- list(data_day, data_hour, data_minute)
                                realtime <- paste(data_day, " ", data_hour, ":", data_minute, sep = "")
                                compareTime <- paste(data_hour, data_minute, sep = ":")
                                data_counts <- x$data[,4] #vapply(x$data$Counts_min, function(x){x == "NaN"}, logical(1))
                                
                                # calculate ZT18
                                lightLength <- as.double(App_settings$LDparams$light)
                                lightOn <- strptime(x$lightOn, format = "%H:%M")
                                
                                lightOn_interval <- lubridate::interval(
                                    start = strptime("00:00", format = "%H:%M"),
                                    end = lightOn)
                                diffSec <- lubridate::int_length(lightOn_interval)
                                ZT18 <- lubridate::int_shift(lightOn_interval, lubridate::hours(lightLength*1.5))
                                ZT0_ZT18 <- lubridate::interval(start = lubridate::int_start(lightOn_interval),
                                                                end = lubridate::int_end(ZT18))
                                ZT0_ZT18_diff <- lubridate::int_length(ZT0_ZT18)
                                discard_ZT18 <- c(1:(ZT0_ZT18_diff/60))
                                data_counts <- data_counts[-discard_ZT18]
                                realTimeFiltered <- realtime[-discard_ZT18]
                                compareTimeFiltered <- compareTime[-discard_ZT18]
                                # Set NaN values to 0
                                temp1 <- data_counts[1:1200]
                                discard <- vapply(temp1, function(x){x != "NaN"}, logical(1))
                                discard1 <- which(discard == FALSE)
                                # browser()
                                if(length(discard1)>0){
                                  # data_counts <- setdiff(data_counts, "NaN")
                                  data_counts <- data_counts[discard1] == 0
                                  # realTimeFiltered <- realtime[-discard1]
                                  # compareTimeFiltered <- compareTime[-discard1]
                                }else{
                                  realTimeFiltered <- realTimeFiltered
                                }
                                
                                # temp2 <- compareTimeFiltered[1:1440]
                                # lightOn <- self$lightOn
                                # discard2 <- which(temp2 == lightOn)
                                # if (discard2 > 1){
                                #   discard3 <- seq(from = 1, to = (discard2-1))
                                #   data_counts <- data_counts[-discard3]
                                #   realTimeFiltered <- realTimeFiltered[-discard3]
                                # }
                                dataLen <- length(data_counts)
                                #Clean NaN values from data
                                removeNaN <- vapply(data_counts, function(x){x == "NaN"}, logical(1))#which(data_counts == "105" )#| is.na(data_counts) == TRUE)
                                removeNaN1 <- which(removeNaN == TRUE)
                                data_counts[removeNaN1] <- (0)
                                #load inside attributes
                                self$realTime <- realTimeFiltered
                                self$timepoint <- seq(from = 0, by = App_settings$timepointDur, length.out = dataLen)
                                self$countsMinute <- data_counts
                                self$length <- dataLen
                              }


                            )
)
# )